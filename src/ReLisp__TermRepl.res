open Js
open ReLisp__Reader
open ReLisp__REPL
open ReLisp__Types

type evalCallback =
  | Ok(ReLisp__Types.t)
  | Error(string)
  | Exception(string)

let initialProgram = [
  `(def! load-file (fn* (f) (eval (read-string (str "(do " (slurp f) "nil)")))))`,
  `(load-file "stdlib.relisp")`,
]

let primitives = [
  "def!",
  "fn*",
  "defmacro!",
  "if",
  "let*",
  "do",
  "macroexpand",
  "quote",
  "quasiquoteexpand",
  "quasiquote",
]

let start = () => {
  let ansiColors = AnsiColors.defaultExport
  let cyan = AnsiColors.cyan(ansiColors)
  let bold = AnsiColors.bold(ansiColors)
  let greenBright = AnsiColors.greenBright(ansiColors)
  let redBright = AnsiColors.redBright(ansiColors)
  let bgRed = AnsiColors.bgRed(ansiColors)
  let whiteBright = AnsiColors.whiteBright(ansiColors)
  let gray = AnsiColors.gray(ansiColors)

  let prompt = "relisp> "->cyan->bold

  let env = initEnv()
  let _ = env->Env.set(
    "ansi",
    Function.fromBootstrap(elems => {
      let len = Belt.Array.length(elems)

      switch len {
      | 2 =>
        switch elems->Array2.unsafe_get(1) {
        | ReLispString(str, _) =>
          switch elems->Array2.unsafe_get(0) {
          | ReLispKeyword("cyan", None) => ReLispString(cyan(str), None)
          | ReLispKeyword("bold", None) => ReLispString(bold(str), None)
          | ReLispKeyword("green-bright", None) => ReLispString(greenBright(str), None)
          | ReLispKeyword("red-bright", None) => ReLispString(redBright(str), None)
          | ReLispKeyword("bg-red", None) => ReLispString(bgRed(str), None)
          | ReLispKeyword("white-bright", None) => ReLispString(whiteBright(str), None)
          | ReLispKeyword("gray", None) => ReLispString(gray(str), None)
          | e => ReLispError("Unknown ANSI type " ++ print(e), None)
          }
        | _ => ReLispError("String expected", None)
        }
      | _ => ReLispError(`Expected 2 arguments, got ${len->Belt.Int.toString}`, None)
      }
    }),
  )
  // Ignore the stdlib results for now
  let _ = repl(~env, initialProgram, [])

  NodeRepl.start({
    prompt: prompt,
    writer: Some(
      obj =>
        switch obj {
        | Error(e) => redBright(e)
        | Exception(e) => "Host Exception"->whiteBright->bgRed ++ redBright(" " ++ e)->bold
        | Ok(res) =>
          switch res {
          | ReLispNil(_) => gray("nil")
          | ReLispError(e, _) => redBright(e)
          | other => other->ReLisp__Printer.printToString->greenBright
          }
        },
    ),
    completer: Some(
      line => {
        let tokens = tokenize(line)
        let tokensLength = Array2.length(tokens)
        let keys = Some(env)->Env.allKeys(primitives)

        if tokensLength == 0 {
          (keys, line)
        } else {
          let token = tokens->Array2.unsafe_get(tokensLength - 1)->String2.trim
          let hits = keys->Array2.filter(key => key->String2.startsWith(token))

          (
            if hits->Array2.length == 0 {
              keys
            } else {
              hits
            },
            token,
          )
        }
      },
    ),
    eval: (input, _, _, callback) => {
      try {
        callback(
          None,
          switch read(input) {
          | Error(e) => Error(e)
          | Ok(ast) =>
            switch eval(~env, ast) {
            | Error(e) => Error(e)
            | Ok(res) => Ok(res)
            }
          },
        )
      } catch {
      | e =>
        callback(
          None,
          Exception(
            switch e->Exn.asJsExn {
            | Some(e) =>
              switch e->Exn.message {
              | Some(e) => e
              | None => "Uncaught Exception"
              }
            | None => "Uncaught Exception"
            },
          ),
        )
      }
      ()
    },
  })
}
