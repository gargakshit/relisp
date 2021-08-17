let read = input => ReLisp__Reader.readStr(input)

let initEnv = () => {
  let env = ReLisp__Types.Env.new(None, ReLisp__Stdlib.stdlib)
  let _ = env->ReLisp__Types.Env.set(
    "eval",
    ReLisp__Types.Function.fromBootstrap(elems => {
      let len = Belt.Array.length(elems)

      switch len {
      | 1 =>
        switch ReLisp__Eval.evalReLisp(elems[0], env) {
        | Error(e) => ReLisp__Types.ReLispError(e, None)
        | Ok(e) => e
        }
      | _ => ReLispError(`Expected 1 argument, got ${len->Belt.Int.toString}`, None)
      }
    }),
  )

  env
}

let eval = (~env=initEnv(), ast) => ast->ReLisp__Eval.evalReLisp(env)

let print = exp => ReLisp__Printer.printToString(exp)

let rep = (~env=initEnv(), input) =>
  switch read(input) {
  | Error(e) => Error(e)
  | Ok(ast) =>
    switch eval(ast, ~env) {
    | Error(e) => Error(e)
    | Ok(result) => Ok(print(result))
    }
  }

let rec repl = (~env=initEnv(), input, results) =>
  switch Js.Array2.shift(input) {
  | None => Ok(results)
  | Some(inp) =>
    switch read(inp) {
    | Error(e) => Error(e)
    | Ok(ast) =>
      switch eval(ast) {
      | Error(e) => Error(e)
      | Ok(result) => input->repl(Belt.Array.concat(results, [print(result)]), ~env)
      }
    }
  }
