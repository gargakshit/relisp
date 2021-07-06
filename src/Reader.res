open ReLisp

@send
external replaceWithFun: (string, Js.Re.t, (string, string) => string) => string = "replace"

let tokenize = (input: string) => {
  let regex = %re("/[\s,]*(~@|[\[\]{}()'`~^@]|\"(?:\\\.|[^\\\\\"])*\"?|;.*|[^\s\[\]{}('\"`,;)]*)/g")

  let rec recursiveTokenizer = tokens => {
    switch regex->Js.Re.exec_(input) {
    | None => tokens
    | Some(matches) => {
        let captures = matches->Js.Re.captures
        switch captures[1]->Js.Nullable.toOption {
        | None => tokens
        | Some("") => tokens
        | Some(match) =>
          switch captures[0]->Js.Nullable.toOption {
          | None => tokens
          | Some(";") => recursiveTokenizer(tokens)
          | _ => {
              let _ = tokens->Js.Array2.push(match)
              recursiveTokenizer(tokens)
            }
          }
        }
      }
    }
  }

  recursiveTokenizer([])
}

let rec readForm = tokens => {
  let token = tokens[0]

  let readSymbol = name => {
    let _ = Js.Array2.shift(tokens)

    let sym = ReLispSymbol(name, None)
    let target = readForm(tokens)

    ReLispList([sym, target], None)
  }

  switch token {
  | "(" => ReLispList([], None) // TODO
  | "[" => ReLispVector([], None) // TODO
  // | "{" => ReLispHashMap({}, None) TODO
  | "'" => readSymbol("quote")
  | "`" => readSymbol("quasiquote")
  | "~" => readSymbol("unquote")
  | "~@" => readSymbol("splice-quote")
  | "^" => {
      let _ = Js.Array2.shift(tokens)

      let sym = ReLispSymbol("with-meta", None)
      let target = readForm(tokens)

      ReLispList([sym, readForm(tokens), target], None)
    }
  | _ => ReLispAtom(readAtom(tokens), None)
  }
}
and readAtom = tokens => {
  let token = switch Js.Array2.shift(tokens) {
  | None => "1" // Should not happen I guess
  | Some(t) => t
  }

  if %re("/^-?[0-9]+$/")->Js.Re.test_(token) {
    let num = switch Belt.Int.fromString(token) {
    | None => 0 // Should not happen due to the regex
    | Some(n) => n
    }->Belt.Float.fromInt

    ReLispNumber(num, None)
  } else if %re("/^-?[0-9]\.[0-9]+$/")->Js.Re.test_(token) {
    let num = switch Belt.Float.fromString(token) {
    | None => 0.
    | Some(n) => n
    }

    ReLispNumber(num, None)
  } else if %re("/^\"(?:\\\.|[^\\\\\"])*\"$/")->Js.Re.test_(token) {
    let str =
      token
      ->Js.String2.slice(~from=1, ~to_=token->Js.String2.length - 1)
      ->replaceWithFun(%re("/\\\\(.)/g"), (_, c) =>
        switch c {
        | "n" => "\n"
        | c => c
        }
      )

    ReLispString(str, None)
  } else if token->Js.String2.charAt(0) == "\"" {
    ReLispError("Expected \", got EOF", None)
  } else if token->Js.String2.charAt(0) == ":" {
    ReLispKeyword(token->Js.String2.substr(~from=1), None)
  } else {
    switch token {
    | "nil" => ReLispNil(None)
    | "true" => ReLispBoolean(true, None)
    | "false" => ReLispBoolean(false, None)
    | _ => ReLispSymbol(token, None)
    }
  }
}
