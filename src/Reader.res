open ReLisp

@send
external replaceWithFun: (string, Js.Re.t, (string, string) => string) => string = "replace"

let tokenize = input => {
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

    switch readForm(tokens) {
    | Error(e) => Error(e)
    | Ok(target) => Ok(ReLispList([sym, target], None))
    }
  }

  switch token {
  | ")" => Error("Unexpected )")
  | "(" =>
    switch readParen(tokens, "(", ")") {
    | Error(e) => Error(e)
    | Ok(list) => Ok(ReLispList(list, None))
    }
  | "]" => Error("Unexpected ]")
  | "[" =>
    switch readParen(tokens, "[", "]") {
    | Error(e) => Error(e)
    | Ok(list) => Ok(ReLispVector(list, None))
    }
  | "}" => Error("Unexpected }")
  | "{" =>
    switch readParen(tokens, "{", "}") {
    | Error(e) => Error(e)
    | Ok(list) => HashMap.new(list)
    }
  | "'" => readSymbol("quote")
  | "`" => readSymbol("quasiquote")
  | "~" => readSymbol("unquote")
  | "~@" => readSymbol("splice-quote")
  | "^" => {
      let _ = Js.Array2.shift(tokens)

      let sym = ReLispSymbol("with-meta", None)

      switch readForm(tokens) {
      | Error(e) => Error(e)
      | Ok(target) =>
        switch readForm(tokens) {
        | Error(e) => Error(e)
        | Ok(target2) => Ok(ReLispList([sym, target2, target], None))
        }
      }
    }
  | _ =>
    switch readAtom(tokens) {
    | Error(e) => Error(e)
    | Ok(atom) => Ok(atom)
    }
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

    Ok(ReLispNumber(num, None))
  } else if %re("/^-?[0-9]\.[0-9]+$/")->Js.Re.test_(token) {
    let num = switch Belt.Float.fromString(token) {
    | None => 0.
    | Some(n) => n
    }

    Ok(ReLispNumber(num, None))
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

    Ok(ReLispString(str, None))
  } else if token->Js.String2.charAt(0) == "\"" {
    Error("Expected \", got EOF")
  } else if token->Js.String2.charAt(0) == ":" {
    Ok(ReLispKeyword(token->Js.String2.substr(~from=1), None))
  } else {
    Ok(
      switch token {
      | "nil" => ReLispNil(None)
      | "true" => ReLispBoolean(true, None)
      | "false" => ReLispBoolean(false, None)
      | _ => ReLispSymbol(token, None)
      },
    )
  }
}
and readParen = (tokens, open_, close) => {
  let token = switch Js.Array2.shift(tokens) {
  | None => open_ // Should not happen I guess
  | Some(t) => t
  }

  if token != open_ {
    Error(`Unexpected token ${token}, expected ${open_}`)
  } else {
    let rec recursiveReader = arr =>
      if tokens[0] == close {
        Ok(arr)
      } else {
        switch readForm(tokens) {
        | Error(e) => Error(e)
        | Ok(target) => {
            let _ = arr->Js.Array2.push(target)
            recursiveReader(arr)
          }
        }
      }

    let list = recursiveReader([])
    let _ = Js.Array2.shift(tokens)

    list
  }
}

let readStr = input => input->tokenize->readForm
