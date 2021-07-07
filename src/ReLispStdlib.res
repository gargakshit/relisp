open ReLisp

let addFun = Function.fromBootstrap(elems => ReLispNumber(
  elems->Belt.Array.reduce(0.0, (acc, el) =>
    switch el {
    | ReLispNumber(num, _) => acc +. num
    | _ => 0.0
    }
  ),
  None,
))

let subFun = Function.fromBootstrap(elems =>
  switch elems[0] {
  | ReLispNumber(start, _) =>
    ReLispNumber(
      elems
      ->Js.Array2.slice(~start=1, ~end_=elems->Js.Array2.length)
      ->Belt.Array.reduce(start, (acc, el) =>
        switch el {
        | ReLispNumber(num, _) => acc -. num
        | _ => 0.0
        }
      ),
      None,
    )
  | _ => ReLispNumber(0.0, None)
  }
)
// Not the most efficient, I know :)

let mulFun = Function.fromBootstrap(elems => ReLispNumber(
  elems->Belt.Array.reduce(1.0, (acc, el) =>
    switch el {
    | ReLispNumber(num, _) => acc *. num
    | _ => 1.0
    }
  ),
  None,
))

let divFun = Function.fromBootstrap(elems =>
  switch elems[0] {
  | ReLispNumber(start, _) =>
    ReLispNumber(
      elems
      ->Js.Array2.slice(~start=1, ~end_=elems->Js.Array2.length)
      ->Belt.Array.reduce(start, (acc, el) =>
        switch el {
        | ReLispNumber(num, _) => acc /. num
        | _ => 0.0
        }
      ),
      None,
    )
  | _ => ReLispNumber(0.0, None)
  }
)
// Not the most efficient, I know :)

let typeFun = Function.fromBootstrap(elems =>
  switch Belt.Array.length(elems) {
  | 1 =>
    ReLispString(
      switch elems[0] {
      | ReLispList(_, _) => "list"
      | ReLispNumber(_, _) => "number"
      | ReLispString(_, _) => "string"
      | ReLispNil(_) => "nil"
      | ReLispBoolean(_, _) => "boolean"
      | ReLispSymbol(_, _) => "symbol"
      | ReLispKeyword(_, _) => "keyword"
      | ReLispVector(_, _) => "vector"
      | ReLispFunction(_, macro, _) =>
        switch macro {
        | false => "function"
        | true => "macro"
        }
      | ReLispAtom(_, _) => "atom"
      | ReLispError(_, _) => "error"
      },
      None,
    )
  | _ =>
    ReLispError(`Expected 1 argument, got ${Belt.Array.length(elems)->Belt.Int.toString}`, None)
  }
)

let makeErrorFun = Function.fromBootstrap(elems =>
  switch Belt.Array.length(elems) {
  | 1 =>
    ReLispError(
      switch elems[0] {
      | ReLispString(s, _) => s
      | ReLispError(e, _) => e
      | _ => `String or error expected`
      },
      None,
    )
  | _ =>
    ReLispError(`Expected 1 argument, got ${Belt.Array.length(elems)->Belt.Int.toString}`, None)
  }
)

let stdlib = Js.Dict.fromArray([
  ("+", addFun),
  ("-", subFun),
  ("*", mulFun),
  ("/", divFun),
  ("typeof", typeFun),
  ("make-error", makeErrorFun),
])
