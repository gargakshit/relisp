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

let makeErrorFun = Function.fromBootstrap(elems => {
  let len = Belt.Array.length(elems)

  switch len {
  | 1 =>
    ReLispError(
      switch elems[0] {
      | ReLispString(s, _) => s
      | ReLispError(e, _) => e
      | _ => `String or error expected`
      },
      None,
    )
  | _ => ReLispError(`Expected 1 argument, got ${len->Belt.Int.toString} arguments`, None)
  }
})

let typeFun = Function.fromBootstrap(elems => {
  let len = Belt.Array.length(elems)

  switch len {
  | 1 => ReLispString(type_(elems[0]), None)
  | _ => ReLispError(`Expected 1 argument, got ${len->Belt.Int.toString}`, None)
  }
})

let isNil = Function.fromBootstrap(elems => {
  let len = Belt.Array.length(elems)

  switch len {
  | 1 =>
    switch elems[0] {
    | ReLispNil(_) => ReLispBoolean(true, None)
    | _ => ReLispBoolean(false, None)
    }
  | _ => ReLispError(`Expected 1 argument, got ${len->Belt.Int.toString}`, None)
  }
})

let isTrue = Function.fromBootstrap(elems => {
  let len = Belt.Array.length(elems)

  switch len {
  | 1 =>
    switch elems[0] {
    | ReLispBoolean(val, _) => ReLispBoolean(val, None)
    | _ => ReLispBoolean(false, None)
    }
  | _ => ReLispError(`Expected 1 argument, got ${len->Belt.Int.toString}`, None)
  }
})

let isFalse = Function.fromBootstrap(elems => {
  let len = Belt.Array.length(elems)

  switch len {
  | 1 =>
    switch elems[0] {
    | ReLispBoolean(val, _) => ReLispBoolean(!val, None)
    | _ => ReLispBoolean(false, None)
    }
  | _ => ReLispError(`Expected 1 argument, got ${len->Belt.Int.toString}`, None)
  }
})

let isString = Function.fromBootstrap(elems => {
  let len = Belt.Array.length(elems)

  switch len {
  | 1 =>
    switch elems[0] {
    | ReLispString(_, _) => ReLispBoolean(true, None)
    | _ => ReLispBoolean(false, None)
    }
  | _ => ReLispError(`Expected 1 argument, got ${len->Belt.Int.toString}`, None)
  }
})

let isSymbol = Function.fromBootstrap(elems => {
  let len = Belt.Array.length(elems)

  switch len {
  | 1 =>
    switch elems[0] {
    | ReLispSymbol(_, _) => ReLispBoolean(true, None)
    | _ => ReLispBoolean(false, None)
    }
  | _ => ReLispError(`Expected 1 argument, got ${len->Belt.Int.toString}`, None)
  }
})

let symbolFun = Function.fromBootstrap(elems => {
  let len = Belt.Array.length(elems)

  switch len {
  | 1 =>
    switch elems[0] {
    | ReLispString(s, _) => ReLispSymbol(s, None)
    | e => ReLispError(`Unexpected type ${type_(e)}, expected string`, None)
    }
  | _ => ReLispError(`Expected 1 argument, got ${len->Belt.Int.toString}`, None)
  }
})

let keywordFun = Function.fromBootstrap(elems => {
  let len = Belt.Array.length(elems)

  switch len {
  | 1 =>
    switch elems[0] {
    | ReLispString(s, _) => ReLispKeyword(s, None)
    | ReLispKeyword(s, m) => ReLispKeyword(s, m)
    | e => ReLispError(`Unexpected type ${type_(e)}, expected string or keyword`, None)
    }
  | _ => ReLispError(`Expected 1 argument, got ${len->Belt.Int.toString}`, None)
  }
})

let isKeyword = Function.fromBootstrap(elems => {
  let len = Belt.Array.length(elems)

  switch len {
  | 1 =>
    switch elems[0] {
    | ReLispKeyword(_, _) => ReLispBoolean(true, None)
    | _ => ReLispBoolean(false, None)
    }
  | _ => ReLispError(`Expected 1 argument, got ${len->Belt.Int.toString}`, None)
  }
})

let isNumber = Function.fromBootstrap(elems => {
  let len = Belt.Array.length(elems)

  switch len {
  | 1 =>
    switch elems[0] {
    | ReLispNumber(_, _) => ReLispBoolean(true, None)
    | _ => ReLispBoolean(false, None)
    }
  | _ => ReLispError(`Expected 1 argument, got ${len->Belt.Int.toString}`, None)
  }
})

let isFn = Function.fromBootstrap(elems => {
  let len = Belt.Array.length(elems)

  switch len {
  | 1 =>
    switch elems[0] {
    | ReLispFunction(_, false, _) => ReLispBoolean(true, None)
    | _ => ReLispBoolean(false, None)
    }
  | _ => ReLispError(`Expected 1 argument, got ${len->Belt.Int.toString}`, None)
  }
})

let isMacro = Function.fromBootstrap(elems => {
  let len = Belt.Array.length(elems)

  switch len {
  | 1 =>
    switch elems[0] {
    | ReLispFunction(_, true, _) => ReLispBoolean(true, None)
    | _ => ReLispBoolean(false, None)
    }
  | _ => ReLispError(`Expected 1 argument, got ${len->Belt.Int.toString}`, None)
  }
})

let strFun = Function.fromBootstrap(elems => ReLispString(
  elems->Belt.Array.map(Printer.printToString)->Js.Array2.joinWith(" "),
  None,
))

let printFun = Function.fromBootstrap(elems => {
  Js.Console.log(elems->Belt.Array.map(Printer.printToString)->Js.Array2.joinWith(" "))
  ReLispNil(None)
})

let listFun = Function.fromBootstrap(elems => ReLispList(elems, None))

let isList = Function.fromBootstrap(elems => {
  let len = Belt.Array.length(elems)

  switch len {
  | 1 =>
    switch elems[0] {
    | ReLispVector(_, _) => ReLispBoolean(true, None)
    | _ => ReLispBoolean(false, None)
    }
  | _ => ReLispError(`Expected 1 argument, got ${len->Belt.Int.toString}`, None)
  }
})

let vectorFun = Function.fromBootstrap(elems => ReLispVector(elems, None))

let isVector = Function.fromBootstrap(elems => {
  let len = Belt.Array.length(elems)

  switch len {
  | 1 =>
    switch elems[0] {
    | ReLispVector(_, _) => ReLispBoolean(true, None)
    | _ => ReLispBoolean(false, None)
    }
  | _ => ReLispError(`Expected 1 argument, got ${len->Belt.Int.toString}`, None)
  }
})

let hashMapFun = Function.fromBootstrap(elems =>
  switch HashMap.new(elems) {
  | Ok(hashmap) => hashmap
  | Error(e) => ReLispError(e, None)
  }
)

let isHashMap = Function.fromBootstrap(elems => {
  let len = Belt.Array.length(elems)

  switch len {
  | 1 =>
    switch elems[0] {
    | ReLispHashMap(_, _) => ReLispBoolean(true, None)
    | _ => ReLispBoolean(false, None)
    }
  | _ => ReLispError(`Expected 1 argument, got ${len->Belt.Int.toString}`, None)
  }
})

let countFun = Function.fromBootstrap(elems => {
  let len = Belt.Array.length(elems)

  switch len {
  | 1 =>
    switch elems[0] {
    | ReLispList(arr, _) => ReLispNumber(Js.Array2.length(arr)->Belt.Int.toFloat, None)
    | ReLispVector(arr, _) => ReLispNumber(Js.Array2.length(arr)->Belt.Int.toFloat, None)
    | e => ReLispError(`Unexpected type ${type_(e)}, expected list or vector`, None)
    }
  | _ => ReLispError(`Expected 1 argument, got ${len->Belt.Int.toString}`, None)
  }
})

let stdlib = Js.Dict.fromArray([
  ("+", addFun),
  ("-", subFun),
  ("*", mulFun),
  ("/", divFun),
  ("typeof", typeFun),
  ("make-error", makeErrorFun),
  ("nil?", isNil),
  ("true?", isTrue),
  ("false?", isFalse),
  ("string?", isString),
  ("symbol?", isSymbol),
  ("symbol", symbolFun),
  ("keyword", keywordFun),
  ("keyword?", isKeyword),
  ("number?", isNumber),
  ("fn?", isFn),
  ("macro?", isMacro),
  ("str", strFun),
  ("print", printFun),
  ("list", listFun),
  ("list?", isList),
  ("vector", vectorFun),
  ("vector?", isVector),
  ("hash-map", hashMapFun),
  ("map?", isHashMap),
  ("count", countFun),
])
