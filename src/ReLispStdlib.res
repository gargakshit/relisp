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
  elems->Belt.Array.map(Printer.printToString)->Js.Array2.joinWith(""),
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

let eqFun = elems => {
  let len = Belt.Array.length(elems)

  let listEq = (arr1, arr2) =>
    if Js.Array2.length(arr1) == Js.Array2.length(arr2) {
      arr1->Js.Array2.reducei((acc, el, i) => acc || el == arr2[i], false)
    } else {
      false
    }

  switch len {
  | 2 =>
    switch elems[0] {
    | ReLispList(arr1, _) =>
      switch elems[1] {
      | ReLispList(arr2, _) => Ok(listEq(arr1, arr2))
      | ReLispVector(arr2, _) => Ok(listEq(arr1, arr2))
      | _ => Ok(false)
      }
    | ReLispNumber(num1, _) =>
      switch elems[1] {
      | ReLispNumber(num2, _) => Ok(num1 == num2)
      | _ => Ok(false)
      }
    | ReLispString(str1, _) =>
      switch elems[1] {
      | ReLispString(str2, _) => Ok(str1 == str2)
      | _ => Ok(false)
      }
    | ReLispBoolean(bool1, _) =>
      switch elems[1] {
      | ReLispBoolean(bool2, _) => Ok(bool1 == bool2)
      | _ => Ok(false)
      }
    | ReLispSymbol(str1, _) =>
      switch elems[1] {
      | ReLispSymbol(str2, _) => Ok(str1 == str2)
      | _ => Ok(false)
      }
    | ReLispKeyword(str1, _) =>
      switch elems[1] {
      | ReLispKeyword(str2, _) => Ok(str1 == str2)
      | _ => Ok(false)
      }
    | ReLispVector(arr1, _) =>
      switch elems[1] {
      | ReLispList(arr2, _) => Ok(listEq(arr1, arr2))
      | ReLispVector(arr2, _) => Ok(listEq(arr1, arr2))
      | _ => Ok(false)
      }
    | ReLispFunction(_, _, _) => Ok(false)
    | ReLispAtom(_, _) => Ok(false)
    | ReLispError(e1, _) =>
      switch elems[1] {
      | ReLispError(e2, _) => Ok(e1 == e2)
      | _ => Ok(false)
      }
    | ReLispHashMap(h1, _) =>
      switch elems[1] {
      | ReLispHashMap(h2, _) =>
        Ok(
          h1.keywordMap->Js.Dict.keys->listEq(h2.keywordMap->Js.Dict.keys) &&
          h1.keywordMap->Js.Dict.values->listEq(h2.keywordMap->Js.Dict.values) &&
          h1.stringMap->Js.Dict.keys->listEq(h2.stringMap->Js.Dict.keys) &&
          h1.stringMap->Js.Dict.values->listEq(h2.stringMap->Js.Dict.values),
        )
      | _ => Ok(false)
      }
    | ReLispNil(_) =>
      switch elems[1] {
      | ReLispNil(_) => Ok(true)
      | _ => Ok(false)
      }
    }
  | _ => Error(`Expected 2 arguments, got ${len->Belt.Int.toString}`)
  }
}

let gtFun = elems => {
  let len = Belt.Array.length(elems)

  switch len {
  | 2 =>
    switch elems[0] {
    | ReLispNumber(n1, _) =>
      switch elems[1] {
      | ReLispNumber(n2, _) => Ok(n1 > n2)
      | e => Error(`Invalid type ${type_(e)}, expected number`)
      }
    | e => Error(`Invalid type ${type_(e)}, expected number`)
    }
  | _ => Error(`Expected 2 arguments, got ${len->Belt.Int.toString}`)
  }
}

let ltFun = elems => {
  let len = Belt.Array.length(elems)

  switch len {
  | 2 =>
    switch elems[0] {
    | ReLispNumber(n1, _) =>
      switch elems[1] {
      | ReLispNumber(n2, _) => Ok(n1 < n2)
      | e => Error(`Invalid type ${type_(e)}, expected number`)
      }
    | e => Error(`Invalid type ${type_(e)}, expected number`)
    }
  | _ => Error(`Expected 2 arguments, got ${len->Belt.Int.toString}`)
  }
}

let geFun = elems => {
  let len = Belt.Array.length(elems)

  switch len {
  | 2 =>
    switch elems[0] {
    | ReLispNumber(n1, _) =>
      switch elems[1] {
      | ReLispNumber(n2, _) => Ok(n1 >= n2)
      | e => Error(`Invalid type ${type_(e)}, expected number`)
      }
    | e => Error(`Invalid type ${type_(e)}, expected number`)
    }
  | _ => Error(`Expected 2 arguments, got ${len->Belt.Int.toString}`)
  }
}

let leFun = elems => {
  let len = Belt.Array.length(elems)

  switch len {
  | 2 =>
    switch elems[0] {
    | ReLispNumber(n1, _) =>
      switch elems[1] {
      | ReLispNumber(n2, _) => Ok(n1 <= n2)
      | e => Error(`Invalid type ${type_(e)}, expected number`)
      }
    | e => Error(`Invalid type ${type_(e)}, expected number`)
    }
  | _ => Error(`Expected 2 arguments, got ${len->Belt.Int.toString}`)
  }
}

let emptyFun = Function.fromBootstrap(elems => {
  let len = Belt.Array.length(elems)

  switch len {
  | 1 =>
    switch elems[0] {
    | ReLispList(arr, _) => ReLispBoolean(Js.Array2.length(arr) == 0, None)
    | ReLispVector(arr, _) => ReLispBoolean(Js.Array2.length(arr) == 0, None)
    | e => ReLispError(`Unexpected type ${type_(e)}, expected list or vector`, None)
    }
  | _ => ReLispError(`Expected 1 argument, got ${len->Belt.Int.toString}`, None)
  }
})

let readStringFun = Function.fromBootstrap(elems => {
  let len = Belt.Array.length(elems)

  switch len {
  | 1 =>
    switch elems[0] {
    | ReLispString(str, _) =>
      switch Reader.readStr(str) {
      | Error(e) => ReLispError(e, None)
      | Ok(e) => e
      }
    | e => ReLispError(`Unexpected type ${type_(e)}, expected string`, None)
    }
  | _ => ReLispError(`Expected 1 argument, got ${len->Belt.Int.toString}`, None)
  }
})

// TODO: add slurp

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
  ("empty?", emptyFun),
  ("count", countFun),
  ("read-string", readStringFun),
  (
    "=",
    Function.fromBootstrap(elems =>
      switch eqFun(elems) {
      | Ok(e) => ReLispBoolean(e, None)
      | Error(e) => ReLispError(e, None)
      }
    ),
  ),
  (
    ">",
    Function.fromBootstrap(elems =>
      switch gtFun(elems) {
      | Ok(e) => ReLispBoolean(e, None)
      | Error(e) => ReLispError(e, None)
      }
    ),
  ),
  (
    ">=",
    Function.fromBootstrap(elems =>
      switch geFun(elems) {
      | Ok(e) => ReLispBoolean(e, None)
      | Error(e) => ReLispError(e, None)
      }
    ),
  ),
  (
    "<",
    Function.fromBootstrap(elems =>
      switch ltFun(elems) {
      | Ok(e) => ReLispBoolean(e, None)
      | Error(e) => ReLispError(e, None)
      }
    ),
  ),
  (
    "<=",
    Function.fromBootstrap(elems =>
      switch leFun(elems) {
      | Ok(e) => ReLispBoolean(e, None)
      | Error(e) => ReLispError(e, None)
      }
    ),
  ),
  (
    "time-ms",
    Function.fromBootstrap(elems => {
      let len = Belt.Array.length(elems)

      switch len {
      | 0 => ReLispNumber(Js.Date.now(), None)
      | _ => ReLispError(`Expected 0 arguments, got ${len->Belt.Int.toString}`, None)
      }
    }),
  ),
])
