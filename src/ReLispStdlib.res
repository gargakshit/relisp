open ReLisp

@inline
let isSeq = val =>
  switch val {
  | ReLispList(list, _) => Some(list)
  | ReLispVector(list, _) => Some(list)
  | _ => None
  }

@inline
let isBrowser = () =>
  switch %external(__BROWSER__) {
  | Some(true) => true
  | _ => false
  }

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

let errorFun = Function.fromBootstrap(elems => {
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

let isError = Function.fromBootstrap(elems => {
  let len = Belt.Array.length(elems)

  switch len {
  | 1 =>
    switch elems[0] {
    | ReLispError(_, _) => ReLispBoolean(true, None)
    | _ => ReLispBoolean(false, None)
    }
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
    | ReLispList(_, _) => ReLispBoolean(true, None)
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
      if Js.Array2.length(arr1) == 0 {
        true
      } else {
        arr1->Js.Array2.reducei((acc, el, i) => acc || el == arr2[i], false)
      }
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

let slurpFun = Function.fromBootstrap(elems => {
  let len = Belt.Array.length(elems)

  @inline
  let performXhr = url => {
    let xhrFun: string => option<string> = %raw(
      // Can't use multiline, the formatter messes up
      "function(url) { const xhr = new XMLHttpRequest(); xhr.open(\"GET\", url, false); xhr.send(); if (xhr.status == 200) { return xhr.responseText; } else { return undefined; } }"
    )
    xhrFun(url)
  }

  switch len {
  | 1 =>
    switch elems[0] {
    | ReLispString(str, _) =>
      if isBrowser() {
        switch performXhr(str) {
        | None => ReLispError(`Failed to slurp ${str}`, None)
        | Some(s) => ReLispString(s, None)
        }
      } else {
        ReLispString(Node.Fs.readFileSync(str, #utf8), None)
      }
    | e => ReLispError(`Unexpected type ${type_(e)}, expected string`, None)
    }
  | _ => ReLispError(`Expected 1 argument, got ${len->Belt.Int.toString}`, None)
  }
})

let atomFun = Function.fromBootstrap(elems => {
  let len = Belt.Array.length(elems)

  switch len {
  | 1 => ReLispAtom(elems[0], None)
  | _ => ReLispError(`Expected 1 argument, got ${len->Belt.Int.toString}`, None)
  }
})

let isAtom = Function.fromBootstrap(elems => {
  let len = Belt.Array.length(elems)

  switch len {
  | 1 =>
    switch elems[0] {
    | ReLispAtom(_, _) => ReLispBoolean(true, None)
    | _ => ReLispBoolean(false, None)
    }
  | _ => ReLispError(`Expected 1 argument, got ${len->Belt.Int.toString}`, None)
  }
})

let derefAtom = Function.fromBootstrap(elems => {
  let len = Belt.Array.length(elems)

  switch len {
  | 1 =>
    switch elems[0] {
    | ReLispAtom(e, _) => e
    | e => ReLispError(`Unexpected type ${type_(e)}, expected atom`, None)
    }
  | _ => ReLispError(`Expected 1 argument, got ${len->Belt.Int.toString}`, None)
  }
})

// Reset doesn't mutate the original atom as the language is immutable
let resetFun = Function.fromBootstrap(elems => {
  let len = Belt.Array.length(elems)

  switch len {
  | 2 => ReLispAtom(elems[1], None)
  | _ => ReLispError(`Expected 1 argument, got ${len->Belt.Int.toString}`, None)
  }
})

// TODO: add swap

let nthFun = Function.fromBootstrap(elems => {
  let len = Belt.Array.length(elems)

  switch len {
  | 2 =>
    switch elems[1] {
    | ReLispNumber(nf, _) => {
        let n = Belt.Float.toInt(nf)

        @inline
        let getEl = arr =>
          switch arr->Belt.Array.get(n) {
          | None => ReLispNil(None)
          | Some(e) => e
          }

        switch elems[0] {
        | ReLispList(arr, _) => getEl(arr)
        | ReLispVector(arr, _) => getEl(arr)
        | e => ReLispError(`Unexpected type ${type_(e)}, expected list or vector`, None)
        }
      }
    | e => ReLispError(`Unexpected type ${type_(e)}, expected number`, None)
    }
  | _ => ReLispError(`Expected 2 arguments, got ${len->Belt.Int.toString}`, None)
  }
})

let consFun = Function.fromBootstrap(elems => {
  let len = Belt.Array.length(elems)

  switch len {
  | 2 =>
    switch elems[1] {
    | ReLispList(arr, _) => ReLispList([elems[0]]->Belt.Array.concat(arr), None)
    | ReLispVector(arr, _) => ReLispList([elems[0]]->Belt.Array.concat(arr), None)
    | e => ReLispError(`Unexpected type ${type_(e)}, expected list or vector`, None)
    }
  | _ => ReLispError(`Expected 2 arguments, got ${len->Belt.Int.toString}`, None)
  }
})

let concatFun = Function.fromBootstrap(elems => ReLispList(elems->Js.Array2.reduce((acc, el) =>
    switch el {
    | ReLispList(arr, _) => acc->Belt.Array.concat(arr)
    | ReLispVector(arr, _) => acc->Belt.Array.concat(arr)
    | _ => acc
    }
  , []), None))

let vecFun = Function.fromBootstrap(elems => {
  let len = Belt.Array.length(elems)

  switch len {
  | 1 =>
    switch elems[0] {
    | ReLispList(arr, meta) => ReLispVector(arr, meta)
    | ReLispVector(_, _) => elems[0]
    | e => ReLispError(`Unexpected type ${type_(e)}, expected list or vector`, None)
    }
  | _ => ReLispError(`Expected 1 argument, got ${len->Belt.Int.toString}`, None)
  }
})

let firstFun = Function.fromBootstrap(elems => {
  let len = Belt.Array.length(elems)

  switch len {
  | 1 =>
    switch elems[0]->isSeq {
    | Some(list) =>
      switch Belt.Array.get(list, 0) {
      | None => ReLispNil(None)
      | Some(e) => e
      }
    | _ => ReLispError(`Unexpected type ${type_(elems[0])}, expected list or vector`, None)
    }
  | _ => ReLispError(`Expected 1 argument, got ${len->Belt.Int.toString}`, None)
  }
})

let restFun = Function.fromBootstrap(elems => {
  let len = Belt.Array.length(elems)

  switch len {
  | 1 =>
    switch elems[0] {
    | ReLispNil(_) => ReLispNil(None)
    | _ =>
      switch elems[0]->isSeq {
      | Some(list) =>
        switch Belt.Array.length(list) {
        | 0 => ReLispNil(None)
        | 1 => ReLispNil(None)
        | _ => ReLispList(list->Js.Array2.slice(~start=1, ~end_=list->Js.Array2.length), None)
        }
      | _ => ReLispError(`Unexpected type ${type_(elems[0])}, expected list or vector`, None)
      }
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
  ("error", errorFun),
  ("error?", isError),
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
  ("atom", atomFun),
  ("atom?", isAtom),
  ("deref", derefAtom),
  ("reset!", resetFun),
  ("hash-map", hashMapFun),
  ("map?", isHashMap),
  ("empty?", emptyFun),
  ("count", countFun),
  ("read-string", readStringFun),
  ("slurp", slurpFun),
  ("nth", nthFun),
  ("cons", consFun),
  ("concat", concatFun),
  ("vec", vecFun),
  ("first", firstFun),
  ("rest", restFun),
  (
    "is-browser",
    Function.fromBootstrap(elems => {
      let len = Belt.Array.length(elems)

      switch len {
      | 0 => ReLispBoolean(isBrowser(), None)
      | _ => ReLispError(`Expected 0 arguments, got ${len->Belt.Int.toString}`, None)
      }
    }),
  ),
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
