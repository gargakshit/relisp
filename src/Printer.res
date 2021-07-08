open ReLisp

let rec printToString = input =>
  switch input {
  | ReLispList(l, _) =>
    "(" ++ l->Belt.Array.map(e => printToString(e))->Js.Array2.joinWith(" ") ++ ")"
  | ReLispNumber(n, _) => Belt.Float.toString(n)
  | ReLispString(s, _) => s
  | ReLispNil(_) => "nil"
  | ReLispBoolean(b, _) => string_of_bool(b)
  | ReLispSymbol(s, _) => s
  | ReLispKeyword(k, _) => ":" ++ k
  | ReLispVector(v, _) =>
    "[" ++ v->Belt.Array.map(e => printToString(e))->Belt.Array.joinWith(" ", e => e) ++ "]"
  | ReLispFunction(_, false, _) => "#<function>"
  | ReLispFunction(_, true, _) => "#<macro>"
  | ReLispAtom(a, _) => "(atom " ++ printToString(a.contents) ++ ")"
  | ReLispError(s, _) => "ERROR: " ++ s
  | ReLispHashMap(h, _) =>
    "{" ++
    h.keywordMap
    ->Js.Dict.entries
    ->Js.Array2.map(((key, val)) => ":" ++ key ++ " " ++ printToString(val))
    ->Js.Array2.joinWith(" ") ++
    if (
      h.stringMap->Js.Dict.values->Js.Array2.length == 0 ||
        h.keywordMap->Js.Dict.values->Js.Array2.length == 0
    ) {
      ""
    } else {
      " "
    } ++
    h.stringMap
    ->Js.Dict.entries
    ->Js.Array2.map(((key, val)) => "\"" ++ key ++ "\" " ++ printToString(val))
    ->Js.Array2.joinWith(" ") ++ "}"
  }
