open ReLisp

let rec printToString = input =>
  switch input {
  | ReLispList(l, _) =>
    "(" ++ l->Belt.Array.map(e => printToString(e))->Belt.Array.joinWith(" ", e => e) ++ ")"
  | ReLispNumber(n, _) => Belt.Float.toString(n)
  | ReLispString(s, _) => s
  | ReLispNil(_) => "nil"
  | ReLispBoolean(b, _) => string_of_bool(b)
  | ReLispSymbol(s, _) => s
  | ReLispKeyword(k, _) => ":" ++ k
  | ReLispVector(v, _) =>
    "[" ++ v->Belt.Array.map(e => printToString(e))->Belt.Array.joinWith(" ", e => e) ++ "]"
  | ReLispFunction(_, _, _) => "#<function>"
  | ReLispAtom(a, _) => "(atom " ++ printToString(a) ++ ")"
  | ReLispError(s, _) => "ERROR: " ++ s
  }
