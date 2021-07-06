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

let stdlib = Js.Dict.fromArray([("+", addFun), ("-", subFun), ("*", mulFun)])
