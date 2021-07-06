open ReLisp

let read = input => Reader.readStr(input)

let eval = (~env=?, ast) =>
  ast->Eval.evalReLisp(
    switch env {
    | None => Js.Dict.empty()
    | Some(e) => e
    },
  )

let print = exp => Printer.printToString(exp)

let rep = (input: string) => {
  let env = Js.Dict.fromList(list{
    (
      "+",
      Function.fromBootstrap(elems => ReLispNumber(
        elems->Belt.Array.reduce(0.0, (acc, el) =>
          switch el {
          | ReLispNumber(num, _) => num +. acc
          | _ => 0.0
          }
        ),
        None,
      )),
    ),
  })

  switch read(input) {
  | Error(e) => Error(e)
  | Ok(ast) =>
    switch eval(ast, ~env) {
    | Error(e) => Error(e)
    | Ok(result) => Ok(print(result))
    }
  }
}
