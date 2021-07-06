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
  switch read(input) {
  | Error(e) => Error(e)
  | Ok(ast) =>
    switch eval(ast, ~env=ReLispStdlib.stdlib) {
    | Error(e) => Error(e)
    | Ok(result) => Ok(print(result))
    }
  }
}
