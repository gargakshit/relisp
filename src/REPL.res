let read = input => Reader.readStr(input)

// let eval = (~env=?, ast) => ast
let eval = ast => ast

let print = exp => Printer.printToString(exp)

let rep = (input: string) => {
  switch read(input) {
  | Error(e) => Error(e)
  | Ok(ast) => Ok(ast->eval->print)
  }
}
