let read = (input: string) => input

let eval = (~env=?, ast) => ast

let print = exp => exp

let rep = (input: string) => {
  read(input)->eval->print
}
