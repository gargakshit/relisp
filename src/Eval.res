open ReLisp

let rec evalAst = (ast, env) => {
  switch ast {
  | ReLispSymbol(name, _) =>
    switch env->Js.Dict.get(name) {
    | None => Error(`Unknown Symbol: ${name}`)
    | Some(f) => Ok(f)
    }
  | ReLispList(list, _) =>
    switch evalList(list, env, 0, []) {
    | Error(e) => Error(e)
    | Ok(r) => Ok(ReLispList(r, None))
    }
  | ReLispVector(list, _) =>
    switch evalList(list, env, 0, []) {
    | Error(e) => Error(e)
    | Ok(r) => Ok(ReLispVector(r, None))
    }
  | else_ => Ok(else_)
  }
}
and evalReLisp = (ast, env) =>
  switch ast {
  | ReLispList(list, _) =>
    switch list->Js.Array2.length {
    | 0 => Ok(ast)
    | _ =>
      switch evalAst(ast, env) {
      | Error(e) => Error(e)
      | Ok(result) =>
        switch result {
        | ReLispList(list, _) => evalFunc(list)
        | ReLispVector(list, _) => evalFunc(list)
        | _ => Error(`Unexpected return type TODO, expected list or vector`)
        }
      }
    }
  | else_ => evalAst(else_, env)
  }
and evalFunc = list => {
  let f = Js.Array2.shift(list)

  switch f {
  | None => Error("Undefined function")
  | Some(ReLispFunction(fun, _, _)) => Ok(fun(list))
  | Some(other) => Ok(ReLispList([other]->Js.Array2.concat(list), None))
  }
}
and evalList = (list, env, i, result) => {
  if i == Belt.Array.length(list) {
    Ok(result)
  } else {
    switch evalReLisp(list[i], env) {
    | Error(e) => Error(e)
    | Ok(r) => evalList(list, env, i + 1, Belt.Array.concat(result, [r]))
    }
  }
}
