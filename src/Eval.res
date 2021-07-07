open ReLisp

let rec evalAst = (ast, env) => {
  switch ast {
  | ReLispSymbol(name, _) =>
    switch env->Env.get(name) {
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
      switch list[0] {
      | ReLispSymbol("def!", _) =>
        switch list->Belt.Array.get(1) {
        | None => Error("Expected a key, got nil")
        | Some(ReLispSymbol(key, _)) =>
          switch list->Belt.Array.get(2) {
          | None => Error("Unexpected syntax")
          | Some(value) =>
            switch evalReLisp(value, env) {
            | Ok(evalVal) => Ok(env->Env.set(key, evalVal))
            | Error(e) => Error(e)
            }
          }
        | Some(e) => Error(`Invalid type ${type_(e)}, expected symbol`)
        }
      | ReLispSymbol("let*", _) => {
          let letEnv = Env.new(Some(env), Js.Dict.empty())

          let process = (arr, ast) => {
            let len = arr->Js.Array2.length

            if len == 0 {
              Error("No keys added")
            } else if len->mod(2) != 0 {
              Error("All keys don't have a value")
            } else {
              let chunks = Utils.chunk(arr, 2)

              let rec recursiveForEach = chunks =>
                switch Js.Array2.shift(chunks) {
                | None => evalReLisp(ast, letEnv)
                | Some(chunk) =>
                  switch chunk[0] {
                  | ReLispSymbol(symbol, _) =>
                    switch evalReLisp(chunk[1], letEnv) {
                    | Error(e) => Error(e)
                    | Ok(evalVal) => {
                        let _ = env->Env.set(symbol, evalVal)
                        recursiveForEach(chunks)
                      }
                    }
                  | _ => Error(`Invalid type ${type_(chunk[0])}, expected symbol`)
                  }
                }

              recursiveForEach(chunks)
            }
          }

          switch list->Belt.Array.get(2) {
          | None => Error("Unexpected syntax")
          | Some(ast) =>
            switch list->Belt.Array.get(1) {
            | None => Error("Expected a list or vector, got nil")
            | Some(ReLispVector(arr, _)) => process(arr, ast)
            | Some(ReLispList(arr, _)) => process(arr, ast)
            | Some(e) => Error(`Invalid type ${type_(e)}, expected list or vector`)
            }
          }
        }
      | ReLispSymbol("do", None) => {
          let astList = list->Js.Array.slice(~start=1, ~end_=list->Js.Array2.length)
          switch evalAst(ReLispList(astList, None), env) {
          | Error(e) => Error(e)
          | Ok(v) =>
            switch v {
            | ReLispList(val, _) => Ok(val[val->Js.Array2.length - 1])
            | ReLispVector(val, _) => Ok(val[val->Js.Array2.length - 1])
            | ret => Error(`Invalid type ${type_(ret)}, expected list or vector`)
            }
          }
        }
      | ReLispSymbol("if", None) =>
        switch list->Belt.Array.get(1) {
        | None => Error("No condition present")
        | Some(cond) =>
          switch evalReLisp(cond, env) {
          | Error(e) => Error(e)
          | Ok(ReLispBoolean(true, None)) =>
            switch list->Belt.Array.get(2) {
            | None => Error("No then expression present")
            | Some(thenExpr) => evalReLisp(thenExpr, env)
            }
          | Ok(_) =>
            switch list->Belt.Array.get(3) {
            | None => Ok(ReLispNil(None))
            | Some(elseExpr) => evalReLisp(elseExpr, env)
            }
          }
        }
      | ReLispSymbol("fn*", None) => {
          let rec checkArgs = (~i=0, args, retArgs) =>
            if i == Belt.Array.length(args) {
              Ok(retArgs)
            } else {
              switch args[i] {
              | ReLispSymbol(a, _) => {
                  let _ = retArgs->Js.Array2.push(a)
                  checkArgs(args, retArgs, ~i=i + 1)
                }
              | el => Error(`Invalid type ${type_(el)}, expected a symbol`)
              }
            }

          let getFun = args =>
            switch list->Belt.Array.get(2) {
            | None => Error("No function body present")
            | Some(body) =>
              Ok(
                Function.fromBootstrap(fnArgs =>
                  switch evalReLisp(
                    body,
                    Env.new(
                      Some(env),
                      Js.Dict.fromArray(args->Js.Array2.mapi((arg, i) => (arg, fnArgs[i]))),
                    ),
                  ) {
                  | Error(e) => ReLispError(e, None)
                  | Ok(e) => e
                  }
                ),
              )
            }

          switch list->Belt.Array.get(1) {
          | None => Error("No args present")
          | Some(ReLispList(args, _)) =>
            switch checkArgs(args, []) {
            | Error(e) => Error(e)
            | Ok(e) => getFun(e)
            }
          | Some(ReLispVector(args, _)) =>
            switch checkArgs(args, []) {
            | Error(e) => Error(e)
            | Ok(e) => getFun(e)
            }
          | Some(e) => Error(`Invalid type ${type_(e)}, expected a list or vector`)
          }
        }
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
