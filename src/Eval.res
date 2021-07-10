open ReLisp

@inline
let isSeq = val =>
  switch val {
  | ReLispList(list, _) => Some(list)
  | ReLispVector(list, _) => Some(list)
  | _ => None
  }

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
      switch macroExpand(ast, env) {
      | Error(e) => Error(e)
      | Ok(ast) =>
        switch isSeq(ast) {
        | Some(list) =>
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

                switch list->Belt.Array.get(2) {
                | None => Error("Unexpected syntax")
                | Some(ast) =>
                  switch list[1]->isSeq {
                  | None => Error(`Invalid type ${type_(list[1])}, expected list or vector`)
                  | Some(arr) => {
                      let len = arr->Js.Array2.length

                      if len == 0 {
                        Error("No keys added")
                      } else if len->mod(2) != 0 {
                        Error("All keys don't have a value")
                      } else {
                        let chunks = Utils.chunk(arr, 2)

                        @inline
                        let rec recursiveForEach = chunks =>
                          switch Js.Array2.shift(chunks) {
                          | None => Ok()
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

                        switch recursiveForEach(chunks) {
                        | Error(e) => Error(e)
                        | Ok(_) => evalReLisp(ast, letEnv)
                        }
                      }
                    }
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
                | Ok(ReLispBoolean(false, None)) =>
                  switch list->Belt.Array.get(3) {
                  | None => Ok(ReLispNil(None))
                  | Some(thenExpr) => evalReLisp(thenExpr, env)
                  }
                | Ok(ReLispNil(None)) =>
                  switch list->Belt.Array.get(3) {
                  | None => Ok(ReLispNil(None))
                  | Some(thenExpr) => evalReLisp(thenExpr, env)
                  }
                | Ok(_) =>
                  switch list->Belt.Array.get(2) {
                  | None => Error("No then expression present")
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
                  | Some(body) => Ok(Function.fromLisp(evalReLisp, env, args, body))
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
            | ReLispSymbol("defmacro!", None) =>
              switch list->Belt.Array.get(1) {
              | None => Error("No name present")
              | Some(ReLispSymbol(name, _)) =>
                switch list->Belt.Array.get(2) {
                | None => Error("No macro body present")
                | Some(e) =>
                  switch evalReLisp(e, env) {
                  | Error(e) => Error(e)
                  | Ok(f) =>
                    switch f {
                    | ReLispFunction(_, _, _) => Ok(env->Env.set(name, f->Function.toMacro))
                    | e => Error(`Invalid type ${type_(e)}, expected a function`)
                    }
                  }
                }
              | Some(e) => Error(`Invalid type ${type_(e)}, expected a symbol`)
              }
            | ReLispSymbol("macroexpand", None) => macroExpand(list[1], env)
            | ReLispSymbol("quote", None) =>
              if Belt.Array.length(list) == 2 {
                Ok(list[1])
              } else {
                Error(`Expected 1 argument, got ${Belt.Array.length(list)->Belt.Int.toString}`)
              }
            | ReLispSymbol("quasiquoteexpand", None) =>
              if Belt.Array.length(list) == 2 {
                Ok(quasiQuote(list[1]))
              } else {
                Error(`Expected 1 argument, got ${Belt.Array.length(list)->Belt.Int.toString}`)
              }
            | ReLispSymbol("quasiquote", None) =>
              if Belt.Array.length(list) == 2 {
                evalReLisp(quasiQuote(list[1]), env)
              } else {
                Error(`Expected 1 argument, got ${Belt.Array.length(list)->Belt.Int.toString}`)
              }
            | _ => {
                @inline
                let evalFunc = list => {
                  let f = Js.Array2.shift(list)

                  switch f {
                  | None => Error("Undefined function")
                  | Some(ReLispFunction(fun, _, _)) =>
                    switch fun.ast {
                    | None => Ok(fun.fun(list))
                    | Some(ast) =>
                      evalReLisp(
                        ast,
                        Env.new(
                          Some(
                            switch fun.env {
                            | None => env
                            | Some(env) => env
                            },
                          ),
                          Env.dataFromLists(fun.params, list),
                        ),
                      )
                    }
                  // | Some(other) => Ok(ReLispList([other]->Js.Array2.concat(list), None))
                  | Some(other) => Error(`Undefined function ${Printer.printToString(other)}`)
                  }
                }

                switch evalAst(ast, env) {
                | Error(e) => Error(e)
                | Ok(result) =>
                  switch result {
                  | ReLispList(list, _) => evalFunc(list)
                  | ReLispVector(list, _) => evalFunc(list)
                  | e => Error(`Unexpected return type ${type_(e)}, expected list or vector`)
                  }
                }
              }
            }
          }
        | _ => evalAst(ast, env)
        }
      }
    }
  | else_ => evalAst(else_, env)
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
and quasiQuote = ast =>
  switch ast {
  | ReLispSymbol(_, _) => ReLispList([ReLispSymbol("quote", None), ast], None)
  | ReLispHashMap(_, _) => ReLispList([ReLispSymbol("quote", None), ast], None)
  | ReLispList([ReLispSymbol("unquote", _), el1], _) => el1
  | ReLispList(list, _) => qqFoldr(list)
  | ReLispVector(vec, _) => ReLispList([ReLispSymbol("vec", None), qqFoldr(vec)], None)
  | _ => ast
  }
and qqLoop = (acc, el) =>
  switch el {
  | ReLispList([ReLispSymbol("splice-unquote", None), el1], _) =>
    ReLispList([ReLispSymbol("concat", None), el1, acc], None)
  | _ => ReLispList([ReLispSymbol("cons", None), quasiQuote(el), acc], None)
  }
and qqFoldr = xs => xs->Js.Array2.reduceRight(qqLoop, ReLispList([], None))
and macroExpand = (ast, env) => {
  @inline
  let process = list =>
    switch list[0] {
    | ReLispSymbol(name, _) =>
      switch env->Env.get(name) {
      | None => Error(`No macro with the name ${name} found`)
      | Some(f) =>
        switch f {
        | ReLispFunction(fun, true, _) =>
          Ok(fun.fun(list->Js.Array2.slice(~start=1, ~end_=list->Js.Array2.length)))
        | e => Error(`Invalid type ${type_(e)}, expected a macro`)
        }
      }
    | e => Error(`Invalid type ${type_(e)}, expected a symbol`)
    }

  if isMacro(ast, env) {
    switch ast {
    | ReLispList(list, _) =>
      switch process(list) {
      | Error(e) => Error(e)
      | Ok(ast) => macroExpand(ast, env)
      }
    | ReLispVector(list, _) =>
      switch process(list) {
      | Error(e) => Error(e)
      | Ok(ast) => macroExpand(ast, env)
      }
    | e => Error(`Invalid type ${type_(e)}, expected a list or vector`)
    }
  } else {
    Ok(ast)
  }
}
and isMacro = (ast, env) => {
  @inline
  let check = ast =>
    switch switch ast {
    | ReLispList(l, _) => Some(l[0])
    | ReLispVector(l, _) => Some(l[0])
    | _ => None
    } {
    | Some(ReLispSymbol(name, _)) => Some(name)
    | _ => None
    }

  switch check(ast) {
  | None => false
  | Some(name) =>
    switch env->Env.get(name) {
    | Some(ReLispFunction(_, true, _)) => true
    | _ => false
    }
  }
}
