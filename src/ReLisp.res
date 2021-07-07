type rec t =
  | ReLispList(array<t>, option<t>)
  | ReLispNumber(float, option<t>)
  | ReLispString(string, option<t>)
  | ReLispNil(option<t>)
  | ReLispBoolean(bool, option<t>)
  | ReLispSymbol(string, option<t>)
  | ReLispKeyword(string, option<t>)
  | ReLispVector(array<t>, option<t>)
  | ReLispFunction(f, bool, option<t>)
  | ReLispAtom(t, option<t>)
  | ReLispError(string, option<t>)
and f = array<t> => t
and envData = Js.Dict.t<t>

module Env = {
  type rec t = {
    outer: option<t>,
    data: envData,
  }

  let set = (env, key, value) => {
    env->Js.Dict.set(key, value)
    value
  }

  let find = (env, key) =>
    switch env.data->Js.Dict.get(key) {
    | None =>
      switch env.outer {
      | None => None
      | Some(e) =>
        switch e.data->Js.Dict.get(key) {
        | None => None
        | Some(_) => Some(e)
        }
      }
    | Some(_) => Some(env)
    }

  let get = (env, key) =>
    switch find(env, key) {
    | None => None
    | Some(e) => e.data->Js.Dict.get(key)
    }

  let new = (outer, data) => {outer: outer, data: data}

  let dataFromLists = (keys, values) => {
    let dict = Js.Dict.empty()
    keys->Belt.Array.forEachWithIndex((i, key) => dict->Js.Dict.set(key, values[i]))
    dict
  }
}

module Function = {
  let toMacro = ast =>
    switch ast {
    | ReLispFunction(f, _, meta) => ReLispFunction(f, true, meta)
    | else_ => else_
    }

  let fromBootstrap = func => ReLispFunction(func, false, None)

  let fromLisp = (eval, env, params, body) => ReLispFunction(
    args => eval(body, Env.new(Some(env), Env.dataFromLists(params, args))),
    false,
    None,
  )
}
