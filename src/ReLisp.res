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
  | ReLispHashMap(hashmap, option<t>)
and f = array<t> => t
and envData = Js.Dict.t<t>
and hashmap = {
  keywordMap: Js.Dict.t<t>,
  stringMap: Js.Dict.t<t>,
}

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

module HashMap = {
  let new = arr => {
    let len = Belt.Array.length(arr)

    if len->mod(2) == 0 {
      let keywordMap = Js.Dict.empty()
      let stringMap = Js.Dict.empty()
      let chunks = arr->Utils.chunk(2)

      let rec recursiveMapper = chunks =>
        switch Js.Array2.shift(chunks) {
        | None => Ok()
        | Some(chunk) =>
          switch chunk[0] {
          | ReLispString(s, _) => {
              stringMap->Js.Dict.set(s, chunk[1])
              recursiveMapper(chunks)
            }
          | ReLispKeyword(s, _) => {
              keywordMap->Js.Dict.set(s, chunk[1])
              recursiveMapper(chunks)
            }
          | _ => Error("Invalid key type. Either string or keyword is accepted")
          }
        }

      switch recursiveMapper(chunks) {
      | Ok(_) =>
        Ok(
          ReLispHashMap(
            {
              keywordMap: keywordMap,
              stringMap: stringMap,
            },
            None,
          ),
        )
      | Error(e) => Error(e)
      }
    } else {
      Error("All keys don't have a value")
    }
  }
}
