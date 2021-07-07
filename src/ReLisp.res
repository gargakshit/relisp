@val @scope("Object")
external merge: (@as(json`{}`) _, Js.Dict.t<'a>, Js.Dict.t<'a>) => Js.Dict.t<'a> = "assign"

let deleteFromMap = (dict, key) => {
  let deleteFun: (Js.Dict.t<'a>, Js.Dict.key) => unit = %raw(
    "function(map, key) { delete map[key]; }"
  )
  deleteFun(dict, key)
}

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
and f = {
  fun: array<t> => t,
  ast: option<t>,
  env: option<envT>,
  params: array<Js.Dict.key>,
}
and envData = Js.Dict.t<t>
and hashmap = {
  keywordMap: Js.Dict.t<t>,
  stringMap: Js.Dict.t<t>,
}
and envT = {
  outer: option<envT>,
  data: envData,
}

let type_ = elem =>
  switch elem {
  | ReLispList(_, _) => "list"
  | ReLispNumber(_, _) => "number"
  | ReLispString(_, _) => "string"
  | ReLispNil(_) => "nil"
  | ReLispBoolean(_, _) => "boolean"
  | ReLispSymbol(_, _) => "symbol"
  | ReLispKeyword(_, _) => "keyword"
  | ReLispVector(_, _) => "vector"
  | ReLispFunction(_, macro, _) =>
    switch macro {
    | false => "function"
    | true => "macro"
    }
  | ReLispAtom(_, _) => "atom"
  | ReLispError(_, _) => "error"
  | ReLispHashMap(_, _) => "hashmap"
  }

module Env = {
  let set = (env, key, value) => {
    env.data->Js.Dict.set(key, value)
    value
  }

  let rec find = (env, key) =>
    switch env.data->Js.Dict.get(key) {
    | None =>
      switch env.outer {
      | None => None
      | Some(e) =>
        switch e->find(key) {
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

  let fromBootstrap = func => ReLispFunction(
    {fun: func, ast: None, env: None, params: []},
    false,
    None,
  )

  let fromLisp = (eval, env, params, body) => ReLispFunction(
    {
      fun: args =>
        switch eval(body, Env.new(Some(env), Env.dataFromLists(params, args))) {
        | Error(e) => ReLispError(e, None)
        | Ok(e) => e
        },
      ast: Some(body),
      env: Some(env),
      params: params,
    },
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

  let has = (hashmap, key) =>
    switch key {
    | ReLispKeyword(s, _) =>
      switch hashmap.keywordMap->Js.Dict.get(s) {
      | None => Ok(false)
      | Some(_) => Ok(true)
      }
    | ReLispString(s, _) =>
      switch hashmap.stringMap->Js.Dict.get(s) {
      | None => Ok(false)
      | Some(_) => Ok(true)
      }
    | _ => Error(`Unexpected type ${type_(key)}, expected string or keyword`)
    }

  let get = (hashmap, key) =>
    switch key {
    | ReLispKeyword(s, _) =>
      switch hashmap.keywordMap->Js.Dict.get(s) {
      | None => Ok(None)
      | Some(e) => Ok(Some(e))
      }
    | ReLispString(s, _) =>
      switch hashmap.stringMap->Js.Dict.get(s) {
      | None => Ok(None)
      | Some(e) => Ok(Some(e))
      }
    | _ => Error(`Unexpected type ${type_(key)}, expected string or keyword`)
    }

  let entries = hashmap =>
    Js.Dict.entries(hashmap.keywordMap)
    ->Js.Array2.map(((v1, v2)) => ReLispList([ReLispKeyword(v1, None), v2], None))
    ->Js.Array2.concat(
      Js.Dict.entries(hashmap.stringMap)->Js.Array2.map(((v1, v2)) => ReLispList(
        [ReLispString(v1, None), v2],
        None,
      )),
    )

  let keys = hashmap =>
    Js.Dict.keys(hashmap.keywordMap)
    ->Js.Array2.map(key => ReLispKeyword(key, None))
    ->Js.Array2.concat(
      Js.Dict.keys(hashmap.stringMap)->Js.Array2.map(key => ReLispString(key, None)),
    )

  let values = hashmap =>
    Js.Dict.values(hashmap.keywordMap)->Js.Array2.concat(Js.Dict.values(hashmap.stringMap))

  let assoc = (hashmap, list) =>
    switch new(list) {
    | Error(e) => Error(e)
    | Ok(newHashMap) =>
      switch newHashMap {
      | ReLispHashMap(newHashMap, _) =>
        Ok({
          keywordMap: merge(hashmap.keywordMap, newHashMap.keywordMap),
          stringMap: merge(hashmap.stringMap, newHashMap.stringMap),
        })
      | _ => Error(`Invalid type ${type_(newHashMap)}, expected hashmap`) // Edge case, it won't happen (I guess)
      }
    }

  let dissoc = (hashmap, list) =>
    switch assoc(hashmap, list) {
    | Error(e) => Error(e)
    | Ok(newHashMap) => {
        let rec recursiveForEach = list =>
          switch Js.Array2.shift(list) {
          | None => Ok()
          | Some(lastElem) =>
            switch lastElem {
            | ReLispString(s, _) => {
                deleteFromMap(newHashMap.stringMap, s)
                recursiveForEach(list)
              }
            | ReLispKeyword(k, _) => {
                deleteFromMap(newHashMap.keywordMap, k)
                recursiveForEach(list)
              }
            | _ => Error(`Invalid type ${type_(lastElem)}, expected hashmap`)
            }
          }

        switch recursiveForEach(list) {
        | Error(e) => Error(e)
        | Ok(_) => Ok(newHashMap)
        }
      }
    }
}
