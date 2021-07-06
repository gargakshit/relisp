type data = Js.Dict.t<ReLisp.t>

type rec t = {
  outer: option<t>,
  data: data,
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
