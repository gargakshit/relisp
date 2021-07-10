open ReLisp

@inline external identity: 'a => 'b = "%identity"

let rec jsToReLisp = val => {
  @inline
  let isString: 'a => bool = val => %raw("(val) => typeof val === 'string'")(val)

  @inline
  let isNum: 'a => bool = val => %raw("(val) => typeof val === 'number'")(val)

  @inline
  let isArray: 'a => bool = val =>
    %raw("(val) => typeof val === 'object' && Array.isArray(val)")(val)

  // @inline
  // let isHashMap: 'a => bool = val => %raw("(val) => typeof val === 'object'")(val)
  // TODO: add hashmap support

  if isString(val) {
    ReLispString(val->identity, None)
  } else if isNum(val) {
    ReLispNumber(val->identity, None)
  } else if isArray(val) {
    ReLispList(val->identity->Js.Array2.map(jsToReLisp), None)
  } else {
    ReLispNil(None)
  }
}
