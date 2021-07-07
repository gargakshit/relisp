let chunk = (arr, n) => {
  let newLen = Belt.Array.length(arr) / n
  let newArr = []

  for x in 0 to newLen - 1 {
    let _ = newArr->Js.Array2.push(arr->Js.Array2.slice(~start=n * x, ~end_=n * x + n))
  }

  newArr
}
