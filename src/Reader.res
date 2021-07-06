let tokenize = (input: string) => {
  let regex = %re("/[\s,]*(~@|[\[\]{}()'`~^@]|\"(?:\\\.|[^\\\\\"])*\"?|;.*|[^\s\[\]{}('\"`,;)]*)/g")

  let rec recursiveTokenizer = tokens => {
    switch regex->Js.Re.exec_(input) {
    | None => tokens
    | Some(matches) => {
        let captures = matches->Js.Re.captures
        switch captures[1]->Js.Nullable.toOption {
        | None => tokens
        | Some("") => tokens
        | Some(match) =>
          switch captures[0]->Js.Nullable.toOption {
          | None => tokens
          | Some(";") => recursiveTokenizer(tokens)
          | _ => {
              let _ = tokens->Js.Array2.push(match)
              recursiveTokenizer(tokens)
            }
          }
        }
      }
    }
  }

  recursiveTokenizer([])
}
