open Zora

zoraBlock("reader", t => {
  t->test("it tokenizes the program", t => {
    let program = "(+ 1 (+ 2 (- 3 4)))"
    let tokens = ReLisp.tokenize(program)

    t->equal(
      tokens,
      ["(", "+", "1", "(", "+", "2", "(", "-", "3", "4", ")", ")", ")"],
      "Should return the correct tokens",
    )

    done()
  })
})
