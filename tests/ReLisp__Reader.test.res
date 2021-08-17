open Zora

zoraBlock("reader", t => {
  t->test("it tokenizes the program properly", t => {
    t->equal(
      ReLisp.tokenize("(+ 1 (+ 2 (- 3 4)))"),
      ["(", "+", "1", "(", "+", "2", "(", "-", "3", "4", ")", ")", ")"],
      "Should return the correct tokens for a basic program",
    )

    t->equal(ReLisp.tokenize("     "), [], "Should return an empty list in case of empty string")

    t->equal(
      ReLisp.tokenize("  (   +   1            , 2         )     "),
      ["(", "+", "1", "2", ")"],
      "Ignores spaces and commas",
    )

    t->equal(
      ReLisp.tokenize("; Comment\n;; Yet another comment (+ 3 4) \n (+ 4 5)"),
      ["(", "+", "4", "5", ")"],
      "Ignores the comments",
    )

    done()
  })
})
