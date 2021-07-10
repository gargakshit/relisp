# ReLisp

A lisp ([mal](https://github.com/kanaka/mal)) dialect written in ReScript

### Building

- Install the npm dependencies using `yarn`
- Build the ReScript code using `yarn res:build`

### Running files

You can run a file using `node run.mjs file.relisp`.

### Examples

```clojure
(def! a (atom 1))

(def! my-fun
  (fn* [a]
    (print @a)
    (reset! a (+ @a 1))
    (print @a)))

(my-fun a)
```
