import { repl } from "./src/ReLisp.mjs";

if (process.argv.length === 3) {
  const fname = process.argv[2];
  const program = [
    `(def! load-file
      (fn* (f)
        (eval
         (read-string (str
             "(do "
             (slurp f)
             "nil)")))))`,
    `(load-file "header.relisp")`,
    `(load-file "${fname}")`,
  ];

  const result = repl(undefined, program, [])._0[2];
  if (result !== "nil") {
    console.log(result);
  }
} else {
  console.error("Please specify a file name");
  process.exit(1);
}
