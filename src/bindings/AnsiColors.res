type t
@module("ansi-colors") external defaultExport: t = "default"

@send external bold: (t, string) => string = "bold"
@send external cyan: (t, string) => string = "cyan"
@send external redBright: (t, string) => string = "redBright"
@send external whiteBright: (t, string) => string = "whiteBright"
@send external bgRed: (t, string) => string = "bgRed"
@send external greenBright: (t, string) => string = "greenBright"
@send external gray: (t, string) => string = "gray"
