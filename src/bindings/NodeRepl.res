type server

type error
@new external newError: string => error = "error"

type context

type replOptions<'writer> = {
  prompt: string,
  eval: (string, context, string, (option<error>, 'writer) => unit) => unit,
  completer: option<string => (array<string>, string)>,
  writer: option<'writer => string>,
}

@module("repl") external start: replOptions<'writer> => server = "start"
