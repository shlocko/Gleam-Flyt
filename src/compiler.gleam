import gleam/dict.{type Dict}
import gleam/json
import gleam/list
import gleam/option.{None}
import gleam/result
import gleam/string
import lexer
import modules.{Module}
import parser
import simplifile

pub type CompilerState {
  CompilerState(
    modules: Dict(modules.ModuleId, modules.Module),
    worklist: List(modules.ModulePath),
  )
}

pub fn compile_program(entry_module: String) -> Result(json.Json, String) {
  use source <- result.try(
    simplifile.read(entry_module <> ".flyt")
    |> result.map_error(fn(err) {
      "Could not read the entry file: " <> entry_module <> ".flyt"
    }),
  )

  use #(_, _, entry_module_tokens) <- result.try(lexer.lex(#(source, 0, [])))
  let entry_module_tokens = list.reverse(entry_module_tokens)
  use #(entry_module_ast, entry_worklist) <- result.try(parser.parse(
    entry_module_tokens,
  ))

  let compiler_state =
    CompilerState(
      modules: dict.from_list([
        #(
          0,
          Module(
            id: 0,
            parent: None,
            name: entry_module,
            exports: [],
            ast: entry_module_ast,
            resolved_ast: None,
          ),
        ),
      ]),
      worklist: entry_worklist,
    )
  echo compiler_state
  todo
}
