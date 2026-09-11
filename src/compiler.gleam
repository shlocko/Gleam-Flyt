import compiler/types.{type CompilerState, CompilerState}
import gleam/dict
import gleam/json
import gleam/list
import gleam/option.{None}
import gleam/result
import lexer
import modules.{Module}
import parser
import resolver
import simplifile

pub fn compile_program(entry_module: String) -> Result(json.Json, String) {
  use source <- result.try(
    simplifile.read(entry_module <> ".flyt")
    |> result.map_error(fn(_err) {
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
      next_ast_id: 0,
    )
  echo compiler_state
  use #(entry_module_resolved, compiler_state) <- result.try(
    resolver.resolve_module(entry_module_ast, compiler_state),
  )
  echo entry_module_resolved
  todo as "End of compile function."
}

pub fn get_ast_id(state: CompilerState) -> #(Int, CompilerState) {
  #(
    state.next_ast_id,
    CompilerState(..state, next_ast_id: state.next_ast_id + 1),
  )
}
