import compiler.{type CompilerState}
import gleam/dict
import gleam/list
import gleam/result
import modules
import parser/ast
import resolver/ast as resolved_ast

pub fn resolve_expression(
  expression: ast.Expression,
  module_id: modules.ModuleId,
  compiler_state: CompilerState,
) -> Result(resolved_ast.ResolvedExpression, String) {
  use module <- result.try(
    compiler_state.modules
    |> dict.get(module_id)
    |> result.map_error(fn(e) { "Dict key not found" }),
  )
  case expression.kind {
    ast.Static(identifier, initializer, mut) -> {
      case
        module.bound_names
        |> list.filter(fn(binding) { binding.name == identifier })
      {
        [] -> todo
        [_] -> todo
        _ -> Error("Too many bindings with this name")
      }
    }
    _ -> todo
  }
}
