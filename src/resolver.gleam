import compiler.{type CompilerState}
import gleam/dict
import gleam/list
import gleam/result
import modules
import parser/ast
import resolver/ast as resolved_ast
import scopes

pub fn resolve_program(
  expressions: List(ast.Expression),
) -> Result(List(resolved_ast.ResolvedExpression), String) {
  let scope_stack =
    scopes.ScopeStack(global: scopes.Scope([], scopes.BasicScope), locals: [])
  use #(_expressions, resolved_expressions, _scopes) <- result.try(
    resolve_program_helper(expressions, [], ScopeStack()),
  )
  Ok(resolved_expressions)
}

pub fn resolve_program_helper(
  expressions: List(ast.Expression),
  resolved_expressions: List(resolved_ast.ResolvedExpression),
  scopes: List(scopes.Scope),
) -> Result(
  #(
    List(ast.Expression),
    List(resolved_ast.ResolvedExpression),
    scopes.ScopeStack,
  ),
  String,
) {
  todo
}

pub fn resolve_expression(
  expression: ast.Expression,
  module_id: modules.ModuleId,
  compiler_state: CompilerState,
) -> Result(resolved_ast.ResolvedExpression, String) {
  todo
}

fn get_module(
  compiler_state: CompilerState,
  module_id: modules.ModuleId,
) -> Result(modules.Module, String) {
  compiler_state.modules
  |> dict.get(module_id)
  |> result.map_error(fn(_) { "Module not found" })
}
