import compiler.{type CompilerState}
import gleam/dict
import gleam/list
import gleam/result
import modules
import parser/ast
import resolver/ast as resolved_ast
import scopes

pub type ResolverContext {
  ResolverContext(scopes: scopes.ScopeStack, compiler_state: CompilerState)
}

pub fn resolve_module(
  expressions: List(ast.Expression),
  compiler_state: CompilerState,
) -> Result(#(List(resolved_ast.ResolvedExpression), CompilerState), String) {
  let context =
    ResolverContext(
      scopes: scopes.ScopeStack(
        global: scopes.Scope([], scopes.BasicScope),
        frames: [],
      ),
      compiler_state: compiler_state,
    )
  use #(resolved_expressions, context) <- result.try(
    expressions
    |> list.try_fold(#([], context), fn(acc, expr) {
      let #(resolved, context) = acc
      use #(resolved_expr, context) <- result.try(resolve_expression(
        expr,
        context,
      ))
      Ok(#([resolved_expr, ..resolved], context))
    }),
  )
  Ok(#(resolved_expressions, context.compiler_state))
}

pub fn resolve_expression(
  expression: ast.Expression,
  context: ResolverContext,
) -> Result(#(resolved_ast.ResolvedExpression, ResolverContext), String) {
  // Now you can resolve a given expression, creating a new resolved expression to replace whatever expression you got in, and updating the context to handled scopes as you go
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
