import compiler/types.{type CompilerState}
import gleam/dict
import gleam/list
import gleam/option.{None}
import gleam/result
import gleam/string
import modules
import parser/ast
import resolver/ast as resolved_ast
import scopes

pub type ResolverContext {
  ResolverContext(
    scopes: scopes.ScopeStack,
    compiler_state: CompilerState,
    next_binding_id: Int,
  )
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
      next_binding_id: 0,
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
  case expression.kind {
    ast.Static(identifier, initializer, mut) -> {
      case scopes.has_name(context.scopes.global, identifier) {
        True -> Error("Name has already been bound at top level of module.")
        False -> {
          let #(new_binding_id, context) = get_binding_id(context)
          let scopes =
            scopes.add_global(
              context.scopes,
              modules.Binding(new_binding_id, identifier, mut),
            )
          let context = ResolverContext(..context, scopes: scopes)
          use #(initializer, context) <- result.try(resolve_expression(
            initializer,
            context,
          ))
          let #(node_id, compiler_state) =
            compiler.get_ast_id(context.compiler_state)
          let context = ResolverContext(..context, compiler_state:)
          let resolved_expression =
            resolved_ast.ResolvedExpression(resolved_ast.Static(
              new_binding_id,
              initializer,
              mut,
            ))
          Ok(#(resolved_expression, context))
        }
      }
    }
    ast.Int(num) -> {
      Ok(#(
        resolved_ast.ResolvedExpression(resolved_ast.Int(num), None),
        context,
      ))
    }
    ast.Print(expr) -> {
      use #(resolved_expr, context) <- result.try(resolve_expression(
        expr,
        context,
      ))
      Ok(#(
        resolved_ast.ResolvedExpression(resolved_ast.Print(resolved_expr), None),
        context,
      ))
    }
    ast.Group(expr) -> {
      use #(resolved_expr, context) <- result.try(resolve_expression(
        expr,
        context,
      ))
      Ok(#(
        resolved_ast.ResolvedExpression(resolved_ast.Group(resolved_expr), None),
        context,
      ))
    }
    _ ->
      todo as {
        "Not yet implemented in resolver: " <> string.inspect(expression)
      }
  }
}

fn get_module(
  compiler_state: CompilerState,
  module_id: modules.ModuleId,
) -> Result(modules.Module, String) {
  compiler_state.modules
  |> dict.get(module_id)
  |> result.map_error(fn(_) { "Module not found" })
}

fn get_binding_id(
  context: ResolverContext,
) -> #(resolved_ast.BindingId, ResolverContext) {
  #(
    context.next_binding_id,
    ResolverContext(..context, next_binding_id: context.next_binding_id + 1),
  )
}
