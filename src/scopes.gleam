import gleam/dict
import gleam/list
import modules
import resolver/ast as resolved_ast

pub type ScopeStack {
  ScopeStack(global: Scope, frames: List(Scope))
}

pub type Scope {
  Scope(names: List(modules.Binding), kind: ScopeKind)
}

pub type ScopeKind {
  FunctionScope
  BasicScope
}

pub fn add_global(
  stack: ScopeStack,
  new_binding: modules.Binding,
) -> Result(ScopeStack, String) {
  case
    stack.global.names |> list.find(fn(name) { name.name == new_binding.name })
  {
    Ok(_) ->
      Error(
        "This name has already been declared at the top-level of this module.",
      )
    Error(_) ->
      Ok(
        ScopeStack(
          ..stack,
          global: Scope(..stack.global, names: [
            new_binding,
            ..stack.global.names
          ]),
        ),
      )
  }
}
