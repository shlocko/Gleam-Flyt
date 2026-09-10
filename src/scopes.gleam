import gleam/dict
import gleam/list
import lexer/token
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

pub fn has_name(scope: Scope, name: token.Token) -> Bool {
  case scope.names |> list.find(fn(binding) { binding.name == name }) {
    Ok(_) -> True
    Error(_) -> False
  }
}

pub fn add_global(
  stack: ScopeStack,
  new_binding: modules.Binding,
) -> ScopeStack {
  ScopeStack(
    ..stack,
    global: Scope(..stack.global, names: [new_binding, ..stack.global.names]),
  )
}
