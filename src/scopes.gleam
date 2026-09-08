import gleam/dict
import modules
import resolver/ast as resolved_ast

pub type ScopeStack {
  ScopeStack(global: Scope, locals: List(Scope))
}

pub type Scope {
  Scope(names: List(modules.Binding), kind: ScopeKind)
}

pub type ScopeKind {
  FunctionScope
  BasicScope
}
