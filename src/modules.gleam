import gleam/option.{type Option}
import parser/ast
import resolver/ast as resolved_ast

pub type ModuleId =
  Int

pub type ModuleInfo {
  ModuleInfo(id: ModuleId, parent: ModuleId)
}

pub type ModulePath =
  List(String)

pub type Module {
  Module(
    id: ModuleId,
    parent: Option(ModuleId),
    ast: List(ast.Expression),
    resolved_ast: List(resolved_ast.ResolvedExpression),
  )
}
