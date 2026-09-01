import gleam/option.{type Option}
import parser/ast
import resolver/ast as resolved_ast

pub type ModuleId =
  Int

pub type ModuleInfo {
  ModuleInfo(id: ModuleId, parent: Option(ModuleId), name: String)
}

pub type ModulePath =
  List(String)

pub type Module {
  Module(
    id: ModuleId,
    parent: Option(ModuleId),
    name: String,
    ast: List(ast.Expression),
    resolved_ast: Option(List(resolved_ast.ResolvedExpression)),
  )
}
