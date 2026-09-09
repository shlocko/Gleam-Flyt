import gleam/list
import gleam/option.{type Option}
import lexer/token
import parser/ast
import resolver/ast.{type BindingId} as resolved_ast

pub type Binding {
  Binding(id: BindingId, name: token.Token, mut: Bool)
}

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
    exports: List(Binding),
    ast: List(ast.Expression),
    resolved_ast: Option(List(resolved_ast.ResolvedExpression)),
  )
}

pub fn add_export(module: Module, export: Binding) -> Result(Module, String) {
  case module.exports |> list.find(fn(binding) { binding.id == export.id }) {
    Ok(_) -> {
      Error("This export already exists")
    }
    Error(_) -> {
      let exports = [export, ..module.exports]
      Ok(Module(..module, exports: exports))
    }
  }
}

pub fn has_binding(module: Module, name: token.Token) -> Bool {
  todo
}
