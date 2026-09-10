import gleam/dict.{type Dict}
import modules

pub type CompilerState {
  CompilerState(
    modules: Dict(modules.ModuleId, modules.Module),
    worklist: List(modules.ModulePath),
  )
}
