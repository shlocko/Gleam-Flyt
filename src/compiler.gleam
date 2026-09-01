import gleam/dict.{type Dict}
import gleam/json
import modules

pub type CompilerState {
  CompilerState(
    modules: Dict(modules.ModuleId, modules.Module),
    worklist: List(modules.ModulePath),
  )
}

pub fn compile_program(entry_file: String) -> Result(json.Json, String) {
  todo
}
