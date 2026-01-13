import code_gen/types as gen_types
import gleam/json.{type Json}

fn encode_jefvalue(value: gen_types.JEFValue) -> Json {
  json.object([
    case value {
      gen_types.Int(num) -> #("Int", json.int(num))
      gen_types.Float(num) -> #("Float", json.float(num))
      gen_types.String(str) -> #("String", json.string(str))
      gen_types.Bool(b) -> #("Bool", json.bool(b))
    },
  ])
}

fn encode_instruction(instruction: gen_types.Instruction) -> Json {
  let instruction_json_list: List(Json) = [
    json.string(instruction.opcode),
    json.array(instruction.args, fn(x) { encode_jefvalue(x) }),
  ]
  json.array(instruction_json_list, fn(x) { x })
}

fn encode_function_meta_data(meta_data: gen_types.FunctionMetaData) -> Json {
  json.object([
    #("address", json.int(meta_data.address)),
    #("arity", json.int(meta_data.arity)),
    #("locals", json.int(meta_data.locals)),
  ])
}

pub fn encode_program(
  consts: List(gen_types.JEFValue),
  functions: List(gen_types.FunctionMetaData),
  instructions: List(gen_types.Instruction),
) -> Json {
  let json_consts: Json = json.array(consts, encode_jefvalue)
  let json_functions: Json = json.array(functions, encode_function_meta_data)
  let json_instructions: Json = json.array(instructions, encode_instruction)
  json.object({
    [
      #("consts", json_consts),
      #("functions", json_functions),
      #("code", json_instructions),
    ]
  })
}
