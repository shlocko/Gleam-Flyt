import code_gen/encoder
import code_gen/types.{
  type FunctionMetaData, type Instruction, type JEFValue, type Program,
} as gen_types
import code_gen/utils
import gleam/json
import gleam/list
import gleam/result
import lexer/token
import parser/expression.{type Expression} as expr
import parser/statement as stmt

pub fn compile_program(
  program: List(stmt.Statement),
) -> Result(json.Json, String) {
  // use #(instructions, consts) <- result.try(
  //   generate_expression(expression, [], []),
  // )
  // let instructions =
  //   [gen_types.Instruction("Print", []), ..instructions] |> list.reverse
  // let instructions = [gen_types.Instruction("Main", []), ..instructions]
  // let consts = consts |> list.reverse
  // Ok(encoder.encode_program(consts, [], instructions))
  Ok(json.string("ok"))
}

pub fn generate_expression(
  expression: Expression,
  instructions: List(Instruction),
  consts: List(JEFValue),
) -> Result(#(List(Instruction), List(JEFValue)), String) {
  case expression.kind {
    expr.BinaryOperator(op, left, right) -> {
      use #(left_instructions, left_consts) <- result.try(generate_expression(
        left,
        instructions,
        consts,
      ))
      use #(right_instructions, right_consts) <- result.try(generate_expression(
        right,
        left_instructions,
        left_consts,
      ))
      case op {
        token.Plus -> {
          Ok(#(
            [gen_types.Instruction("Add", []), ..right_instructions],
            right_consts,
          ))
        }
        token.Minus -> {
          Ok(#(
            [gen_types.Instruction("Sub", []), ..right_instructions],
            right_consts,
          ))
        }
        token.Star -> {
          Ok(#(
            [gen_types.Instruction("Mul", []), ..right_instructions],
            right_consts,
          ))
        }
        token.Slash -> {
          Ok(#(
            [gen_types.Instruction("Div", []), ..right_instructions],
            right_consts,
          ))
        }
        _ -> Error("Invalid binary operator")
      }
    }
    expr.Group(expr) -> {
      generate_expression(expr, instructions, consts)
    }
    expr.Int(num) -> {
      let #(idx, consts) = utils.find_or_push(gen_types.Int(num), consts)
      echo idx
      echo consts
      Ok(#(
        [
          gen_types.Instruction("PushConst", [gen_types.Int(idx)]),
          ..instructions
        ],
        consts,
      ))
    }
    expr.Float(num) -> {
      let #(idx, consts) = utils.find_or_push(gen_types.Float(num), consts)
      Ok(#(
        [
          gen_types.Instruction("PushConst", [gen_types.Int(idx)]),
          ..instructions
        ],
        consts,
      ))
    }
    _ -> todo
  }
}
