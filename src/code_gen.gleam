import code_gen/types.{
  type FunctionMetaData, type Instruction, type JEFValue, type Program,
} as gen_types
import code_gen/utils
import gleam/list
import gleam/result
import lexer/token
import parser/expression.{type Expression} as expr

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
      Ok(#([gen_types.Instruction("PushConst", [gen_types.Int(idx)])], consts))
    }
    expr.Float(num) -> {
      let #(idx, consts) = utils.find_or_push(gen_types.Float(num), consts)
      Ok(#([gen_types.Instruction("PushConst", [gen_types.Int(idx)])], consts))
    }
    _ -> todo
  }
}
