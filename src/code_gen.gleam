import code_gen/encoder
import code_gen/types.{
  type FunctionMetaData, type Instruction, type JEFValue, type Program,
} as gen_types
import code_gen/utils
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import lexer/token
import parser/expression.{type Expression} as expr
import parser/statement.{type Statement} as stmt

pub fn compile_program(
  program: List(stmt.Statement),
) -> Result(json.Json, String) {
  echo program
  use #(instructions, consts, _labels) <- result.try(generate_statements(
    program,
    [],
    [],
    0,
  ))
  // let instructions =
  //   [gen_types.Instruction("Print", []), ..instructions] |> list.reverse
  let instructions = instructions |> list.reverse
  let instructions = [gen_types.Instruction("Main", []), ..instructions]
  Ok(encoder.encode_program(consts, [], instructions))
  // Ok(json.string("ok"))
}

fn generate_statements(
  statements: List(Statement),
  instructions: List(Instruction),
  consts: List(JEFValue),
  labels: Int,
) -> Result(#(List(Instruction), List(JEFValue), Int), String) {
  case statements {
    [] -> Ok(#(instructions, consts, labels))
    [statement, ..rest] -> {
      echo rest
      use #(instructions, consts, labels) <- result.try(generate_statement(
        statement,
        instructions,
        consts,
        labels,
      ))
      generate_statements(rest, instructions, consts, labels)
    }
  }
}

pub fn generate_statement(
  statement: Statement,
  instructions: List(Instruction),
  consts: List(JEFValue),
  labels: Int,
) -> Result(#(List(Instruction), List(JEFValue), Int), String) {
  case statement {
    stmt.Expression(expr) -> {
      use #(instructions_expr, consts_expr, labels) <- result.try(
        generate_expression(expr, instructions, consts, labels),
      )
      let instructions_expr = [
        gen_types.Instruction("Print", []),
        ..instructions_expr
      ]
      Ok(#(instructions_expr, consts_expr, labels))
    }
    stmt.Block(statements) -> {
      generate_block(statements, instructions, consts, labels)
    }
    stmt.If(condition, if_block, else_block) -> {
      use #(instructions, consts, labels) <- result.try(generate_expression(
        condition,
        instructions,
        consts,
        labels,
      ))
      // let labels = labels + 1
      // let instructions = [
      //   gen_types.Instruction("Label", [
      //     gen_types.String(labels |> int.to_string),
      //   ]),
      //   ..instructions
      // ]
      let else_label = labels + 1
      let instructions = [
        gen_types.Instruction("JumpIfFalse", [
          gen_types.String(else_label |> int.to_string),
        ]),
        ..instructions
      ]
      use #(instructions, consts, labels) <- result.try(generate_statement(
        if_block,
        instructions,
        consts,
        else_label,
      ))
      let instructions = [
        gen_types.Instruction("Label", [
          gen_types.String(else_label |> int.to_string),
        ]),
        ..instructions
      ]
      case else_block {
        Some(statement) -> {
          use #(instructions, consts, labels) <- result.try(generate_expression(
            condition,
            instructions,
            consts,
            labels,
          ))
          let end_label = labels + 1
          let instructions = [
            gen_types.Instruction("JumpIfTrue", [
              gen_types.String(end_label |> int.to_string),
            ]),
            ..instructions
          ]
          use #(instructions, consts, labels) <- result.try(generate_statement(
            statement,
            instructions,
            consts,
            end_label,
          ))

          let instructions = [
            gen_types.Instruction("Label", [
              gen_types.String(end_label |> int.to_string),
            ]),
            ..instructions
          ]
          Ok(#(instructions, consts, end_label))
        }
        None -> Ok(#(instructions, consts, labels))
      }
    }
    _ -> todo
  }
}

fn generate_block(
  statements: List(Statement),
  instructions: List(Instruction),
  consts: List(JEFValue),
  labels: Int,
) -> Result(#(List(Instruction), List(JEFValue), Int), String) {
  case statements {
    [] -> Ok(#(instructions, consts, labels))
    [statement, ..rest] -> {
      use #(instructions, consts, labels) <- result.try(generate_statement(
        statement,
        instructions,
        consts,
        labels,
      ))
      generate_block(rest, instructions, consts, labels)
    }
  }
}

pub fn generate_expression(
  expression: Expression,
  instructions: List(Instruction),
  consts: List(JEFValue),
  labels: Int,
) -> Result(#(List(Instruction), List(JEFValue), Int), String) {
  case expression.kind {
    expr.BinaryOperator(op, left, right) -> {
      use #(left_instructions, left_consts, labels) <- result.try(
        generate_expression(left, instructions, consts, labels),
      )
      use #(right_instructions, right_consts, labels) <- result.try(
        generate_expression(right, left_instructions, left_consts, labels),
      )
      case op {
        token.Plus -> {
          Ok(#(
            [gen_types.Instruction("Add", []), ..right_instructions],
            right_consts,
            labels,
          ))
        }
        token.Minus -> {
          Ok(#(
            [gen_types.Instruction("Sub", []), ..right_instructions],
            right_consts,
            labels,
          ))
        }
        token.Star -> {
          Ok(#(
            [gen_types.Instruction("Mul", []), ..right_instructions],
            right_consts,
            labels,
          ))
        }
        token.Slash -> {
          Ok(#(
            [gen_types.Instruction("Div", []), ..right_instructions],
            right_consts,
            labels,
          ))
        }
        token.EqualsEquals -> {
          Ok(#(
            [gen_types.Instruction("Equal", []), ..right_instructions],
            right_consts,
            labels,
          ))
        }
        token.BangEquals -> {
          Ok(#(
            [gen_types.Instruction("NotEqual", []), ..right_instructions],
            right_consts,
            labels,
          ))
        }
        _ -> Error("Invalid binary operator")
      }
    }
    expr.Group(expr) -> {
      generate_expression(expr, instructions, consts, labels)
    }
    expr.Int(num) -> {
      let #(idx, consts) = utils.find_or_push(gen_types.Int(num), consts)
      echo num
      echo idx
      echo consts
      Ok(#(
        [
          gen_types.Instruction("PushConst", [gen_types.Int(idx)]),
          ..instructions
        ],
        consts,
        labels,
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
        labels,
      ))
    }
    _ -> todo
  }
}
