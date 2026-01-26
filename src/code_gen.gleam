import code_gen/encoder
import code_gen/types.{type Instruction, type JEFValue} as gen_types
import code_gen/utils
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import lexer/token
import parser/ast.{type Expression}

pub fn compile_program(program: List(Expression)) -> Result(json.Json, String) {
  use #(instructions, consts, _labels) <- result.try(generate_expressions(
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

fn generate_expressions(
  expressions: List(Expression),
  instructions: List(Instruction),
  consts: List(JEFValue),
  labels: Int,
) -> Result(#(List(Instruction), List(JEFValue), Int), String) {
  case expressions {
    [] -> Ok(#(instructions, consts, labels))
    [expression, ..rest] -> {
      use #(instructions, consts, labels) <- result.try(generate_expression(
        expression,
        instructions,
        consts,
        labels,
      ))
      generate_expressions(rest, instructions, consts, labels)
    }
  }
}

fn generate_block(
  expressions: List(Expression),
  instructions: List(Instruction),
  consts: List(JEFValue),
  labels: Int,
) -> Result(#(List(Instruction), List(JEFValue), Int), String) {
  case expressions {
    [] -> Ok(#(instructions, consts, labels))
    [expression, ..rest] -> {
      use #(instructions, consts, labels) <- result.try(generate_expression(
        expression,
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
    ast.BinaryOperator(op, left, right) -> {
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
    ast.Group(expr) -> {
      generate_expression(expr, instructions, consts, labels)
    }
    ast.Int(num) -> {
      let #(idx, consts) = utils.find_or_push(gen_types.Int(num), consts)
      Ok(#(
        [
          gen_types.Instruction("PushConst", [gen_types.Int(idx)]),
          ..instructions
        ],
        consts,
        labels,
      ))
    }
    ast.Float(num) -> {
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
    ast.Block(expressions) -> {
      generate_block(expressions, instructions, consts, labels)
    }
    ast.If(condition, if_block, else_block) -> {
      use #(instructions, consts, labels) <- result.try(generate_expression(
        condition,
        instructions,
        consts,
        labels,
      ))
      let else_label = labels + 1
      let instructions = [
        gen_types.Instruction("JumpIfFalse", [
          gen_types.String(else_label |> int.to_string),
        ]),
        ..instructions
      ]
      use #(instructions, consts, labels) <- result.try(generate_expression(
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
          use #(instructions, consts, _labels) <- result.try(
            generate_expression(statement, instructions, consts, end_label),
          )

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
    ast.Print(expression) -> {
      use #(instructions, consts, labels) <- result.try(generate_expression(
        expression,
        instructions,
        consts,
        labels,
      ))
      let instructions = [gen_types.Instruction("Print", []), ..instructions]
      Ok(#(instructions, consts, labels))
    }
    ast.Identifier(_name) -> todo
    // _ -> todo
  }
}
