import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import lexer/token
import parser/ast
import type_checker/types

pub fn type_expression(
  expression: ast.Expression,
) -> Result(ast.Expression, String) {
  case expression.kind, expression.value_type {
    _, Some(_) -> Ok(expression)
    ast.Int(_), _ -> {
      Ok(ast.Expression(..expression, value_type: Some(types.Int)))
    }
    ast.Float(_), _ -> {
      Ok(ast.Expression(..expression, value_type: Some(types.Float)))
    }
    ast.Group(inner), _ -> {
      case inner.value_type {
        Some(kind) -> Ok(ast.Expression(..expression, value_type: Some(kind)))
        None -> {
          use inner_typed <- result.try(type_expression(inner))
          Ok(ast.Expression(
            kind: ast.Group(inner_typed),
            value_type: inner_typed.value_type,
          ))
        }
      }
    }
    ast.BinaryOperator(op, left, right), _ -> {
      use left_typed <- result.try(type_expression(left))
      use right_typed <- result.try(type_expression(right))
      case left_typed.value_type == right_typed.value_type {
        True ->
          Ok(
            ast.Expression(
              kind: ast.BinaryOperator(
                op: op,
                left: left_typed,
                right: right_typed,
              ),
              value_type: case op {
                token.EqualsEquals | token.BangEquals -> Some(types.Bool)
                _ -> left.value_type
              },
            ),
          )
        False ->
          Error("Mismatched types in left and right of binary expression.")
      }
    }
    ast.Identifier(_name), _ -> todo
    ast.If(condition, if_block, else_block), _ -> {
      case type_expression(condition) {
        Ok(expression) -> {
          case expression.value_type {
            Some(types.Bool) -> {
              use if_block <- result.try(type_expression(if_block))
              case else_block {
                Some(blk) -> {
                  use else_block <- result.try(type_expression(blk))
                  Ok(ast.Expression(
                    ast.If(
                      condition: expression,
                      if_block: if_block,
                      else_block: Some(else_block),
                    ),
                    None,
                  ))
                }
                None -> {
                  Ok(ast.Expression(
                    ast.If(
                      condition: expression,
                      if_block: if_block,
                      else_block: None,
                    ),
                    None,
                  ))
                }
              }
            }
            _ -> Error("Expected 'Bool' type inside 'if' condition expression.")
          }
        }
        Error(e) -> Error(e)
      }
    }
    ast.Block(expressions), _ -> {
      use expressions <- result.try(list.try_map(expressions, type_expression))
      use last <- result.try(
        expressions
        |> list.last
        |> result.map_error(fn(_) { "Unexpected empty list" }),
      )
      Ok(ast.Expression(ast.Block(expressions), last.value_type))
    }
    ast.Print(_), _ -> {
      Ok(expression)
    }
  }
}

pub fn type_check_program(
  expressions: List(ast.Expression),
) -> Result(List(ast.Expression), String) {
  use #(_, checked) <- result.try(type_check_program_helper(expressions, []))
  Ok(checked |> list.reverse)
}

fn type_check_program_helper(
  expressions: List(ast.Expression),
  checked: List(ast.Expression),
) -> Result(#(List(ast.Expression), List(ast.Expression)), String) {
  case expressions {
    [] -> Ok(#([], checked))
    [expression, ..rest] -> {
      use checked_expression <- result.try(type_expression(expression))
      let checked = [checked_expression, ..checked]
      type_check_program_helper(rest, checked)
    }
  }
}
