import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import parser/expression as expr
import parser/statement as stmt
import type_checker/types

pub fn type_expression(
  expression: expr.Expression,
) -> Result(expr.Expression, String) {
  case expression.kind, expression.value_type {
    _, Some(_) -> Ok(expression)
    expr.Int(_), _ -> {
      Ok(expr.Expression(..expression, value_type: Some(types.Int)))
    }
    expr.Float(_), _ -> {
      Ok(expr.Expression(..expression, value_type: Some(types.Float)))
    }
    expr.Group(inner), _ -> {
      case inner.value_type {
        Some(kind) -> Ok(expr.Expression(..expression, value_type: Some(kind)))
        None -> {
          use inner_typed <- result.try(type_expression(inner))
          Ok(expr.Expression(
            kind: expr.Group(inner_typed),
            value_type: inner_typed.value_type,
          ))
        }
      }
    }
    expr.BinaryOperator(op, left, right), _ -> {
      use left_typed <- result.try(type_expression(left))
      use right_typed <- result.try(type_expression(right))
      case left_typed.value_type == right_typed.value_type {
        True ->
          Ok(expr.Expression(
            kind: expr.BinaryOperator(
              op: op,
              left: left_typed,
              right: right_typed,
            ),
            value_type: left_typed.value_type,
          ))
        False ->
          Error("Mismatched types in left and right of binary expression.")
      }
    }
    expr.Identifier(_name), _ -> todo
  }
}

pub fn type_check_statement(
  statement: stmt.Statement,
) -> Result(stmt.Statement, String) {
  case statement {
    stmt.If(condition, if_block, else_block) -> {
      case type_expression(condition) {
        Ok(expression) -> {
          case expression.value_type {
            Some(types.Bool) -> {
              use if_block <- result.try(type_check_statement(if_block))
              case else_block {
                Some(blk) -> {
                  use else_block <- result.try(type_check_statement(blk))
                  Ok(stmt.If(
                    condition: expression,
                    if_block: if_block,
                    else_block: Some(else_block),
                  ))
                }
                None -> {
                  Ok(stmt.If(
                    condition: expression,
                    if_block: if_block,
                    else_block: None,
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
    stmt.Block(statements) -> {
      // list.try_each(statements, type_check_statement)
      use statements <- result.try(list.try_map(
        statements,
        type_check_statement,
      ))
      Ok(stmt.Block(statements))
    }
    stmt.Expression(expression) -> {
      use expression <- result.try(type_expression(expression))
      Ok(stmt.Expression(expression))
    }
  }
}

pub fn type_check_program(
  statements: List(stmt.Statement),
) -> Result(List(stmt.Statement), String) {
  // echo statements
  use #(_, checked) <- result.try(type_check_program_helper(statements, []))
  Ok(checked |> list.reverse)
}

fn type_check_program_helper(
  statements: List(stmt.Statement),
  checked: List(stmt.Statement),
) -> Result(#(List(stmt.Statement), List(stmt.Statement)), String) {
  case statements {
    [] -> Ok(#([], checked))
    [statement, ..rest] -> {
      use checked_statement <- result.try(type_check_statement(statement))
      let checked = [checked_statement, ..checked]
      type_check_program_helper(rest, checked)
    }
  }
}
