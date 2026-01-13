import gleam/option.{type Option, None, Some}
import gleam/result
import parser/expression as expr
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
