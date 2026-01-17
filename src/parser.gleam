import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import lexer/token.{type Token}
import parser/expression as expr
import parser/statement as stmt
import type_checker/types

pub type ExpressionState =
  #(expr.Expression, List(Token))

pub type ExpressionResult =
  Result(ExpressionState, String)

pub type StatementState =
  #(stmt.Statement, List(Token))

pub type StatementResult =
  Result(StatementState, String)

pub fn parse(tokens: List(Token)) -> StatementResult {
  // parse_expression(tokens)
  parse_statement(tokens)
}

fn parse_expression(tokens: List(Token)) -> ExpressionResult {
  parse_term(tokens)
}

fn parse_term(tokens: List(Token)) -> ExpressionResult {
  use #(left, rest) <- result.try(parse_factor(tokens))
  parse_term_helper(left, rest)
}

fn parse_term_helper(
  left: expr.Expression,
  tokens: List(Token),
) -> ExpressionResult {
  case tokens {
    [op, ..rest] if op.kind == token.Plus || op.kind == token.Minus -> {
      use #(right, rest2) <- result.try(parse_factor(rest))

      let expression =
        expr.Expression(
          kind: expr.BinaryOperator(op: op.kind, left: left, right: right),
          value_type: None,
        )
      parse_term_helper(expression, rest2)
    }
    _ -> Ok(#(left, tokens))
  }
}

fn parse_factor(tokens: List(Token)) -> ExpressionResult {
  use #(left, rest) <- result.try(parse_primary(tokens))
  parse_factor_helper(left, rest)
}

fn parse_factor_helper(
  left: expr.Expression,
  tokens: List(Token),
) -> ExpressionResult {
  case tokens {
    [op, ..rest] if op.kind == token.Star || op.kind == token.Slash -> {
      use #(right, rest2) <- result.try(parse_primary(rest))

      let expression =
        expr.Expression(
          expr.BinaryOperator(op: op.kind, left: left, right: right),
          value_type: None,
        )
      parse_factor_helper(expression, rest2)
    }
    _ -> Ok(#(left, tokens))
  }
}

pub fn parse_primary(tokens: List(Token)) -> ExpressionResult {
  case tokens {
    [tok, ..rest] -> {
      case tok.kind, tok.literal {
        token.Int, token.IntLiteral(num) -> {
          Ok(#(
            expr.Expression(expr.Int(num), value_type: Some(types.Int)),
            rest,
          ))
        }
        token.LeftParen, _ -> {
          use #(expression, rest) <- result.try(parse_expression(rest))
          case rest {
            [tok, ..rest] -> {
              case tok.kind {
                token.RightParen -> {
                  Ok(#(
                    expr.Expression(expr.Group(expression), value_type: None),
                    rest,
                  ))
                }
                _ -> Error("Expected right paren.")
              }
            }
            _ -> Error("Expected right paren, reached end of tokens.")
          }
        }
        token.Float, token.FloatLiteral(num) -> {
          Ok(#(
            expr.Expression(expr.Float(num), value_type: Some(types.Float)),
            rest,
          ))
        }
        _, _ -> {
          echo tok
          todo
        }
      }
    }
    _ -> todo
  }
}

pub fn parse_program(
  tokens: List(Token),
) -> Result(List(stmt.Statement), String) {
  use #(statements, tokens) <- result.try(parse_program_helper(tokens, []))
  Ok(statements |> list.reverse)
}

fn parse_program_helper(
  tokens: List(Token),
  statements: List(stmt.Statement),
) -> Result(#(List(stmt.Statement), List(Token)), String) {
  use #(statement, tokens) <- result.try(parse_statement(tokens))
  echo tokens
  case tokens {
    [] -> {
      Ok(#([statement, ..statements], []))
    }
    tokens -> {
      echo "helper matched tokens"
      echo tokens
      parse_program_helper(tokens, [statement, ..statements])
    }
  }
}

pub fn parse_statement(tokens: List(Token)) -> StatementResult {
  echo "statement"
  echo tokens
  case tokens {
    [tok, ..rest] -> {
      case tok.kind {
        token.LeftBrace -> {
          echo "parse block"
          parse_block(rest, [])
        }
        token.If -> {
          parse_if(rest)
        }
        _ -> {
          use #(expression, rest) <- result.try(parse_expression(tokens))
          Ok(#(stmt.Expression(expression: expression), rest))
        }
      }
    }
    _ -> todo
  }
}

fn parse_block(
  tokens: List(Token),
  statements: List(stmt.Statement),
) -> StatementResult {
  case tokens {
    [tok, ..rest] -> {
      case tok.kind {
        token.RightBrace -> Ok(#(stmt.Block(statements), rest))
        _ -> {
          use #(statement, rest) <- result.try(parse_statement(tokens))
          parse_block(rest, [statement, ..statements])
        }
      }
    }
    _ -> Error("Expected statement or '}', found EOF.")
  }
}

fn parse_if(tokens: List(Token)) -> StatementResult {
  use #(condition, tokens) <- result.try(parse_expression(tokens))
  use #(if_block, tokens) <- result.try(parse_statement(tokens))
  case tokens {
    [tok, ..rest] -> {
      case tok.kind {
        token.Else -> {
          use #(else_block, rest) <- result.try(parse_statement(rest))
          echo "after else"
          echo rest
          Ok(#(stmt.If(condition, if_block, Some(else_block)), rest))
        }
        _ -> {
          Ok(#(stmt.If(condition, if_block, option.None), tokens))
        }
      }
    }
    _ -> Ok(#(stmt.If(condition, if_block, option.None), tokens))
  }
}
