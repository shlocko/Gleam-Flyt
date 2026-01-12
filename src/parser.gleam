import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import lexer/token.{type Token, type TokenType}
import parser/expression as expr
import parser/utils
import type_checker/types

pub type ParserState =
  #(expr.Expression, List(Token))

pub type ParserResult =
  Result(ParserState, String)

pub fn parse(tokens: List(Token)) -> ParserResult {
  parse_expression(tokens)
}

fn parse_expression(tokens: List(Token)) -> ParserResult {
  parse_term(tokens)
}

fn parse_term(tokens: List(Token)) -> ParserResult {
  use #(left, rest) <- result.try(parse_factor(tokens))
  parse_term_helper(left, rest)
}

fn parse_term_helper(left: expr.Expression, tokens: List(Token)) -> ParserResult {
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

fn parse_factor(tokens: List(Token)) -> ParserResult {
  use #(left, rest) <- result.try(parse_exponential(tokens))
  parse_factor_helper(left, rest)
}

fn parse_factor_helper(
  left: expr.Expression,
  tokens: List(Token),
) -> ParserResult {
  case tokens {
    [op, ..rest] if op.kind == token.Star || op.kind == token.Slash -> {
      use #(right, rest2) <- result.try(parse_exponential(rest))

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

fn parse_exponential(tokens: List(Token)) -> ParserResult {
  use #(left, rest) <- result.try(parse_primary(tokens))
  parse_exponential_helper(left, rest)
}

fn parse_exponential_helper(
  left: expr.Expression,
  tokens: List(Token),
) -> ParserResult {
  case tokens {
    [op, ..rest] if op.kind == token.Caret -> {
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

pub fn parse_primary(tokens: List(Token)) -> ParserResult {
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
        _, _ -> todo
      }
    }
    _ -> todo
  }
}
