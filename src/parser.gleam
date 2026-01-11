import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import lexer/token.{type Token, type TokenType}
import parser/expression as expr
import parser/utils

pub type ParserState =
  #(expr.Expression, List(Token))

pub type ParserResult =
  Result(ParserState, String)

pub fn parse(tokens: List(Token)) -> ParserResult {
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
        expr.BinaryOperator(op: op.kind, left: left, right: right)
      parse_term_helper(expression, rest2)
    }
    _ -> Ok(#(left, tokens))
  }
}

fn parse_factor(tokens: List(Token)) -> ParserResult {
  use #(left, rest) <- result.try(parse_primary(tokens))
  parse_factor_helper(left, rest)
}

fn parse_factor_helper(
  left: expr.Expression,
  tokens: List(Token),
) -> ParserResult {
  case tokens {
    [op, ..rest] if op.kind == token.Star || op.kind == token.Slash -> {
      use #(right, rest2) <- result.try(parse_primary(rest))

      let expression =
        expr.BinaryOperator(op: op.kind, left: left, right: right)
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
          Ok(#(expr.Int(num), rest))
        }
        _, _ -> todo
      }
    }
    _ -> todo
  }
}
