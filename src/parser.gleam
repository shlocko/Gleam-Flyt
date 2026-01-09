import gleam/option.{type Option, None, Some}
import gleam/result
import lexer/token.{type Token, type TokenType}
import parser/expression as expr
import parser/utils

pub type ParserState =
  #(List(Token), expr.Expression)

pub fn parse(tokens: List(Token)) -> Result(Nil, String) {
  todo
}

fn parse_term(tokens: List(Token)) -> Result(expr.Expression, String) {
  let left = parse_primary(tokens)

  todo
}

fn parse_term_rest(tokens: List(Token)) -> Option(expr.Expression) {
  case utils.peek_token(tokens) {
    Some(tok) if tok.kind == token.Plus || tok.kind == token.Minus -> {
      todo
      // this is where I need to implement iteration for precedence, replacing the while loop used in TSFlyt
    }
    _ -> todo
  }
}

pub fn parse_primary(tokens: List(Token)) -> Result(expr.Expression, String) {
  case tokens {
    [tok, ..rest] -> {
      case tok.kind, tok.literal {
        token.Int, token.IntLiteral(num) -> {
          Ok(expr.Int(num))
        }
        _, _ -> todo
      }
    }
    _ -> todo
  }
}
