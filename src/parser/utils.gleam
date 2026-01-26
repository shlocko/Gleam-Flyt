import gleam/list
import gleam/option.{type Option, None, Some}
import lexer/token.{type Token}

pub fn peek_token(tokens: List(Token)) -> Option(Token) {
  case
    tokens
    |> list.first
  {
    Ok(tok) -> Some(tok)
    _ -> None
  }
}

pub fn consume_token(tokens: List(Token)) -> Option(#(Token, List(Token))) {
  case tokens {
    [tok, ..rest] -> Some(#(tok, rest))
    _ -> None
  }
}
