import gleam/list
import gleam/option.{type Option, None, Some}
import lexer/token.{type Token}
import parser/ast
import type_checker/types

pub type Global {
  Global(
    name: token.Token,
    value_type: Option(types.FlytType),
    initializer: Option(ast.Expression),
    mutable: Bool,
  )
}

pub type Local {
  Local(
    name: token.Token,
    value_type: Option(types.FlytType),
    initializer: Option(ast.Expression),
    mutable: Bool,
  )
}

pub type Function {
  Function(locals: List(Local))
}

pub type ParserState {
  ParserState(
    tokens: List(Token),
    globals: List(Global),
    functions: List(Function),
  )
}

pub type ExpressionResult =
  Result(#(ast.Expression, ParserState), String)

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

pub fn check_token(
  state: ParserState,
  expected: token.TokenType,
) -> Option(#(token.Token, ParserState)) {
  case state.tokens {
    [tok, ..rest] -> {
      case tok {
        _ if tok.kind == expected ->
          Some(#(tok, ParserState(..state, tokens: rest)))
        _ -> None
      }
    }
    [] -> None
  }
}

pub fn expect_token(
  state: ParserState,
  expected: token.TokenType,
) -> Result(#(token.Token, ParserState), String) {
  case state.tokens {
    [tok, ..rest] -> {
      case tok {
        _ if tok.kind == expected ->
          Ok(#(tok, ParserState(..state, tokens: rest)))
        _ -> Error("Found token: " <> tok.lexeme)
      }
    }
    [] -> Error("Expected token, encounter EOF")
  }
}
