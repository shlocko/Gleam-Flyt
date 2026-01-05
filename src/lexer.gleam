import lexer/token.{type Token, Token}

pub fn lex(data: #(String, Int, List(Token))) -> #(String, Int, List(Token)) {
  case data.0 {
    "\n" <> rest -> lex(#(rest, { data.1 } + 1, data.2))
    "(" <> rest -> lex(add_token(rest, data.2, token.LeftParen, "(", data.1))
    ")" <> rest -> lex(add_token(rest, data.2, token.RightParen, ")", data.1))
    "+" <> rest -> lex(add_token(rest, data.2, token.Plus, "+", data.1))
    _ -> {
      data
    }
  }
}

fn add_token(
  rest: String,
  tokens: List(Token),
  kind: token.TokenType,
  lexeme: String,
  line_number: Int,
) -> #(String, Int, List(Token)) {
  let tok =
    Token(
      kind: kind,
      literal: token.None,
      lexeme: lexeme,
      line_number: line_number,
    )
  #(rest, line_number, [tok, ..tokens])
}
