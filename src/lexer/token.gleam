pub type TokenLiteral {
  IntLiteral(Int)
  FloatLiteral(Float)
  StringLiteral(String)
  None
}

pub type TokenType {
  // Single Character
  Plus
  Minus
  Star
  Slash
  LeftParen
  RightParen
  Equals
  LeftBrace
  RightBrace

  // Two Character
  EqualsEquals
  BangEquals

  // Values
  Int
  Float
  String
  Identifier

  // Keywords
  Let
  Mut
  If
  Else
  Print
}

pub type Token {
  Token(
    kind: TokenType,
    literal: TokenLiteral,
    lexeme: String,
    line_number: Int,
  )
}
