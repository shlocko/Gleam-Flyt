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

  // Two Character
  EqualsEquals

  // Values
  Int
  Float
  String
  Identifier

  // Keywords
  Let
}

pub type Token {
  Token(
    kind: TokenType,
    literal: TokenLiteral,
    lexeme: String,
    line_number: Int,
  )
}
