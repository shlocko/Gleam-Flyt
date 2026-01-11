pub type TokenLiteral {
  IntLiteral(Int)
  FloatLiteral(Float)
  StringLiteral(String)
  None
}

pub type TokenType {
  // Single Token
  Plus
  Minus
  Star
  Slash
  LeftParen
  RightParen
  Caret

  // 
  Int
  Float
  String
  Identifier
}

pub type Token {
  Token(
    kind: TokenType,
    literal: TokenLiteral,
    lexeme: String,
    line_number: Int,
  )
}
