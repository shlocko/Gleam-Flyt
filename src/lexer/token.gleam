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

  // 
  Int
  Float
  String
}

pub type Token {
  Token(
    kind: TokenType,
    literal: TokenLiteral,
    lexeme: String,
    line_number: Int,
  )
}
