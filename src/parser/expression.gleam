import lexer/token.{type TokenType, Token}

pub type Expression {
  Int(Int)
  Float(Float)
  Identifier(String)
  BinaryOperator(op: TokenType, left: Expression, right: Expression)
  Group(Expression)
}
