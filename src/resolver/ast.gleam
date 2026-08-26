import gleam/option.{type Option}
import lexer/token.{type TokenType}

pub type ResolvedExpression {
  ResolvedExpression(id: Int, kind: ResolvedExprKind)
}

pub type ResolvedExprKind {

  Int(Int)
  Float(Float)
  Identifier(String)
  BinaryOperator(
    op: TokenType,
    left: ResolvedExpression,
    right: ResolvedExpression,
  )
  Group(ResolvedExpression)
  If(
    condition: ResolvedExpression,
    if_block: ResolvedExpression,
    else_block: Option(ResolvedExpression),
  )
  Block(statements: List(ResolvedExpression))
  Print(ResolvedExpression)
  Let(identifier: token.Token, initializer: ResolvedExpression, mut: Bool)
}
