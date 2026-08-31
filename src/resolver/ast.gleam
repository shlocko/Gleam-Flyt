import gleam/option.{type Option}
import lexer/token.{type TokenType}
import type_checker/types

pub type ResolvedExpression {
  ResolvedExpression(kind: ResolvedExprKind, value_type: Option(types.FlytType))
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
