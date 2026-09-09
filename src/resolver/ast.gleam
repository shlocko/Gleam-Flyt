import gleam/option.{type Option}
import lexer/token.{type TokenType}
import type_checker/types

pub type BindingId =
  Int

pub type ResolvedExpression {
  ResolvedExpression(kind: ResolvedExprKind)
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
  Static(identifier: BindingId, initializer: ResolvedExpression, mut: Bool)
}
