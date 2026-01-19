import gleam/option.{type Option}
import lexer/token.{type TokenType}
import type_checker/types

pub type Expression {
  Expression(kind: ExprKind, value_type: Option(types.FlytType))
}

pub type ExprKind {

  Int(Int)
  Float(Float)
  Identifier(String)
  BinaryOperator(op: TokenType, left: Expression, right: Expression)
  Group(Expression)
  If(
    condition: Expression,
    if_block: Expression,
    else_block: Option(Expression),
  )
  Block(statements: List(Expression))
  Print(Expression)
}
