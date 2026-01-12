import gleam/option.{type Option, None, Some}
import lexer/token.{type TokenType, Token}
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
}
