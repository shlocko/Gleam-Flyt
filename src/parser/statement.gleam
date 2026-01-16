import parser/expression as expr

pub type Statement {
  If(condition: expr.Expression, if_block: Statement, else_block: Statement)
  Block(statements: List(Statement))
  Expression(expression: expr.Expression)
}
