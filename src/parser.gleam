import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import lexer/token.{type Token}
import parser/ast
import type_checker/types

pub type Global {
  Global(
    name: token.Token,
    value_type: Option(types.FlytType),
    initializer: Option(ast.Expression),
    mutable: Bool,
  )
}

pub type Local {
  Local(
    name: token.Token,
    value_type: Option(types.FlytType),
    initializer: Option(ast.Expression),
    mutable: Bool,
  )
}

pub type Function {
  Function(locals: List(Local))
}

pub type ExpressionState {
  ExpressionState(
    tokens: List(Token),
    globals: List(Global),
    functions: List(Function),
  )
}

pub type ExpressionResult =
  Result(#(ast.Expression, ExpressionState), String)

pub fn parse(tokens: List(Token)) -> Result(List(ast.Expression), String) {
  // parse_expression(tokens)
  parse_program(ExpressionState(tokens, [], []))
}

fn parse_expression(state: ExpressionState) -> ExpressionResult {
  parse_equality(state)
}

fn parse_equality(state: ExpressionState) -> ExpressionResult {
  use #(left, state) <- result.try(parse_term(state))
  parse_equality_helper(left, state)
}

fn parse_equality_helper(
  left: ast.Expression,
  state: ExpressionState,
) -> ExpressionResult {
  case state.tokens {
    [op, ..rest]
      if op.kind == token.EqualsEquals || op.kind == token.BangEquals
    -> {
      use #(right, state) <- result.try(parse_term(
        ExpressionState(..state, tokens: rest),
      ))

      let expression =
        ast.Expression(
          kind: ast.BinaryOperator(op: op.kind, left: left, right: right),
          value_type: None,
        )
      parse_equality_helper(expression, state)
    }
    _ -> Ok(#(left, state))
  }
}

fn parse_term(state: ExpressionState) -> ExpressionResult {
  use #(left, state) <- result.try(parse_factor(state))
  parse_term_helper(left, state)
}

fn parse_term_helper(
  left: ast.Expression,
  state: ExpressionState,
) -> ExpressionResult {
  case state.tokens {
    [op, ..rest] if op.kind == token.Plus || op.kind == token.Minus -> {
      use #(right, state) <- result.try(parse_factor(
        ExpressionState(..state, tokens: rest),
      ))

      let expression =
        ast.Expression(
          kind: ast.BinaryOperator(op: op.kind, left: left, right: right),
          value_type: None,
        )
      parse_term_helper(expression, state)
    }
    _ -> Ok(#(left, state))
  }
}

fn parse_factor(state: ExpressionState) -> ExpressionResult {
  use #(left, state) <- result.try(parse_primary(state))
  parse_factor_helper(left, state)
}

fn parse_factor_helper(
  left: ast.Expression,
  state: ExpressionState,
) -> ExpressionResult {
  case state.tokens {
    [op, ..rest] if op.kind == token.Star || op.kind == token.Slash -> {
      use #(right, state) <- result.try(parse_primary(
        ExpressionState(..state, tokens: rest),
      ))

      let expression =
        ast.Expression(
          ast.BinaryOperator(op: op.kind, left: left, right: right),
          value_type: None,
        )
      parse_factor_helper(expression, state)
    }
    _ -> Ok(#(left, state))
  }
}

pub fn parse_primary(state: ExpressionState) -> ExpressionResult {
  case state.tokens {
    [tok, ..rest] -> {
      case tok.kind, tok.literal {
        token.Int, token.IntLiteral(num) -> {
          Ok(#(
            ast.Expression(ast.Int(num), value_type: Some(types.Int)),
            ExpressionState(..state, tokens: rest),
          ))
        }
        token.LeftParen, _ -> {
          use #(expression, state) <- result.try(parse_expression(
            ExpressionState(..state, tokens: rest),
          ))
          case state.tokens {
            [tok, ..rest] -> {
              case tok.kind {
                token.RightParen -> {
                  Ok(#(
                    ast.Expression(ast.Group(expression), value_type: None),
                    ExpressionState(..state, tokens: rest),
                  ))
                }
                _ -> Error("Expected right paren.")
              }
            }
            _ -> Error("Expected right paren, reached end of tokens.")
          }
        }
        token.Float, token.FloatLiteral(num) -> {
          Ok(#(
            ast.Expression(ast.Float(num), value_type: Some(types.Float)),
            ExpressionState(..state, tokens: rest),
          ))
        }
        token.If, _ -> {
          parse_if(ExpressionState(..state, tokens: rest))
        }
        token.LeftBrace, _ -> {
          parse_block([], ExpressionState(..state, tokens: rest))
        }
        token.Print, _ -> {
          use #(expression, state) <- result.try(parse_expression(
            ExpressionState(..state, tokens: rest),
          ))
          Ok(#(ast.Expression(ast.Print(expression), None), state))
        }
        token.Let, _ -> {
          parse_let(ExpressionState(..state, tokens: rest))
        }
        _, _ -> {
          echo tok
          todo
        }
      }
    }
    _ -> todo
  }
}

pub fn parse_program(
  state: ExpressionState,
) -> Result(List(ast.Expression), String) {
  use #(expressions, _tokens) <- result.try(parse_program_helper([], state))
  Ok(expressions |> list.reverse)
}

fn parse_program_helper(
  expressions: List(ast.Expression),
  state: ExpressionState,
) -> Result(#(List(ast.Expression), List(Token)), String) {
  use #(expression, state) <- result.try(parse_expression(state))
  echo state.tokens
  case state.tokens {
    [] -> {
      Ok(#([expression, ..expressions], []))
    }
    tokens -> {
      echo "helper matched tokens"
      echo tokens
      parse_program_helper([expression, ..expressions], state)
    }
  }
}

fn parse_block(
  expressions: List(ast.Expression),
  state: ExpressionState,
) -> ExpressionResult {
  case state.tokens {
    [tok, ..rest] -> {
      case tok.kind {
        token.RightBrace ->
          Ok(#(
            ast.Expression(ast.Block(expressions |> list.reverse), None),
            ExpressionState(..state, tokens: rest),
          ))
        _ -> {
          use #(expression, state) <- result.try(parse_expression(state))
          parse_block([expression, ..expressions], state)
        }
      }
    }
    _ -> Error("Expected statement or '}', found EOF.")
  }
}

fn parse_if(state: ExpressionState) -> ExpressionResult {
  use #(condition, state) <- result.try(parse_expression(state))
  use #(if_block, state) <- result.try(parse_expression(state))
  case state.tokens {
    [tok, ..rest] -> {
      case tok.kind {
        token.Else -> {
          use #(else_block, state) <- result.try(parse_expression(
            ExpressionState(..state, tokens: rest),
          ))
          Ok(#(
            ast.Expression(ast.If(condition, if_block, Some(else_block)), None),
            state,
          ))
        }
        _ -> {
          Ok(#(
            ast.Expression(ast.If(condition, if_block, option.None), None),
            ExpressionState(..state, tokens: rest),
          ))
        }
      }
    }
    _ ->
      Ok(#(
        ast.Expression(ast.If(condition, if_block, option.None), None),
        state,
      ))
  }
}

fn parse_let(state: ExpressionState) -> ExpressionResult {
  case state.tokens {
    [tok, ..rest] -> {
      case tok.kind {
        token.Identifier -> todo
        token.Mut -> todo
        _ -> todo
      }
    }
    _ -> todo
  }
}
