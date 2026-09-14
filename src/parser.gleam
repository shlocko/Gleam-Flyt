import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import lexer/token.{type Token}
import modules
import parser/ast
import parser/utils.{
  type ExpressionResult, type Function, type Local, type ParserState,
  ParserState,
}
import type_checker/types

pub fn parse(
  tokens: List(Token),
) -> Result(#(List(ast.Expression), List(modules.ModulePath)), String) {
  parse_program(ParserState(tokens, []))
}

fn parse_expression(state: ParserState) -> ExpressionResult {
  parse_equality(state)
}

fn parse_equality(state: ParserState) -> ExpressionResult {
  use #(left, state) <- result.try(parse_term(state))
  parse_equality_helper(left, state)
}

fn parse_equality_helper(
  left: ast.Expression,
  state: ParserState,
) -> ExpressionResult {
  case state.tokens {
    [op, ..rest]
      if op.kind == token.EqualsEquals || op.kind == token.BangEquals
    -> {
      use #(right, state) <- result.try(parse_term(
        ParserState(..state, tokens: rest),
      ))

      let expression =
        ast.Expression(kind: ast.BinaryOperator(
          op: op.kind,
          left: left,
          right: right,
        ))
      parse_equality_helper(expression, state)
    }
    _ -> Ok(#(left, state))
  }
}

fn parse_term(state: ParserState) -> ExpressionResult {
  use #(left, state) <- result.try(parse_factor(state))
  parse_term_helper(left, state)
}

fn parse_term_helper(
  left: ast.Expression,
  state: ParserState,
) -> ExpressionResult {
  case state.tokens {
    [op, ..rest] if op.kind == token.Plus || op.kind == token.Minus -> {
      use #(right, state) <- result.try(parse_factor(
        ParserState(..state, tokens: rest),
      ))

      let expression =
        ast.Expression(kind: ast.BinaryOperator(
          op: op.kind,
          left: left,
          right: right,
        ))
      parse_term_helper(expression, state)
    }
    _ -> Ok(#(left, state))
  }
}

fn parse_factor(state: ParserState) -> ExpressionResult {
  use #(left, state) <- result.try(parse_primary(state))
  parse_factor_helper(left, state)
}

fn parse_factor_helper(
  left: ast.Expression,
  state: ParserState,
) -> ExpressionResult {
  case state.tokens {
    [op, ..rest] if op.kind == token.Star || op.kind == token.Slash -> {
      use #(right, state) <- result.try(parse_primary(
        ParserState(..state, tokens: rest),
      ))

      let expression =
        ast.Expression(ast.BinaryOperator(op: op.kind, left: left, right: right))
      parse_factor_helper(expression, state)
    }
    _ -> Ok(#(left, state))
  }
}

pub fn parse_primary(state: ParserState) -> ExpressionResult {
  case state.tokens {
    [tok, ..rest] -> {
      case tok.kind, tok.literal {
        token.Int, token.IntLiteral(num) -> {
          Ok(#(ast.Expression(ast.Int(num)), ParserState(..state, tokens: rest)))
        }
        token.LeftParen, _ -> {
          use #(expression, state) <- result.try(parse_expression(
            ParserState(..state, tokens: rest),
          ))
          case state.tokens {
            [tok, ..rest] -> {
              case tok.kind {
                token.RightParen -> {
                  Ok(#(
                    ast.Expression(ast.Group(expression)),
                    ParserState(..state, tokens: rest),
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
            ast.Expression(ast.Float(num)),
            ParserState(..state, tokens: rest),
          ))
        }
        token.If, _ -> {
          parse_if(ParserState(..state, tokens: rest))
        }
        token.LeftBrace, _ -> {
          parse_block([], ParserState(..state, tokens: rest))
        }
        token.Print, _ -> {
          use #(expression, state) <- result.try(parse_expression(
            ParserState(..state, tokens: rest),
          ))
          Ok(#(ast.Expression(ast.Print(expression)), state))
        }
        token.Let, _ -> {
          parse_let(ParserState(..state, tokens: rest))
        }
        token.Static, _ -> {
          parse_static(ParserState(..state, tokens: rest))
        }
        _, _ -> {
          echo tok
          todo as {
            "Parsing for this token not implemented: " <> string.inspect(tok)
          }
        }
      }
    }
    [] -> {
      Error("Unexpected EOF")
    }
  }
}

pub fn parse_program(
  state: ParserState,
) -> Result(#(List(ast.Expression), List(modules.ModulePath)), String) {
  use #(expressions, _tokens) <- result.try(parse_program_helper([], state))
  Ok(#(expressions |> list.reverse, []))
}

fn parse_program_helper(
  expressions: List(ast.Expression),
  state: ParserState,
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
  state: ParserState,
) -> ExpressionResult {
  case state.tokens {
    [tok, ..rest] -> {
      case tok.kind {
        token.RightBrace ->
          Ok(#(
            ast.Expression(ast.Block(expressions |> list.reverse)),
            ParserState(..state, tokens: rest),
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

fn parse_if(state: ParserState) -> ExpressionResult {
  use #(condition, state) <- result.try(parse_expression(state))
  use #(if_block, state) <- result.try(parse_expression(state))
  case state.tokens {
    [tok, ..rest] -> {
      case tok.kind {
        token.Else -> {
          use #(else_block, state) <- result.try(parse_expression(
            ParserState(..state, tokens: rest),
          ))
          Ok(#(
            ast.Expression(ast.If(condition, if_block, Some(else_block))),
            state,
          ))
        }
        _ -> {
          Ok(#(
            ast.Expression(ast.If(condition, if_block, option.None)),
            ParserState(..state, tokens: rest),
          ))
        }
      }
    }
    _ -> Ok(#(ast.Expression(ast.If(condition, if_block, option.None)), state))
  }
}

fn parse_let(state: ParserState) -> ExpressionResult {
  case state.tokens {
    [tok, ..rest] -> {
      let #(mutable, state) = case utils.check_token(state, token.Mut) {
        Some(#(_, state)) -> #(True, state)
        None -> #(False, state)
      }
      use #(identifier, state) <- result.try(utils.expect_token(
        state,
        token.Identifier,
      ))
      use #(_, state) <- result.try(utils.expect_token(state, token.Equals))
      use #(initializer, state) <- result.try(parse_expression(state))

      Ok(#(
        ast.Expression(kind: ast.Let(identifier, initializer, mutable)),
        state,
      ))
    }
    _ -> Error("Unexpected EOF")
  }
}

fn parse_static(state: ParserState) -> ExpressionResult {
  case state.tokens {
    [tok, ..rest] -> {
      let #(mutable, state) = case utils.check_token(state, token.Mut) {
        Some(#(_, state)) -> #(True, state)
        None -> #(False, state)
      }
      use #(identifier, state) <- result.try(utils.expect_token(
        state,
        token.Identifier,
      ))
      use #(_, state) <- result.try(utils.expect_token(state, token.Equals))
      use #(initializer, state) <- result.try(parse_expression(state))

      Ok(#(
        ast.Expression(kind: ast.Static(identifier, initializer, mutable)),
        state,
      ))
    }
    _ -> Error("Unexpected EOF")
  }
}

fn parse_type_annotation(state: ParserState) -> ExpressionResult {
  case state.tokens {
    [tok, ..rest] -> {
      case tok.kind {
        token.Identifier -> {
          todo
        }
        _ -> todo
      }
    }
    _ -> Error("Unexpected EOF")
  }
}
