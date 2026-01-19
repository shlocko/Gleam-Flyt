import gleam/list
import gleam/option.{None, Some}
import gleam/result
import lexer/token.{type Token}
import parser/ast
import type_checker/types

pub type ExpressionState =
  #(ast.Expression, List(Token))

pub type ExpressionResult =
  Result(ExpressionState, String)

pub fn parse(tokens: List(Token)) -> Result(List(ast.Expression), String) {
  // parse_expression(tokens)
  parse_program(tokens)
}

fn parse_expression(tokens: List(Token)) -> ExpressionResult {
  parse_equality(tokens)
}

fn parse_equality(tokens: List(Token)) -> ExpressionResult {
  use #(left, rest) <- result.try(parse_term(tokens))
  parse_equality_helper(left, rest)
}

fn parse_equality_helper(
  left: ast.Expression,
  tokens: List(Token),
) -> ExpressionResult {
  case tokens {
    [op, ..rest]
      if op.kind == token.EqualsEquals || op.kind == token.BangEquals
    -> {
      use #(right, rest2) <- result.try(parse_term(rest))

      let expression =
        ast.Expression(
          kind: ast.BinaryOperator(op: op.kind, left: left, right: right),
          value_type: None,
        )
      parse_equality_helper(expression, rest2)
    }
    _ -> Ok(#(left, tokens))
  }
}

fn parse_term(tokens: List(Token)) -> ExpressionResult {
  use #(left, rest) <- result.try(parse_factor(tokens))
  parse_term_helper(left, rest)
}

fn parse_term_helper(
  left: ast.Expression,
  tokens: List(Token),
) -> ExpressionResult {
  case tokens {
    [op, ..rest] if op.kind == token.Plus || op.kind == token.Minus -> {
      use #(right, rest2) <- result.try(parse_factor(rest))

      let expression =
        ast.Expression(
          kind: ast.BinaryOperator(op: op.kind, left: left, right: right),
          value_type: None,
        )
      parse_term_helper(expression, rest2)
    }
    _ -> Ok(#(left, tokens))
  }
}

fn parse_factor(tokens: List(Token)) -> ExpressionResult {
  use #(left, rest) <- result.try(parse_primary(tokens))
  parse_factor_helper(left, rest)
}

fn parse_factor_helper(
  left: ast.Expression,
  tokens: List(Token),
) -> ExpressionResult {
  case tokens {
    [op, ..rest] if op.kind == token.Star || op.kind == token.Slash -> {
      use #(right, rest2) <- result.try(parse_primary(rest))

      let expression =
        ast.Expression(
          ast.BinaryOperator(op: op.kind, left: left, right: right),
          value_type: None,
        )
      parse_factor_helper(expression, rest2)
    }
    _ -> Ok(#(left, tokens))
  }
}

pub fn parse_primary(tokens: List(Token)) -> ExpressionResult {
  case tokens {
    [tok, ..rest] -> {
      case tok.kind, tok.literal {
        token.Int, token.IntLiteral(num) -> {
          Ok(#(ast.Expression(ast.Int(num), value_type: Some(types.Int)), rest))
        }
        token.LeftParen, _ -> {
          use #(expression, rest) <- result.try(parse_expression(rest))
          case rest {
            [tok, ..rest] -> {
              case tok.kind {
                token.RightParen -> {
                  Ok(#(
                    ast.Expression(ast.Group(expression), value_type: None),
                    rest,
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
            rest,
          ))
        }
        token.If, _ -> {
          parse_if(rest)
        }
        token.LeftBrace, _ -> {
          parse_block(rest, [])
        }
        token.Print, _ -> {
          use #(expression, rest) <- result.try(parse_expression(rest))
          Ok(#(ast.Expression(ast.Print(expression), Some(types.Nil)), rest))
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
  tokens: List(Token),
) -> Result(List(ast.Expression), String) {
  use #(expressions, _tokens) <- result.try(parse_program_helper(tokens, []))
  Ok(expressions |> list.reverse)
}

fn parse_program_helper(
  tokens: List(Token),
  expressions: List(ast.Expression),
) -> Result(#(List(ast.Expression), List(Token)), String) {
  use #(expression, tokens) <- result.try(parse_expression(tokens))
  echo tokens
  case tokens {
    [] -> {
      Ok(#([expression, ..expressions], []))
    }
    tokens -> {
      echo "helper matched tokens"
      echo tokens
      parse_program_helper(tokens, [expression, ..expressions])
    }
  }
}

fn parse_block(
  tokens: List(Token),
  expressions: List(ast.Expression),
) -> ExpressionResult {
  case tokens {
    [tok, ..rest] -> {
      case tok.kind {
        token.RightBrace ->
          Ok(#(
            ast.Expression(ast.Block(expressions |> list.reverse), None),
            rest,
          ))
        _ -> {
          use #(expression, rest) <- result.try(parse_expression(tokens))
          parse_block(rest, [expression, ..expressions])
        }
      }
    }
    _ -> Error("Expected statement or '}', found EOF.")
  }
}

fn parse_if(tokens: List(Token)) -> ExpressionResult {
  use #(condition, tokens) <- result.try(parse_expression(tokens))
  use #(if_block, tokens) <- result.try(parse_expression(tokens))
  case tokens {
    [tok, ..rest] -> {
      case tok.kind {
        token.Else -> {
          use #(else_block, rest) <- result.try(parse_expression(rest))
          Ok(#(
            ast.Expression(ast.If(condition, if_block, Some(else_block)), None),
            rest,
          ))
        }
        _ -> {
          Ok(#(
            ast.Expression(ast.If(condition, if_block, option.None), None),
            tokens,
          ))
        }
      }
    }
    _ ->
      Ok(#(
        ast.Expression(ast.If(condition, if_block, option.None), None),
        tokens,
      ))
  }
}
