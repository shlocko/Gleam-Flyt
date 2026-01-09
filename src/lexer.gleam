import gleam/int
import gleam/list
import gleam/result
import gleam/string
import lexer/token.{type Token, Token}
import lexer/utils

pub fn lex(
  // ata: (source, linenumber, token accumulator)
  data: #(String, Int, List(Token)),
) -> Result(#(String, Int, List(Token)), String) {
  case data.0 {
    // Keywords
    // Two Character Tokens
    // One Character Tokens
    "\n" <> rest -> lex(#(rest, { data.1 } + 1, data.2))
    "(" <> rest ->
      lex(add_token(rest, data.2, token.LeftParen, "(", token.None, data.1))
    ")" <> rest ->
      lex(add_token(rest, data.2, token.RightParen, ")", token.None, data.1))
    "+" <> rest ->
      lex(add_token(rest, data.2, token.Plus, "+", token.None, data.1))
    "-" <> rest ->
      lex(add_token(rest, data.2, token.Minus, "-", token.None, data.1))
    "*" <> rest ->
      lex(add_token(rest, data.2, token.Star, "*", token.None, data.1))
    "/" <> rest ->
      lex(add_token(rest, data.2, token.Slash, "/", token.None, data.1))

    _ -> {
      case string.first(data.0) {
        Ok(c) -> {
          let mask = #(utils.is_digit(c), utils.is_identifier_valid(c))
          case mask {
            #(True, _) -> {
              let #(num_str, rest) = consume_while(data.0, utils.is_digit)
              use num <- result.try(
                int.parse(num_str)
                |> result.map_error(fn(_) {
                  "Failed to parse numeric literal: " <> num_str
                }),
              )
              lex(add_token(
                rest,
                data.2,
                token.Int,
                num_str,
                token.IntLiteral(num),
                data.1,
              ))
            }
            #(_, True) -> {
              let #(ident, rest) =
                consume_while(data.0, utils.is_identifier_valid)
              lex(add_token(
                rest,
                data.2,
                token.Identifier,
                ident,
                token.None,
                data.1,
              ))
            }
            _ -> todo
          }
        }
        _ -> Ok(#("", data.1, data.2))
      }
    }
  }
}

fn consume_while(
  input: String,
  predicate: fn(String) -> Bool,
) -> #(String, String) {
  let #(acc, rest) =
    consume_while_helper(string.to_graphemes(input), [], predicate)
  #(acc, rest)
}

fn consume_while_helper(
  input: List(String),
  acc: List(String),
  predicate: fn(String) -> Bool,
) -> #(String, String) {
  case input {
    [c, ..rest] -> {
      case predicate(c) {
        True -> {
          consume_while_helper(rest, [c, ..acc], predicate)
        }
        False -> {
          let taken = string.join(list.reverse(acc), "")
          // echo rest
          let unused = string.join([c, ..rest], "")
          #(taken, unused)
        }
      }
    }
    [] -> #(string.join(list.reverse(acc), ""), "")
  }
}

fn add_token(
  rest: String,
  tokens: List(Token),
  kind: token.TokenType,
  lexeme: String,
  literal: token.TokenLiteral,
  line_number: Int,
) -> #(String, Int, List(Token)) {
  let tok =
    Token(
      kind: kind,
      literal: literal,
      lexeme: lexeme,
      line_number: line_number,
    )
  #(rest, line_number, [tok, ..tokens])
}
