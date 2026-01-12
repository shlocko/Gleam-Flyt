import gleam/int
import gleam/io
import gleam/list
import gleam/result
import gleam/string
import lexer
import lexer/utils
import parser
import type_checker

pub fn main() -> Result(Nil, String) {
  echo "_ => "
  echo utils.check_utf("_")
  case lexer.lex(#("1+2^(7*(2-3))", 0, [])) {
    Ok(#(str, _, tokens)) -> {
      let tokens = list.reverse(tokens)
      echo str
      echo tokens
      use #(expr, toks) <- result.try(parser.parse(tokens))
      echo type_checker.type_expression(expr)
      Ok(Nil)
    }
    Error(str) -> {
      io.println(str)
      Error(str)
    }
  }
}
