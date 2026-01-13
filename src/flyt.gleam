import code_gen
import gleam/io
import gleam/list
import gleam/result
import lexer
import lexer/utils
import parser
import type_checker

pub fn main() -> Result(Nil, String) {
  echo "_ => "
  echo utils.check_utf("_")
  case lexer.lex(#("1+2", 0, [])) {
    Ok(#(str, _, tokens)) -> {
      let tokens = list.reverse(tokens)
      echo str
      echo tokens
      use #(expr, _leftover_tokens) <- result.try(parser.parse(tokens))
      use checked <- result.try(type_checker.type_expression(expr))
      let generated_code = code_gen.generate_expression(checked, [], [])
      let _ = echo generated_code
      Ok(Nil)
    }
    Error(str) -> {
      io.println(str)
      Error(str)
    }
  }
}
