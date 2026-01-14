import code_gen
import code_gen/types
import gleam/io
import gleam/json
import gleam/list
import gleam/result
import lexer
import lexer/utils
import parser
import simplifile
import type_checker

pub fn main() -> Result(Nil, String) {
  echo "_ => "
  echo utils.check_utf("_")
  case lexer.lex(#("(4+5-1)*2", 0, [])) {
    Ok(#(str, _, tokens)) -> {
      let tokens = list.reverse(tokens)
      echo str
      echo tokens
      use #(expr, _leftover_tokens) <- result.try(parser.parse(tokens))
      use checked <- result.try(type_checker.type_expression(expr))
      use program <- result.try(code_gen.compile_program(checked))
      simplifile.write("program.jef", program |> json.to_string)

      let _ = echo program
      Ok(Nil)
    }
    Error(str) -> {
      io.println(str)
      Error(str)
    }
  }
}
