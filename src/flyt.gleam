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
  // echo "_ => "
  // echo utils.check_utf("_")
  case compile_and_run("3f*(1.1-0.3)/2f") {
    Ok(_) -> {
      Ok(Nil)
    }
    Error(str) -> {
      io.println(str)
      Error(str)
    }
  }
}

fn compile_and_run(source: String) -> Result(Nil, String) {
  use #(_, _, tokens) <- result.try(lexer.lex(#(source, 0, [])))

  let tokens = list.reverse(tokens)
  // echo tokens
  use #(expr, _leftover_tokens) <- result.try(parser.parse(tokens))
  use checked <- result.try(type_checker.type_expression(expr))
  use program <- result.try(code_gen.compile_program(checked))
  let _ = simplifile.write("program.jef", program |> json.to_string)

  let _ = echo program
  Ok(Nil)
}
