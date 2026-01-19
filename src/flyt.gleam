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
  // use #(_, _, tokens) <- result.try(
  //   lexer.lex(#("if 1+1 {1+2} else if 2+1 {2+2} {8+8}", 0, [])),
  // )
  // let tokens = tokens |> list.reverse
  // echo tokens
  // echo parser.parse_program(tokens)
  case
    compile_and_run(
      "
print {if 1==2
  2+2
else if 1==2
  3+3
else
  4+4
1+8}
print (print 81)
        ",
    )
  {
    Ok(_) -> {
      Ok(Nil)
    }
    Error(str) -> {
      // io.println(str)
      panic as str
    }
  }
}

fn compile_and_run(source: String) -> Result(Nil, String) {
  use #(_, _, tokens) <- result.try(lexer.lex(#(source, 0, [])))

  let tokens = list.reverse(tokens)
  // echo tokens
  use stmts <- result.try(parser.parse(tokens))
  // echo stmts
  use checked <- result.try(type_checker.type_check_program(stmts))
  echo checked
  use program <- result.try(code_gen.compile_program(checked))
  echo program |> json.to_string
  let _ = simplifile.write("program.jef", program |> json.to_string)

  let _ = echo program
  Ok(Nil)
}
