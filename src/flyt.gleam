import gleam/int
import gleam/io
import gleam/list
import gleam/result
import gleam/string
import lexer
import lexer/utils
import parser

pub fn main() -> Result(Nil, String) {
  echo "_ => "
  echo utils.check_utf("_")
  case lexer.lex(#("311", 0, [])) {
    Ok(#(str, _, tokens)) -> {
      echo str
      echo tokens |> list.reverse
      echo parser.parse_primary(tokens)
      Ok(Nil)
    }
    Error(str) -> {
      io.println(str)
      Error(str)
    }
  }
}
