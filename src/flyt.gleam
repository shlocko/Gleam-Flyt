import gleam/int
import gleam/io
import gleam/list
import gleam/result
import gleam/string
import lexer
import lexer/utils

pub fn main() -> Result(Nil, String) {
  echo "_ => "
  echo utils.check_utf("_")
  case lexer.lex(#("311/456\n(1+8)*2+foo7_bar", 0, [])) {
    Ok(#(str, _, tokens)) -> {
      echo str
      echo tokens |> list.reverse
      Ok(Nil)
    }
    Error(str) -> {
      io.println(str)
      Error(str)
    }
  }
}
