import gleam/io
import gleam/list
import gleam/string
import lexer
import lexer/utils

pub fn main() -> Nil {
  let #(str, _, tokens) = lexer.lex(#("(+)\n++)", 0, []))
  echo str
  echo tokens |> list.reverse
  Nil
}
