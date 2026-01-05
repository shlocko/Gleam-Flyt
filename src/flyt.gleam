import gleam/io
import gleam/list
import gleam/string
import lexer
import lexer/utils

pub fn main() -> Nil {
  let #(str, _, tokens) = lexer.lex(#("311/456\n(1+8)*2", 0, []))
  echo str
  echo tokens |> list.reverse
  Nil
}
