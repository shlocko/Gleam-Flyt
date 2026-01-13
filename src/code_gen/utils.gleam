import code_gen/types as gen_types
import gleam/list

pub fn find_or_push(
  val: gen_types.JEFValue,
  items: List(gen_types.JEFValue),
) -> #(Int, List(gen_types.JEFValue)) {
  case find_index(val, items) {
    Ok(idx) -> {
      #(idx, items)
    }
    Error(_) -> {
      let new_items = [val, ..items]
      #(list.length(items), new_items)
    }
  }
}

pub fn find_index(
  val: gen_types.JEFValue,
  items: List(gen_types.JEFValue),
) -> Result(Int, Nil) {
  let res = find_index_helper(val, items, 0)
  case res {
    Ok(#(_, idx)) -> {
      Ok(idx)
    }
    Error(_) -> Error(Nil)
  }
}

fn find_index_helper(
  val: gen_types.JEFValue,
  items: List(gen_types.JEFValue),
  idx: Int,
) -> Result(#(List(gen_types.JEFValue), Int), Nil) {
  case items {
    [] -> Error(Nil)
    [first, ..rest] if first == val -> {
      Ok(#(rest, idx))
    }
    [_, ..rest] -> {
      find_index_helper(val, rest, idx + 1)
    }
  }
}
