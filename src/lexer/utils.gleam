import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string

pub fn check_utf(c: String) -> Option(Int) {
  case string.to_utf_codepoints(c) {
    [codepoint] -> {
      Some(string.utf_codepoint_to_int(codepoint))
    }
    _ -> None
  }
}

pub fn intutf_to_char(n: Int) -> Option(String) {
  case string.utf_codepoint(n) {
    Ok(code) -> {
      Some(string.from_utf_codepoints([code]))
    }
    _ -> None
  }
}

pub fn is_digit(c: String) -> Bool {
  case string.to_utf_codepoints(c) {
    [codepoint] -> {
      let code = string.utf_codepoint_to_int(codepoint)
      code >= 48 && code <= 57
    }
    _ -> False
  }
}

pub fn is_alpha(c: String) -> Bool {
  case string.to_utf_codepoints(c) {
    [codepoint] -> {
      let code = string.utf_codepoint_to_int(codepoint)
      { code >= 65 && code <= 90 } || { code >= 97 && code <= 122 }
    }
    _ -> False
  }
}

pub fn is_alphanumeric(c: String) -> Bool {
  is_digit(c) || is_alpha(c)
}

pub fn is_identifier_valid(c: String) -> Bool {
  case string.to_utf_codepoints(c) {
    [codepoint] -> {
      let code = string.utf_codepoint_to_int(codepoint)
      // digit || alpha || _
      is_digit(c) || is_alpha(c) || code == 95
    }
    _ -> False
  }
}
