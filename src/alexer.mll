{
open Aparser
}

let int = ['0'-'9']
let comment = ['0'-'9' 'a'-'z' 'A'-'Z' '-' '.']
let spaces = [' ' '\t' '\n' '\r']
let double_quote = '\"'
let not_doube_quote = [^ '\"']
let idS = ['a'-'z' 'A'-'Z' '_']
let id = ['0'-'9' 'a'-'z' 'A'-'Z' '_']
let unit = "uM" | "mM"

rule main = parse
  | unit as u                                              { UNIT(u)}
  | double_quote not_doube_quote* double_quote as i        { ID(Atypes.get_index i) }
  | idS id* as i                                           { ID(Atypes.get_index i) }
  | int+ ('.' int+)? ("e" ("-"|"+") int+)? as v            { VALUE(v) }
  | "->"                                                   { PONCTUATOR_ARROW}
  | '-'                                                    { PONCTUATOR_DASH}
  | '|'                                                    { PONCTUATOR_SEPARATOR}
  | ','                                                    { PONCTUATOR_COMMA }
  | ':'                                                    { PONCTUATOR_COLON }
  | ';'                                                    { PONCTUATOR_SEMICOLON }
  | '+'                                                    { PONCTUATOR_ADD}
  | "// " comment* as c                                    { main lexbuf }
  | spaces                                                 { main lexbuf }
  | eof                                                    { EOF }
  | _ as s                                                 { failwith "unexpected character" }
