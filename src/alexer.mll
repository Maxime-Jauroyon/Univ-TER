{
open Aparser
}

let int = ['0'-'9']
let comment = ['0'-'9' 'a'-'z' 'A'-'Z' '-' '.'] (* characters allowed in a comment *)
let spaces = [' ' '\t' '\n' '\r']
let double_quote = '\"'
let not_doube_quote = [^ '\"']
let idS = ['a'-'z' 'A'-'Z' '_'] (* first character allowed of an identifier *)
let id = ['0'-'9' 'a'-'z' 'A'-'Z' '_'] (* characters allowed after the first one of an identifier *)
let unit = "uM" | "mM"

rule main = parse
  | unit as u                                              { UNIT(u)} (* keeping the unit as a string since we don't use it *)
  | double_quote not_doube_quote* double_quote as i        { ID(Atypes.get_index i) } (* keeping the index of the ID in the table of names and not the string ! *)
  | idS id* as i                                           { ID(Atypes.get_index i) } (* same *)
  | int+ ('.' int+)? ("e" ("-"|"+") int+)? as v            { VALUE(v) } (* keeping the value as a string since we don't use it *)
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
  | _ as s                                                 { failwith ("unexpected character : " ^ (String.make 1 s)) }
