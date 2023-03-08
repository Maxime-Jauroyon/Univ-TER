open Printf
open Atypes

(* print the document from the automata *)
let print_automata (a: automata) (print: bool) (print_transitions: bool) ?(oc = stdout) will_outpout_to_file : unit =
  if print then
    if will_outpout_to_file then
      begin
        fprintf oc "%s" (Astring.string_of_automata a);
        if print && print_transitions then
          fprintf oc "\n\n";
      end
    else 
      begin
        printf "%s" (Astring.string_of_automata a);
        if print && print_transitions then
          printf "\n\n"
      end

(* print the document from the tables *)
let print_tables rt it (print: bool) (interpret_word: bool) ?(oc = stdout) will_outpout_to_file: unit =
  if print then
    if will_outpout_to_file then
      begin
        fprintf oc "Reactions :\n%s\nInhibiteurs :\n%s" (Astring.string_of_reac_table rt) (Astring.string_of_inhib_table it);
        if print && interpret_word then
          fprintf oc "\n\n";
      end
    else
      begin
        printf "Reactions :\n%s\nInhibiteurs :\n%s" (Astring.string_of_reac_table rt) (Astring.string_of_inhib_table it);
        if print && interpret_word then
          printf "\n\n"
      end