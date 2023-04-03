open Printf
open Atypes

(* print the document from the automata *)
(* a : the automata *)
(* print : bool telling if we have to print *)
(* print_transitions : bool telling if we are going to print after this (for printing purpose) *)
(* oc : the output channel *)
(* will_outpout_to_file : bool telling if we are going to output to a file *)
(* output : none, just printing if asked for *)
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
(* rt : the reaction table *)
(* it : the inhibitors table *)
(* print : bool telling if we have to print *)
(* interpret_word : bool telling if we are going to print after this (for printing purpose) *)
(* oc : the output channel *)
(* will_outpout_to_file : bool telling if we are going to output to a file *)
(* output : none, just printing if asked for *)
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