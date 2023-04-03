open Printf
open Atypes

(* setup for program launching with options *)
let print_version = ref false
let print_back = ref false
let print_graph = ref false
let input_files = ref []
let input_len = ref ""
let output_file = ref ""
let use_inhibitors = ref false

let set_input_file filename = input_files := filename :: !input_files

let speclist = [
  ("-o", Arg.Set_string output_file, "The output file (default: stdout)");
  ("-l", Arg.Set_string input_len, "The length of the path to find (1 to 6)");
  ("-t", Arg.Set print_graph, "Print the list of reactions and inhibitors linked to their reactions");
  ("-p", Arg.Set print_back, "Print the input document");
  ("-v", Arg.Set print_version, "Display the program's version");
  ("-i", Arg.Set use_inhibitors, "To discard inhibitors in the searching process")
]

let usage_msg = "automata [options] <filename>"

(* the program *)
let main =
  try
    Arg.parse speclist set_input_file usage_msg;
    (* print version of the program if asked for *)
    if !print_version then
      printf "version: 1.0.0\n"
    (* if we don't have the file to read, we print the usage message *)
    else if List.length !input_files <> 1 then
      Arg.usage speclist usage_msg
    else
      (* get the bool which tell if we have to search the graph *)
      let will_interpret_path = (String.equal !input_len "") = false in
      (* get the bool which tell if we have received an stdout *)
      let will_outpout_to_file = (String.equal !output_file "") = false in
      (* checking if we need to give an stdout (every print will be instead write to the given stdout) *)
      if will_outpout_to_file then
        begin
          (* opening the file (or creating it if it doesn't exist) on which we will write results *)
          let oc = open_out !output_file in
          (* parse the input file *)
          let aut = Autil.parse_automata (List.hd !input_files) in
          (* print back the input file if asked for *)
          Aprint.print_automata aut !print_back (will_interpret_path || !print_graph) ~oc:(oc) will_outpout_to_file ;
          (* stop the program if file format isn't right and will specify why *)
          Autil.automata_is_interpretable aut;
          (* create the graph (it's 2 tables) *)
          let (rt,it) = Atransitions.transitions_from_automata aut in
          (* print reactions and inhibtions from the 2 tables if asked for *)
          Aprint.print_tables rt it !print_graph will_interpret_path ~oc:(oc) will_outpout_to_file;
          (* checking if we were asked to search the graph *)
          if will_interpret_path then
            (* checking if we need to discard the inhibitors *)
            if !use_inhibitors then
              (* table like the one storing inhibitors for each reaction name BUT empty *)
              let empty_inhib_table = Array.make 65536 (ref []) in
              (* giving an empty table of inhibitors instead of the good one *)
              (* search the paths of the length given and print the reactions participating in them *)
              Ainterpreter.interpret_graph rt empty_inhib_table (int_of_string !input_len) ~oc:(oc) will_outpout_to_file
            else
              (* search the paths of the length given and print the reactions participating in them *)
              Ainterpreter.interpret_graph rt it (int_of_string !input_len) ~oc:(oc) will_outpout_to_file;
          close_out oc;
        end
      else
        (* same things but without giving an stdout *)
        let aut = Autil.parse_automata (List.hd !input_files) in
        Aprint.print_automata aut !print_back (will_interpret_path || !print_graph) will_outpout_to_file;
        Autil.automata_is_interpretable aut;
        let (rt,it) = Atransitions.transitions_from_automata aut in
        Aprint.print_tables rt it !print_graph will_interpret_path will_outpout_to_file;
        if will_interpret_path then
            if !use_inhibitors then
              let empty_inhib_table = Array.make 65536 (ref []) in
              Ainterpreter.interpret_graph rt empty_inhib_table (int_of_string !input_len) will_outpout_to_file;
            else
              Ainterpreter.interpret_graph rt it (int_of_string !input_len) will_outpout_to_file;
  with Failure message ->
    printf "Failure : %s\n" message;
    exit 1

let () = main
