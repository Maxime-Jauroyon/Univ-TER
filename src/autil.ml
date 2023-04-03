open Atypes

(* lex then parse the file *)
(* filename : name of the file to read the data from *)
(* output : the automata being the parsed file *)
let parse_automata (filename: string): automata =
  let lexbuf = Lexing.from_channel (open_in filename) in
  Aparser.automata Alexer.main lexbuf


(* check if the automata is interpretable *)
(* a : the automata to check *)
(* output : nothing if the automata is interpretable, failwith an error message otherwise *)
let automata_is_interpretable (a: automata): unit =

  let rec interpretibility_of_reaction r =
    match r with
    | Reaction(i,l1,l2,c,v) -> 
      (* get the number of substrat *)
      let l1_length = length_of_non_nullable_mol_list l1  0 in
      (* get the number of concentration *)
      let c_length = length_of_non_nullable_conc_list c 0 in
      (* if we don't have the same number of substrat and concentration it's not conform to the format *)
      if l1_length != c_length then
        let err = "this reaction isn't interpretable :\n" ^ (Astring.string_of_reaction r) ^ "\n" in
        failwith err
    | Inhibitor(i1,i2,v,u) -> ()
  and interpretibility_of_non_nullable_reac_list = function
    | NonNullableReacList(r,l) -> interpretibility_of_reaction r; interpretibility_of_non_nullable_reac_list l
    | NonNullableReac(r) -> interpretibility_of_reaction r
  and length_of_non_nullable_mol_list li t =
    match li with
    | NonNullableMolList(i,l) -> length_of_non_nullable_mol_list l (t+1)
    | NonNullableMol(i) -> (t+1)
  and length_of_non_nullable_conc_list li t =
    match li with
    | NonNullableConcList(v,u,l) -> length_of_non_nullable_conc_list l (t+1)
    | NonNullableConc(v,u) -> (t+1)
  and interpretibility_of_automata = function
    | Automata(l) -> interpretibility_of_non_nullable_reac_list l in

  interpretibility_of_automata a