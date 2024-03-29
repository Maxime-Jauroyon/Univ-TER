open Atypes

(* return a string representing the document from automata *)
let rec string_of_reaction = function
  | Reaction(i,l1,l2,c,v) -> get_name i ^ " : " ^ string_of_non_nullable_mol_list l1 ^ " -> " ^ string_of_non_nullable_mol_list l2 ^ " | " ^ string_of_non_nullable_conc_list c ^ " - " ^ v ^ ";"
  | Inhibitor(i1,i2,v,u) -> get_name i1 ^ " : " ^ get_name i2 ^ " | " ^ v ^ " " ^ u ^ ";"
and string_of_non_nullable_reac_list = function
  | NonNullableReacList(r,l) -> string_of_reaction r ^ "\n" ^ string_of_non_nullable_reac_list l
  | NonNullableReac(r) -> string_of_reaction r ^ "\n"
and string_of_non_nullable_mol_list = function
  | NonNullableMolList(i,l) -> get_name i ^ " + " ^ string_of_non_nullable_mol_list l
  | NonNullableMol(i) -> get_name i
and string_of_non_nullable_conc_list = function
  | NonNullableConcList(v,u,l) -> v ^ " " ^ u ^ ", " ^ string_of_non_nullable_conc_list l
  | NonNullableConc(v,u) -> v ^ " " ^ u
and string_of_automata = function
  | Automata(l) -> string_of_non_nullable_reac_list l






(* string of the list of inhibitors *)
(* b : bool to know if it's the first inhib of the list (for printing purpose) *)
(* l : list of inhibitors indexes *)
(* output : a string representing the list of inhibitors names *)
let rec string_of_list ?(b = true) l = 
  match l with
  | [] -> "\n"
  | i::l ->if b then get_name i ^ string_of_list ~b:(false) l else ", " ^ get_name i ^ string_of_list ~b:(false) l

(* string of the table of nodes *)
(* t : iterator over the table *)
(* rt : table of nodes (reactions) *)
(* output : a string representing the list of reactions *)
let rec string_of_reac_table ?(t = 0) rt =
  if t >= Array.length rt then ""
  else
    let actual_v = Array.get rt t in
    match actual_v with
    | None -> ""
    | Some(n) -> string_of_node n ^ "\n" ^ string_of_reac_table ~t:(t+1) rt
(* to get the string of the reaction *)
and string_of_node n =
  let (r,_,_,_,_,_) = n in
  string_of_reaction r

(* string of the table of inhibitors *)
(* t : iterator over the table *)
(* it : table of inhibitors *)
(* output : a string representing the list of inhibitors *)
let rec string_of_inhib_table ?(t = 0) it =
  if t >= Array.length it then ""
  else
    let actual_v = Array.get it t in
    match !actual_v with
    | [] -> string_of_inhib_table ~t:(t+1) it 
    | _ -> get_name t ^ " : " ^ string_of_list !actual_v ^ string_of_inhib_table ~t:(t+1) it








(* string of the solutions nodes *)
(* t : iterator over the table *)
(* rt : table of nodes (reactions) *)
(* sol : list of solutions indexes *)
(* output : a string representing the list of solutions reactions *)
let rec string_of_reac_table_sol rt ?(t = 0) sol =
  if t >= Array.length rt then ""
  else
    if (List.mem t sol) then
      let actual_v = Array.get rt t in
      match actual_v with
      | None -> ""
      | Some(n) -> string_of_node n ^ "\n" ^ string_of_reac_table_sol rt ~t:(t+1) sol
    else
      string_of_reac_table_sol rt ~t:(t+1) sol


(* string of the int list (used for debuging) *)
(* l : int list *)
(* output : a string representing the list of int*)
let rec string_of_int_list l =
  match l with
  | [] -> ""
  | h::t -> string_of_int h ^ " " ^ string_of_int_list t