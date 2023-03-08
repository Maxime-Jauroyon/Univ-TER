(* table used to store molecule names accessed by hash function *)
let name_table = Array.make 65536 None

(* given a molecule name, return the index of the table where she his (or has been stored) *)
let rec get_index ?(iteration = 0) (s:string) = 
  (* 2^16 = 65536 and 19 are both primal number so we will see every spot of the table possible *)
  let index = (Hashtbl.hash s + (19 * iteration)) mod 65536 in
  let actual_v = Array.get name_table index in 
  match actual_v with 
    (* empty spot so we store it and return the index where we stored it *)
    | None -> Array.set name_table index (Some s); index
    | Some(actual_s) -> 
      (* if we found it we return the index where she is *)
      if String.equal s actual_s then 
        index
      else 
        (* if we didn't find it we try again with a new index *)
        get_index ~iteration:(iteration+1) s

(* given an index, return the molecule name *)
let get_name index = 
  let actual_v = Array.get name_table index in 
  match actual_v with 
    | None -> failwith "unregistered name"
    | Some(actual_s) -> actual_s

(* store the number of reactions we have seen while parsing *)
let reac_table_length = ref 0



(* type of the automata we get from the parsing *)
type reaction =
  | Reaction of int * non_nullable_mol_list * non_nullable_mol_list * non_nullable_conc_list * string
  | Inhibitor of int * int * string * string
and non_nullable_reac_list = 
  | NonNullableReacList of reaction * non_nullable_reac_list
  | NonNullableReac of reaction
and non_nullable_mol_list =
  | NonNullableMolList of int * non_nullable_mol_list
  | NonNullableMol of int
and non_nullable_conc_list =
  | NonNullableConcList of string * string * non_nullable_conc_list
  | NonNullableConc of string * string
and automata =
  | Automata of non_nullable_reac_list



(* type of the nodes we create for each reactions when building the graph *)
(*          reaction   name  substrat     product      in_edges     out_edges *)
type node = reaction * int * (int list) * (int list) * (int list) * (int list) 