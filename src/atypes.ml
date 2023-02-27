let name_table = Array.make 65536 None

let rec get_index ?(iteration = 0) (s:string) = 
  let index = (Hashtbl.hash s + (19 * iteration)) mod 65536 in
  let actual_v = Array.get name_table index in 
  match actual_v with 
    | None -> Array.set name_table index (Some s); index
    | Some(actual_s) -> 
      if String.equal s actual_s then 
        index
      else 
        get_index ~iteration:(iteration+1) s

let get_name index = 
  let actual_v = Array.get name_table index in 
  match actual_v with 
    | None -> failwith "unregistered name"
    | Some(actual_s) -> actual_s

let reac_table_length = ref 0

let inhib_table_length = ref 0




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




(*          reaction   name  substrat     product      in_edges     out_edges   initial final  visited_by_initial visited_by_final *)
type node = reaction * int * (int list) * (int list) * (int list) * (int list) * bool * bool * int *              int