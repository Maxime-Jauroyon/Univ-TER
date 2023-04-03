open Atypes

(* create the graph (2 tables) from the automata *)
let rec transitions_from_reaction r rt it t = 
  match r with
  (* create the node of the reaction and add it to the table of reactions (nodes) *)
  | Reaction(i,l1,l2,c,v) -> 
    let new_node = (r,i,(transitions_from_non_nullable_mol_list l1),(transitions_from_non_nullable_mol_list l2),[],[]) in
    Array.set rt t (Some new_node); (rt,it,(t+1))
  (* add the inhibitor to the table of inhibitors *)
  | Inhibitor(i1,i2,v,u) -> Array.set it i1 (ref (i2 :: !(Array.get it i1))); (rt,it,t)
(* update the tables with the reaction (or inhibition) and continue until the end of the list *)
and transitions_from_non_nullable_reac_list li rt it t = 
  match li with
  | NonNullableReacList(r,l) -> let (new_rt,new_it,new_t) = transitions_from_reaction r rt it t in transitions_from_non_nullable_reac_list l new_rt new_it new_t
  | NonNullableReac(r) -> let (new_rt,new_it,new_t) = transitions_from_reaction r rt it t in (new_rt,new_it)
(* return the list of the ids of the molecules in the list *)
and transitions_from_non_nullable_mol_list = function
  | NonNullableMolList(i,l) -> i :: transitions_from_non_nullable_mol_list l
  | NonNullableMol(i) -> [i]
and transitions_from_automata = function
  | Automata(l) -> 
    (* table for the nodes *)
    let reac_table = Array.make !Atypes.reac_table_length None in
    (* table like the one storing names but store inhibitors for each reaction name *)
    let inhib_table = Array.make 65536 (ref []) in
    transitions_from_non_nullable_reac_list l reac_table inhib_table 0