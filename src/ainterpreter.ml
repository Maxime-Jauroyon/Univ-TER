open Printf
open Atypes


(* Print every node who start or end with requested enzymes *)
let rec string_of_reac_table rt t =
  if t >= Array.length rt then ""
  else
    let actual_v = Array.get rt t in
    match actual_v with
    | None -> ""
    | Some(n) -> string_of_node n ^ "" ^ string_of_reac_table rt (t+1)
and string_of_node n =
  let (r,_,_,_,ie,oe,i,f,_,_) = n in
  if (i || f) then
    Astring.string_of_reaction r ^ " | in_edges : " ^ string_of_edges ie ^ " | out_edges : " ^ string_of_edges oe ^ " | start : " ^ string_of_bool i ^ " | end : " ^ string_of_bool f ^ "\n"
  else
    ""
and string_of_edges e =
  let rec string_of_edges_rec e =
    match e with
    | [] -> ""
    | h::t -> string_of_int h ^ " " ^ string_of_edges_rec t
  in
  "[" ^ string_of_edges_rec e ^ "]"



let rec string_of_int_list l =
  match l with
  | [] -> ""
  | h::t -> string_of_int h ^ " " ^ string_of_int_list t




















let rec initialize_edges rt s p ie oe ?(iteration = 0) index =
  if iteration < !Atypes.reac_table_length then
    if iteration != index then
      let actual_v = Array.get rt iteration in
      match actual_v with
      | None -> failwith "error in initialize_edges"
      | Some(n) -> 
        let (_,_,su,pr,_,_,_,_,_,_) = n in
        if (List.exists (fun x -> (List.mem x p)) su) then
          if (List.exists (fun x -> (List.mem x s)) pr) then
            initialize_edges rt s p (iteration :: ie) (iteration :: oe) ~iteration:(iteration + 1) index
          else
            initialize_edges rt s p ie (iteration :: oe) ~iteration:(iteration + 1) index
        else
          if (List.exists (fun x -> (List.mem x s)) pr) then
            initialize_edges rt s p (iteration :: ie) oe ~iteration:(iteration + 1) index
          else
            initialize_edges rt s p ie oe ~iteration:(iteration + 1) index
    else
      initialize_edges rt s p ie oe ~iteration:(iteration + 1) index
  else
    (ie,oe)


let rec initialize_rt ?(iteration = 0) rt =
  if iteration < !Atypes.reac_table_length then
    let actual_v = Array.get rt iteration in
    match actual_v with
    | None -> failwith "error in initialize_rt"
    | Some(n) -> 
      let (r,name,s,p,ie,oe,i,f,vi,vf) = n in
      let (new_in_edges,new_out_edges) = initialize_edges rt s p [] [] iteration in
      let new_node = (r,name,s,p,new_in_edges,new_out_edges,i,f,vi,vf) in
      Array.set rt iteration (Some new_node);
      initialize_rt ~iteration:(iteration + 1) rt
  else
    rt







let rec initialize_start_end ?(iteration = 0) rt w_start w_end start_list end_list =
  if iteration < !Atypes.reac_table_length then
    let actual_v = Array.get rt iteration in
    match actual_v with
    | None -> failwith "error in initialize_start_end"
    | Some(n) -> 
      let (r,name,s,p,ie,oe,i,f,vi,vf) = n in
      let new_i = ref i in
      let new_start_list = ref start_list in
      let new_f = ref f in
      let new_end_list = ref end_list in
      if (List.mem (Atypes.get_index w_start) s) then
        begin
        new_i := true;
        new_start_list := iteration :: !new_start_list;
        end;
      if (List.mem (Atypes.get_index w_end) p) then
        begin
        new_f := true;
        new_end_list := iteration :: !new_end_list;
        end;
      let new_node = (r,name,s,p,ie,oe,!new_i,!new_f,vi,vf) in
      Array.set rt iteration (Some new_node);
      initialize_start_end ~iteration:(iteration + 1) rt w_start w_end !new_start_list !new_end_list
  else
    (rt,start_list,end_list)









let search_path rt it w_start w_end start_list end_list len =
  if len == 1 then
    (* find every index who are both in start_list and end_list *)
    let rec find_index l1 l2 =
      match l1 with
      | [] -> []
      | h::t -> if (List.mem h l2) then h :: find_index t l2 else find_index t l2 in
    let index_list = find_index start_list end_list in
    (* get rid of those who produce w_start or consum w_end *)
    let rec check_start_end l =
      match l with
      | [] -> []
      | h::t -> 
        let actual_v = Array.get rt h in
        match actual_v with
        | None -> failwith "error in search_path"
        | Some(n) -> 
          let (_,_,s,p,_,_,_,_,_,_) = n in
          if (List.mem (Atypes.get_index w_start) p) then
            check_start_end t
          else
            if (List.mem (Atypes.get_index w_end) s) then
              check_start_end t
            else
              h :: (check_start_end t) in
    check_start_end index_list
  else
    []
    








let interpret_graph rt it len ?(oc = stdout) will_outpout_to_file: unit =
  (* get the start and end enzymes from the user input *)
  let w_start = ref "" in
  let w_end = ref "" in
  printf "Enter the start enzyme: ";
  let () = w_start := read_line () in
  printf "Enter the end enzyme: ";
  let () = w_end := read_line () in
  printf "interpreting %s to %s with length %d :\n" !w_start !w_end len;
  let new_rt = initialize_rt rt in
  let (new_rt,start_list,end_list) = initialize_start_end new_rt !w_start !w_end [] [] in
  match len with
  | 0 -> printf "length must be greater than 0\n"
  | n -> 
    let sol = search_path new_rt it !w_start !w_end start_list end_list n in
    printf "found %d solutions\n" (List.length sol);
    printf "printing solutions : %s" (string_of_int_list sol);
    printf "done !\n\n";
    if will_outpout_to_file then
      begin
      fprintf oc "%s" (string_of_reac_table new_rt 0);
      end
    else
      printf "%s" (string_of_reac_table new_rt 0);
      (*printf "%s\n" (string_of_reac_table new_rt 0);*) (* Print every node who start or end with requested enzymes *)
      (*printf "start_list : %s\n" (string_of_int_list start_list); 
      printf "end_list : %s\n" (string_of_int_list end_list)*) (* Print the index of the start and end nodes *)
