open Printf
open Atypes


let rec remove_duplicates s l =
  match l with
  | [] -> []
  | h::t -> if (List.mem h s) then remove_duplicates s t else (h::(remove_duplicates (h::s) t))


(* gives in_edges and out_edges for a list of substrat and product *)
let rec initialize_edges rt s p ie oe ?(iteration = 0) index =
  if iteration < !Atypes.reac_table_length then
    if iteration != index then
      let actual_v = Array.get rt iteration in
      match actual_v with
      | None -> failwith "error in initialize_edges"
      | Some(n) -> 
        let (_,_,su,pr,_,_) = n in
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


(* initialize the edges of every node *)
let rec initialize_rt ?(iteration = 0) rt =
  if iteration < !Atypes.reac_table_length then
    let actual_v = Array.get rt iteration in
    match actual_v with
    | None -> failwith "error in initialize_rt"
    | Some(n) -> 
      let (r,name,s,p,ie,oe) = n in
      let (new_in_edges,new_out_edges) = initialize_edges rt s p [] [] iteration in
      let new_node = (r,name,s,p,new_in_edges,new_out_edges) in
      Array.set rt iteration (Some new_node);
      initialize_rt ~iteration:(iteration + 1) rt
  else
    rt


(* initialize the start and end list of nodes *)
let rec initialize_start_end ?(iteration = 0) rt w_start w_end start_list end_list =
  if iteration < !Atypes.reac_table_length then
    let actual_v = Array.get rt iteration in
    match actual_v with
    | None -> failwith "error in initialize_start_end"
    | Some(n) -> 
      let (r,name,s,p,ie,oe) = n in
      let new_start_list = ref start_list in
      let new_end_list = ref end_list in
      if (List.mem (Atypes.get_index w_start) s) then
        new_start_list := iteration :: !new_start_list;
      if (List.mem (Atypes.get_index w_end) p) then
        new_end_list := iteration :: !new_end_list;
      let new_node = (r,name,s,p,ie,oe) in
      Array.set rt iteration (Some new_node);
      initialize_start_end ~iteration:(iteration + 1) rt w_start w_end !new_start_list !new_end_list
  else
    (rt,start_list,end_list)























(* search all the path in the graph for a given length *)
let search_path rt it w_start w_end start_list end_list len =

  (* get rid of those who produce w_end *)
  let rec check_being_end l =
    match l with
    | [] -> []
    | h::t -> 
      let actual_v = Array.get rt h in
      match actual_v with
      | None -> failwith "error in search_path"
      | Some(n) -> 
        let (_,_,_,p,_,_) = n in
        if (List.mem (Atypes.get_index w_end) p) then
          check_being_end t
        else
          h :: (check_being_end t) in

  (* get rid of those who produce w_start or consum w_end *)
  let rec check_start_end l =
    match l with
    | [] -> []
    | h::t -> 
      let actual_v = Array.get rt h in
      match actual_v with
      | None -> failwith "error in search_path"
      | Some(n) -> 
        let (_,_,s,p,_,_) = n in
        if (List.mem (Atypes.get_index w_start) p) then
          check_start_end t
        else
          if (List.mem (Atypes.get_index w_end) s) then
            check_start_end t
          else
            h :: (check_start_end t) in

  (* check if products appears in the list of current inhibitors *)
  let rec is_in_inhibitor p l =
    match p with
    | [] -> false
    | h::t -> 
      let rec is_in_inhibitor_aux p l =
        match l with
        | [] -> false
        | h::t -> if (List.mem p !(Array.get it h)) then true else is_in_inhibitor_aux p t in 
      if (is_in_inhibitor_aux h l) then true else is_in_inhibitor t l in

  (* get rid of those who are being inhib by product list *)
  let rec check_inhib_by_product l p=
  match l with
  | [] -> []
  | h::t -> 
    let actual_v = Array.get rt h in
    match actual_v with
    | None -> failwith "error in search_path"
    | Some(n) -> 
      let (_,i,_,_,_,_) = n in
      if (is_in_inhibitor p [i]) then
        check_inhib_by_product t p
      else
        h :: (check_inhib_by_product t p) in

  (* get rid of those who produce an inhibitor of themselves or reactions list *)
  let rec check_inhib_by_itself_or_others l l2=
  match l with
  | [] -> []
  | h::t -> 
    let actual_v = Array.get rt h in
    match actual_v with
    | None -> failwith "error in search_path"
    | Some(n) -> 
      let (_,i,_,p,_,_) = n in
      if (is_in_inhibitor p (i::l2)) then
        check_inhib_by_itself_or_others t l2
      else
        h :: (check_inhib_by_itself_or_others t l2) in









          







  (* find every index who are both in l1 and l2 *)
  let rec find_index_1 l1 l2 =
    match l1 with
    | [] -> []
    | h::t -> 
      if (List.mem h l2) then
        (* get rid of those who produce w_start or consum w_end (very unlikely) *)
        if ((check_start_end [h]) == []) then
          find_index_1 t l2
        else
          (* get rid of those who produce an inhibitor of themselves (very unlikely) *)
          if ((check_inhib_by_itself_or_others [h] []) == []) then
            find_index_1 t l2
          else
            h :: find_index_1 t l2 
      else find_index_1 t l2 in
  








  (* for each l1 index find the out_edges that connect with l2 *)
  let rec find_index_2 l1 l2 =
    match l1 with
    | [] -> []
    | h::t -> 
      let actual_v = Array.get rt h in
      match actual_v with
      | None -> failwith "error in search_path"
      | Some(n) -> 
        let (_,i,s,p,_,oe) = n in
        let rec find_index_aux l1 l2 =
          match l1 with
          | [] -> []
          | h::t -> if (List.mem h l2) then h :: find_index_aux t l2 else find_index_aux t l2 in
        let neighbors_index_list = find_index_aux oe l2 in
        if (neighbors_index_list == []) then
          find_index_2 t l2
        else
          (* check if start node produce w_start or consum w_end
          OR check if start node produce w_end *)
          if (((check_start_end [h]) == []) || ((check_being_end [h]) == [])) then
            find_index_2 t l2
          else
            (* get rid of end nodes who produce w_start or consum w_end *)
            let neighbors_index_list = check_start_end neighbors_index_list in
            if (neighbors_index_list == []) then
              find_index_2 t l2
            else
              (* check if start node produce an inhibitor of himself (very unlikely) *)
              if ((check_inhib_by_itself_or_others [h] []) == []) then
                find_index_2 t l2
              else
                (* get rid of those who are being inhib by start node products *)
                let neighbors_index_list = check_inhib_by_product neighbors_index_list p in
                (* get rid of those who produce an inhibitor of themselves or start node *)
                let neighbors_index_list = check_inhib_by_itself_or_others neighbors_index_list [i] in
                if (neighbors_index_list = []) then
                  find_index_2 t l2
                else
                  (h::neighbors_index_list) @ (find_index_2 t l2) in
















  (* for each l1 index find the nodes that connects with l2 *)
  let rec find_index_3 l1 l2 li lp l2_is_end =
    match l1 with
    | [] -> []
    | h::t -> 
      let actual_v = Array.get rt h in
      match actual_v with
      | None -> failwith "error in search_path"
      | Some(n) -> 
        let (_,i,s,p,_,oe) = n in
        let rec find_index_edges l1 l2 =
          match l1 with
          | [] -> []
          | h::t -> 
            let actual_v = Array.get rt h in
            match actual_v with
            | None -> failwith "error in search_path"
            | Some(n) -> 
              let (_,i,s,p,_,oe) = n in
              let rec find_index_aux l1 l2 =
                match l1 with
                | [] -> []
                | h::t -> if (List.mem h l2) then h :: find_index_aux t l2 else find_index_aux t l2 in
              let neighbors_index_list = find_index_aux oe l2 in
              if (neighbors_index_list == []) then
                find_index_edges t l2
              else
                (h,neighbors_index_list) :: find_index_edges t l2 in
        (* for each neighbor of start node we associate it's neighbor who are end nodes *)
        let neighbors_index_edges = find_index_edges oe l2 in
        if (neighbors_index_edges == []) then
          find_index_3 t l2 li lp l2_is_end
        else
          let rec get_nodes l1 =
            match l1 with
            | [] -> []
            | (h1,h2)::t -> h1 :: get_nodes t in
          (* neighbors of the start node *)
          let neighbors_index_list = get_nodes neighbors_index_edges in
          (* check if start node produce w_start or consum w_end 
          OR check if start node produce w_end *)
          if (((check_start_end [h]) == []) || ((check_being_end [h]) == [])) then
            find_index_3 t l2 li lp l2_is_end
          else
            (* get rid of nodes who produce w_start or consum w_end *)
            let neighbors_index_list = check_start_end neighbors_index_list in
            (* get rid of nodes who produce w_end *)
            let neighbors_index_list = check_being_end neighbors_index_list in
            if (neighbors_index_list == []) then
              find_index_3 t l2 li lp l2_is_end
            else
              (* check if start node produce an inhibitor of himself (very unlikely) or of external nodes 
                OR start node being inhib by external nodes product *)
              if (((check_inhib_by_itself_or_others [h] li) == []) || ((check_inhib_by_product [h] lp) == [])) then
                find_index_3 t l2 li lp l2_is_end
              else
                (* get rid of those who are being inhib by start node products or external nodes products *)
                let neighbors_index_list = check_inhib_by_product neighbors_index_list (p @ lp) in
                (* get rid of those who produce an inhibitor of themselves or start node or external nodes *)
                let neighbors_index_list = check_inhib_by_itself_or_others neighbors_index_list (i::li) in
                if (neighbors_index_list = []) then
                  find_index_3 t l2 li lp l2_is_end
                else
                  (* will repeat the process but to join the neighbors of start node with their own neighbors that are end nodes *)
                  let rec join_with_ends is ps l1 l2 l2_is_end =
                    match l2 with
                    | [] -> []
                    | (h1,h2)::t -> 
                      (* h1 = neighbor of start node ; h2 = neighbors of h1 that are end nodes *)
                      let actual_v = Array.get rt h1 in
                      match actual_v with
                      | None -> failwith "error in search_path"
                      | Some(n) -> 
                        let (_,i,_,p,_,_) = n in
                        if (List.mem h1 l1) then
                          (* get rid of nodes who produce w_start or consum w_end *)
                          let new_h2 = check_start_end h2 in
                          if (new_h2 == []) then
                            join_with_ends is ps l1 t l2_is_end
                          else
                            (* get rid of those who are being inhib by other nodes products *)
                            let new_h2 = check_inhib_by_product new_h2 (p @ ps) in
                            (* get rid of those who produce an inhibitor of themselves or other nodes *)
                            let new_h2 = check_inhib_by_itself_or_others new_h2 (i::is) in
                            if (new_h2 == []) then
                              join_with_ends is ps l1 t l2_is_end
                            else
                              (* if using it for length 3 : the l2 list is the end list so we are good *)
                              if l2_is_end then
                                (h1::new_h2) @ join_with_ends is ps l1 t l2_is_end
                              (* else we are using it for greater length and we don't want the l2 list to be a part of the end list *)
                              else
                                (* get rid of nodes who produce w_end *)
                                let new_h2 = check_being_end h2 in
                                if (new_h2 == []) then
                                  join_with_ends is ps l1 t l2_is_end
                                else
                                  (h1::new_h2) @ join_with_ends is ps l1 t l2_is_end
                        else
                          join_with_ends is ps l1 t l2_is_end in
                  (* final list of reactions that participate in a path from this start node *)
                  let neighbors_index = join_with_ends (i::li) (p @ lp) neighbors_index_list neighbors_index_edges l2_is_end in
                  if (neighbors_index == []) then
                    find_index_3 t l2 li lp l2_is_end
                  else
                    (h::neighbors_index) @ (find_index_3 t l2 li lp l2_is_end) in


















  (* we search the path from l1 to in_edges of every l2 nodes, using find_index_3 *)
  let rec find_index_4 l1 l2 l2_is_end =
    match l2 with
    | [] -> []
    | h::t -> 
      let actual_v = Array.get rt h in
      match actual_v with
      | None -> failwith "error in search_path"
      | Some(n) -> 
        let (_,i,s,p,ie,_) = n in
        if (ie == []) then
          find_index_4 l1 t l2_is_end
        else
          (* check if end node produce w_start or consum w_end
          OR check if end node produce an inhibitor of himself (very unlikely) *)
          if (((check_start_end [h]) == []) || ((check_inhib_by_itself_or_others [h] []) == []))then
            find_index_4 l1 t l2_is_end
          else
            (* generate the reactions from path start to in_edges conform with the end node *)
            let subpath_neighbors = find_index_3 l1 ie [i] p false in
            if (subpath_neighbors == []) then
              find_index_4 l1 t l2_is_end
            else
              (* if using it for length 4 : the l2 list is the end list so we are good *)
              if l2_is_end then
                (h::subpath_neighbors) @ (find_index_4 l1 t l2_is_end)
              (* else we are using it for greater length and we don't want the l2 list to be a part of the end list *)
              else
                (* check if end node produce w_end *)
                if ((check_being_end [h]) == []) then
                  find_index_4 l1 t l2_is_end
                else
                  (h::subpath_neighbors) @ (find_index_4 l1 t l2_is_end) in

  
  (* we search the path from l1 to in_edges of every l2 nodes using find_index_4 *)
  let rec find_index_5 l1 l2 l2_is_end =
    match l2 with
    | [] -> []
    | h::t -> 
      let actual_v = Array.get rt h in
      match actual_v with
      | None -> failwith "error in search_path"
      | Some(n) -> 
        let (_,i,s,p,ie,_) = n in
        if (ie == []) then
          find_index_5 l1 t l2_is_end
        else
          (* check if end node produce w_start or consum w_end
          OR check if end node produce an inhibitor of himself (very unlikely) *)
          if (((check_start_end [h]) == []) || ((check_inhib_by_itself_or_others [h] []) == []))then
            find_index_5 l1 t l2_is_end
          else
            (* generate the reactions from path start to in_edges conform with the end node *)
            let subpath_neighbors = find_index_4 l1 ie false in
            if (subpath_neighbors == []) then
              find_index_5 l1 t l2_is_end
            else
              (* if using it for length 5 : the l2 list is the end list so we are good *)
              if l2_is_end then
                (h::subpath_neighbors) @ (find_index_5 l1 t l2_is_end)
              (* else we are using it for greater length and we don't want the l2 list to be a part of the end list *)
              else
                (* check if end node produce w_end *)
                if ((check_being_end [h]) == []) then
                  find_index_5 l1 t l2_is_end
                else
                  (h::subpath_neighbors) @ (find_index_5 l1 t l2_is_end) in




    

  (* we search the path from l1 to in_edges of every l2 nodes using find_index_5 *)
  let rec find_index_6 l1 l2 =
    match l2 with
    | [] -> []
    | h::t -> 
      let actual_v = Array.get rt h in
      match actual_v with
      | None -> failwith "error in search_path"
      | Some(n) -> 
        let (_,i,s,p,ie,_) = n in
        if (ie == []) then
          find_index_6 l1 t
        else
          (* check if end node produce w_start or consum w_end
          OR check if end node produce an inhibitor of himself (very unlikely) *)
          if (((check_start_end [h]) == []) || ((check_inhib_by_itself_or_others [h] []) == []))then
            find_index_6 l1 t
          else
            (* generate the reactions from path start to in_edges conform with the end node *)
            let subpath_neighbors = find_index_5 l1 ie false in
            if (subpath_neighbors == []) then
              find_index_6 l1 t
            else
              (h::subpath_neighbors) @ (find_index_6 l1 t) in

          
  


















  (* results might contains duplicate but doesn't matter*)
  match len with 
  | 1 ->
    (* find every index who are both in start_list and end_list *)
    find_index_1 start_list end_list
  | 2 ->
    (* for each start index find the out_edges that ends *)
    find_index_2 start_list end_list 
  | 3 ->
    (* for each start index find the nodes that connects with ends *)
    find_index_3 start_list end_list [] [] true
  | 4 -> 
    (* we search the path from start to in_edges of every end nodes using find_index_3 *)
    find_index_4 start_list end_list true
  | 5 ->
    (* we search the path from start to in_edges of every end nodes using find_index_4 *)
    find_index_5 start_list end_list true
  | 6 ->
    (* we search the path from start to in_edges of every end nodes using find_index_5 *)
    find_index_6 start_list end_list
  | _ -> []

























let interpret_graph rt it len ?(oc = stdout) will_outpout_to_file: unit =
  (* get the start and end enzymes from the user input *)
  let w_start = ref "" in
  let w_end = ref "" in
  printf "Enter the start enzyme: ";
  let () = w_start := read_line () in
  printf "Enter the end enzyme: ";
  let () = w_end := read_line () in
  printf "interpreting %s to %s with length %d :\n" !w_start !w_end len;
  (* initialize the edges *)
  let start_time = Sys.time() in
  let new_rt = initialize_rt rt in
  printf "edges construction time : %f\n" (Sys.time() -. start_time);
  (* initialize the start and end list *)
  let start_time = Sys.time() in
  let (new_rt,start_list,end_list) = initialize_start_end new_rt !w_start !w_end [] [] in
  printf "start/end list construction time : %f\n" (Sys.time() -. start_time);
  match len with
  | 0 -> printf "length must be greater than 0\n"
  | n -> 
    (* search the path *)
    let start_time = Sys.time() in
    let sol = (remove_duplicates [] (search_path new_rt it !w_start !w_end start_list end_list n)) in
    printf "search paths time : %f\n" (Sys.time() -. start_time);
    printf "found %d solutions\n" (List.length sol);
    printf "printing solutions : %s\n" (Astring.string_of_int_list sol);
    printf "done !\n\n";
    if will_outpout_to_file then
      begin
      fprintf oc "%s" (Astring.string_of_reac_table_sol new_rt sol);
      end
    else
      printf "%s" (Astring.string_of_reac_table_sol new_rt sol);
