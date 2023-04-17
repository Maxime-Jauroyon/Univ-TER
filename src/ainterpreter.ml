open Printf
open Atypes

(* remove duplicates in l *)
(* s : to keep track of item seen *)
(* l : list to remove duplicates from *)
(* output : list without duplicates (in the same order as l) *)
let rec remove_duplicates ?(s = []) l =
  match l with
  | [] -> []
  | h::t -> if (List.mem h s) then remove_duplicates ~s:(s) t else (h::(remove_duplicates ~s:(h::s) t))


(* gives in_edges and out_edges for a reaction *)
(* rt : reaction table *)
(* s : list of substrat of the reaction *)
(* p : list of product of the reaction *)
(* ie : list of in_edges of the reaction *)
(* oe : list of out_edges of the reaction *)
(* iteration : to keep track of the current reaction *)
(* index : the reaction we are computing edges for (such that we don't look at it) *)
(* output : (in_edges,out_edges) of the reaction *)
let rec initialize_edges rt s p ?(ie=[]) ?(oe=[]) ?(iteration = 0) index =
  if iteration < !Atypes.reac_table_length then
    if iteration != index then
      let actual_v = Array.get rt iteration in
      match actual_v with
      | None -> failwith "error in initialize_edges"
      | Some(n) -> 
        let (_,_,su,pr,_,_) = n in
        if (List.exists (fun x -> (List.mem x p)) su) then
          if (List.exists (fun x -> (List.mem x s)) pr) then
            (* if this node is linked to both the substrats and products (the node is in both edges of the reaction) *)
            initialize_edges rt s p ~ie:(iteration :: ie) ~oe:(iteration :: oe) ~iteration:(iteration + 1) index
          else
            (* if this node substrats are linked to the products (the node is an out_edge of the reaction) *)
            initialize_edges rt s p ~ie:ie ~oe:(iteration :: oe) ~iteration:(iteration + 1) index
        else
          if (List.exists (fun x -> (List.mem x s)) pr) then
            (* if this node products are linked to the substrats (the node is an in_edge of the reaction) *)
            initialize_edges rt s p ~ie:(iteration :: ie) ~oe:oe ~iteration:(iteration + 1) index
          else
            (* if this node is not linked to the substrats or products *)
            initialize_edges rt s p ~ie:ie ~oe:oe ~iteration:(iteration + 1) index
    else
      (* if this was the reaction we are looking for edges, we skip it *)
      initialize_edges rt s p ~ie:ie ~oe:oe ~iteration:(iteration + 1) index
  else
    (ie,oe)


(* initialize the edges of every node *)
(* rt : reaction table *)
(* iteration : to keep track of the current reaction *)
(* output : reaction table with edges initialized *)
let rec initialize_rt ?(iteration = 0) rt =
  if iteration < !Atypes.reac_table_length then
    let actual_v = Array.get rt iteration in
    match actual_v with
    | None -> failwith "error in initialize_rt"
    | Some(n) -> 
      (* get the node *)
      let (r,name,s,p,ie,oe) = n in
      (* getting the in and out edges of the node *)
      let (new_in_edges,new_out_edges) = initialize_edges rt s p iteration in
      (* change them *)
      let new_node = (r,name,s,p,new_in_edges,new_out_edges) in
      (* save the changes *)
      Array.set rt iteration (Some new_node);
      initialize_rt ~iteration:(iteration + 1) rt
  else
    rt


(* initialize the start and end list of nodes *)
(* rt : reaction table *)
(* w_start : the name of the start enzyme *)
(* w_end : the name of the end enzyme *)
(* start_list : list of nodes that produce w_start *)
(* end_list : list of nodes that consum w_end *)
(* iteration : to keep track of the current reaction *)
(* output : start_list and end_list *)
let rec initialize_start_end ?(iteration = 0) rt ?(start_list=[]) ?(end_list=[]) w_start w_end =
  if iteration < !Atypes.reac_table_length then
    let actual_v = Array.get rt iteration in
    match actual_v with
    | None -> failwith "error in initialize_start_end"
    | Some(n) -> 
      let (r,name,s,p,ie,oe) = n in
      (* update start and end list if needed *)
      let new_start_list = ref start_list in
      let new_end_list = ref end_list in
      (* if the reaction consum w_start, add it to the start_list *)
      if (List.mem (Atypes.get_index w_start) s) then
        new_start_list := iteration :: !new_start_list;
      (* if the reaction produce w_end, add it to the end_list *)
      if (List.mem (Atypes.get_index w_end) p) then
        new_end_list := iteration :: !new_end_list;
      initialize_start_end ~iteration:(iteration + 1) rt ~start_list:!new_start_list ~end_list:!new_end_list w_start w_end
  else
    (start_list,end_list)























(* search all the path in the graph for a given length *)
(* rt : reaction table *)
(* it : inhibitor table *)
(* w_start : the name of the start enzyme *)
(* w_end : the name of the end enzyme *)
(* start_list : list of nodes that consum w_start *)
(* end_list : list of nodes that produce w_end *)
(* len : the length of the path *)
(* output : list of path *)
let search_path rt it w_start w_end start_list end_list len =

  (* following functions are going to be used for the different searching path algorithms *)
  
  (* get rid of those who consum w_start *)
  (* l : list of nodes to check *)
  (* output : list of nodes from l that don't consum w_start *)
  let rec check_being_start l =
    match l with
    | [] -> []
    | h::t -> 
      let actual_v = Array.get rt h in
      match actual_v with
      | None -> failwith "error in search_path"
      | Some(n) -> 
        let (_,_,s,_,_,_) = n in
        if (List.mem (Atypes.get_index w_start) s) then
          check_being_start t
        else
          h :: (check_being_start t) in

  (* get rid of those who produce w_end *)
  (* l : list of nodes to check *)
  (* output : list of nodes from l that don't produce w_end *)
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
  (* l : list of nodes to check *)
  (* output : list of nodes from l that don't produce w_start or consum w_end *)
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
  (* p : list of products *)
  (* l : list of inhibitors *)
  (* output : true if p is in l, false otherwise *)
  let rec is_in_inhibitor p l =
    match p with
    | [] -> false
    | h::t -> 
      let rec is_in_inhibitor_aux p l =
        match l with
        | [] -> false
        | h::t -> if (List.mem p !(Array.get it h)) then true else is_in_inhibitor_aux p t in 
      if (is_in_inhibitor_aux h l) then true else is_in_inhibitor t l in

  (* get rid of those who are being inhib by a product list *)
  (* l : list of nodes to check *)
  (* p : list of products *)
  (* output : list of nodes from l that are not being inhib by p *)
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
  (* l : list of nodes to check *)
  (* l2 : list of reactions *)
  (* output : list of nodes from l that are not producing an inhibitor of themselves or reactions list (l2) *)
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










          


  (* following functions are the 6 searching path algorithms *)

  (* find every index who are both in l1 and l2 *)
  (* l1 : list of nodes to check (always start list) *)
  (* l2 : list of nodes to check (always end list) *)
  (* output : list of nodes from l1 and l2 *)
  (* detail : looking for nodes that are in both start and end list *)
  (* o *)
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
  (* l1 : list of nodes to check (always start list) *)
  (* l2 : list of nodes to check (always end list) *)
  (* output : list of nodes from l1 or l2 being a part of a possible path *)
  (* detail : for each start node, looking for nodes (through the out_edges) that are end nodes *)
  (* o1 -> o2 *)
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
        (* get the neighbors of the start node that are end nodes *)
        let neighbors_index_list = find_index_aux oe l2 in
        if (neighbors_index_list == []) then
          find_index_2 t l2
        else
          (* check if start node produce w_start or consum w_end
          OR check if start node produce w_end *)
          if (((check_start_end [h]) == []) || ((check_being_end [h]) == [])) then
            find_index_2 t l2
          else
            (* get rid of those who consum w_start *)
            let neighbors_index_list = check_being_start neighbors_index_list in
            (* get rid of neighbors (end nodes) who produce w_start or consum w_end *)
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
  (* l1 : list of nodes to check (always start list) *)
  (* l2 : list of nodes to check (not always end list !!) *)
  (* l2_is_end : true if l2 is the end list, false otherwise *)
  (* li : external nodes indexes (used when l2 is not the end list) *)
  (* lp : external nodes products (used when l2 is not the end list) *)
  (* output : list of nodes being a part of a possible path *)
  (* detail : for each start node, looking for (intermediate) nodes (through the out_edges) and again for nodes that are in l2  *)
  (* o1 -> i2 -> o3 *)
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
            (* get rid of nodes who consum w_start *)
            let neighbors_index_list = check_being_start neighbors_index_list in
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
                        (* check if h1 isn't a node that we discard previously *)
                        if (List.mem h1 l1) then
                          (*get rid of nodes who consum w_start *)
                          let new_h2 = check_being_start h2 in
                          (* get rid of nodes who produce w_start or consum w_end *)
                          let new_h2 = check_start_end new_h2 in
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
                  (* printf "%s\n" (Astring.string_of_int_list neighbors_index); *)
                  if (neighbors_index == []) then
                    find_index_3 t l2 li lp l2_is_end
                  else
                    (h::neighbors_index) @ (find_index_3 t l2 li lp l2_is_end) in


















  (* we search the path from l1 to in_edges of every l2 nodes, using find_index_3 *)
  (* l1 : list of nodes to check (always start list) *)
  (* l2 : list of nodes to check (not always end list !!) *)
  (* l2_is_end : true if l2 is the end list, false otherwise *)
  (* output : list of nodes being a part of a possible path *)
  (* detail : for each end node, looking for (intermediate) nodes (through the in_edges) and then paths between start nodes and intermediate nodes *)
  (* o1 -> i2 <- o3 *)
  (* o1 -> i2 being of length 3 (using find_index_3) *)
  (* o1 -> ii1 -> i2 <- o3 *)
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
          OR check if end node produce an inhibitor of himself (very unlikely) 
          OR check if end node consum w_start *)
          if (((check_start_end [h]) == []) || ((check_inhib_by_itself_or_others [h] []) == []) || ((check_being_start [h]) == []))then
            find_index_4 l1 t l2_is_end
          else
            (* get the reactions from path start to in_edges conform with the end node *)
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
  (* l1 : list of nodes to check (always start list) *)
  (* l2 : list of nodes to check (not always end list !!) *)
  (* l2_is_end : true if l2 is the end list, false otherwise *)
  (* output : list of nodes being a part of a possible path *)
  (* detail : for each end node, looking for (intermediate) nodes (through the in_edges) and then paths between start nodes and intermediate nodes *)
  (* o1 -> i2 <- o3 *)
  (* o1 -> i2 being of length 4 (using find_index_4) *)
  (* o1 -> ii1 -> ii2 <- i2 <- o3 *)
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
          OR check if end node produce an inhibitor of himself (very unlikely)
          OR check if end node consum w_start *)
          if (((check_start_end [h]) == []) || ((check_inhib_by_itself_or_others [h] []) == []) || ((check_being_start [h]) == []))then
            find_index_5 l1 t l2_is_end
          else
            (* get the reactions from path start to in_edges conform with the end node *)
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
  (* l1 : list of nodes to check (always start list) *)
  (* l2 : list of nodes to check (always end list) *)
  (* output : list of nodes being a part of a possible path *)
  (* detail : for each end node, looking for (intermediate) nodes (through the in_edges) and then paths between start nodes and intermediate nodes *)
  (* o1 -> i2 <- o3 *)
  (* o1 -> i2 being of length 5 (using find_index_5) *)
  (* o1 -> ii1 -> ii2 <- ii3 <- i2 <- o3 *)
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
          OR check if end node produce an inhibitor of himself (very unlikely)
          OR check if end node consum w_start *)
          if (((check_start_end [h]) == []) || ((check_inhib_by_itself_or_others [h] []) == []) || ((check_being_start [h]) == []))then
            find_index_6 l1 t
          else
            (* get the reactions from path start to in_edges conform with the end node *)
            let subpath_neighbors = find_index_5 l1 ie false in
            if (subpath_neighbors == []) then
              find_index_6 l1 t
            else
              (h::subpath_neighbors) @ (find_index_6 l1 t) in

          
  


















  (* results might contains duplicate but doesn't matter*)
  match len with 
  | 1 ->
    (* find every index who are both in start_list and end_list *)
    (* o in start and end list *)
    find_index_1 start_list end_list
  | 2 ->
    (* for each start index find the out_edges that ends *)
    (* o1 -> o2 with o1 in start list and o2 in end list *)
    find_index_2 start_list end_list 
  | 3 ->
    (* for each start index find the nodes that connects with ends *)
    (* o1 -> i1 -> o2 with o1 in start list and o2 in end list, i1 connect them *)
    find_index_3 start_list end_list [] [] true
  | 4 -> 
    (* we search the path from start to in_edges of every end nodes using find_index_3 *)
    (* o1 -> i1 -> i2 <- o2 with o1 in start list and o2 in end list, i1 and i2 connect them *)
    (* o1 -> i1 -> i2 being find using find_index_3 *)
    find_index_4 start_list end_list true
  | 5 ->
    (* we search the path from start to in_edges of every end nodes using find_index_4 *)
    (* o1 -> i1 -> i2 -> i3 <- o2 with o1 in start list and o2 in end list, i1, i2 and i3 connect them *)
    (* o1 -> i1 -> i2 -> i3 being find using find_index_4 *)
    (* which give o1 -> i1 -> i2 <- i3 <- o2 so the search is divided *)
    find_index_5 start_list end_list true
  | 6 ->
    (* we search the path from start to in_edges of every end nodes using find_index_5 *)
    (* o1 -> i1 -> i2 -> i3 -> i4 <- o2 with o1 in start list and o2 in end list, i1, i2, i3 and i4 connect them *)
    (* o1 -> i1 -> i2 -> i3 -> i4 being find using find_index_5 *)
    (* which give o1 -> i1 -> i2 <- i3 <- i4 <- o2 so the search is divided too *)
    find_index_6 start_list end_list
  | _ -> []
























(* search all the possibles path in the graph *)
(* rt : the reaction table *)
(* it : the inhibitor table *)
(* len : the length of the path *)
(* oc : the output channel *)
(* will_outpout_to_file : if true the output will be written in a file *)
(* output : none, will just print the list of reactions being in a possible path *)
let interpret_graph rt it len ?(oc = stdout) will_outpout_to_file: unit =
  (* check if the length is valid *)
  if (len < 1) || (len > 6) then
    printf "length must be between 1 and 6\n"
  else
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
    let (start_list,end_list) = initialize_start_end new_rt !w_start !w_end in
    printf "start/end list construction time : %f\n" (Sys.time() -. start_time);
    (* search the path *)
    let start_time = Sys.time() in
    let sol = (remove_duplicates (search_path new_rt it !w_start !w_end start_list end_list len)) in
    printf "search paths time : %f\n" (Sys.time() -. start_time);
    printf "found %d solutions\n" (List.length sol);
    (* printf "printing solutions : %s\n" (Astring.string_of_int_list sol); *) (* for debuging *)
    printf "\n\n";
    if will_outpout_to_file then
      begin
      fprintf oc "%s" (Astring.string_of_reac_table_sol new_rt sol);
      end
    else
      printf "%s" (Astring.string_of_reac_table_sol new_rt sol);
