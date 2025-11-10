exception Cycle
type mark = NotVisited | InProgress | Visited

type 'a graph =
    { mutable g_nodes : 'a node list }
and 'a node = {
  n_label : 'a;
  mutable n_mark : mark;
  mutable n_link_to : 'a node list;
  mutable n_linked_by : 'a node list;
}

let mk_graph () = { g_nodes = [] }

let add_node g x =
  if not (List.mem x (List.map (fun n -> n.n_label) g.g_nodes)) then begin 
  let n = { n_label = x; n_mark = NotVisited; n_link_to = []; n_linked_by = [] } in
  g.g_nodes <- n :: g.g_nodes end

let node_of_label g x =
  List.find (fun n -> n.n_label = x) g.g_nodes

let add_edge g id1 id2 =
  try
    let n1 = node_of_label g id1 in
    let n2 = node_of_label g id2 in
    n1.n_link_to   <- n2 :: n1.n_link_to;
    n2.n_linked_by <- n1 :: n2.n_linked_by
  with Not_found -> Format.eprintf "Tried to add an edge between non-existing nodes"; raise Not_found

let clear_marks g =
  List.iter (fun n -> n.n_mark <- NotVisited) g.g_nodes

let find_roots g =
  List.filter (fun n -> n.n_linked_by = []) g.g_nodes

let has_cycle (g : 'a graph) = 
  List.iter (fun n -> n.n_mark <- NotVisited ) g.g_nodes ;
  let rec has_cycle_aux (n : 'a node) : bool =
    n.n_mark = InProgress || begin n.n_mark = NotVisited && begin
    n.n_mark <- InProgress ;
    if List.exists has_cycle_aux n.n_link_to then true else (n.n_mark <- Visited ; false) end end
  in 
  List.exists has_cycle_aux g.g_nodes

let topological (g : 'a graph) : 'a list = 
  if has_cycle g then raise Cycle 
  else begin 
    List.iter (fun n -> n.n_mark <- NotVisited ) g.g_nodes ;
    let l = ref [] in 
    let rec topological_aux (n : 'a node) : unit = 
      match n.n_mark with
      | Visited -> ()
      | InProgress -> assert(false) (* I could as well no check has_cycle at the beginning and raise Cycle here *)
      | NotVisited -> begin 
        n.n_mark <- InProgress ;
        List.iter topological_aux n.n_link_to ;
        n.n_mark <- Visited ;
        l := n.n_label :: !l
      end
    in
    List.iter topological_aux g.g_nodes ;
    !l
  end


let print_graph g print = 
  Printf.printf "Graphe actuel : \n" ;
  List.iter (fun n -> print n.n_label ; Printf.printf " : " ; List.iter (fun n' -> print n'.n_label) n.n_link_to ; Printf.printf "\n") g.g_nodes