open Netlist_ast
open Graph

exception Combinational_cycle

let read_exp (eq : equation) : ident list = 
  let extract_vars = function Avar x -> [x] | _ -> [] in  
  match snd eq with
  | Earg a -> extract_vars a
  | Ereg x -> [x] 
  | Enot a -> extract_vars a
  | Ebinop  (binop, a1, a2) -> (extract_vars a1) @ (extract_vars a2)(* OP a1 a2 : boolean operator *)
  | Emux (a1,a2,a3) -> (extract_vars a1) @ (extract_vars a2) @ (extract_vars a3)(* MUX a1 a2 : multiplexer *)
  | Erom (_,_,a) -> extract_vars a
  | Eram (_,_,a,_,_,_) -> (extract_vars a)
      (* RAM addr_size word_size read_addr write_enable write_addr data *)
  | Econcat (a1,a2) -> (extract_vars a1) @ (extract_vars a2)
  | Eslice (_,_,a) -> extract_vars a
    (* SLICE i1 i2 a : extract the slice of a between indices i1 and i2 *)
  | Eselect (_,a) -> extract_vars a
    (* SELECT i a : ith element of a *)


let schedule0 (p : program) : program = 
  let g = mk_graph () in
  List.iter (add_node g) p.p_inputs ;
  List.iter (add_node g) p.p_outputs ;
  List.iter (fun eq -> 
    let vars = read_exp eq in
    add_node g (fst eq) ;
    print_string (fst eq) ;
    List.iter (fun x -> add_edge g x (fst eq)) vars
  ) p.p_eqs ;
  (* print_graph g (Printf.printf "%s  "); *)
  try 
    let tri = topological g in (* tri est alors une liste de variables triées dans l'ordre, il faut obtenir leurs équations de définition *)
    let list_eqs = List.concat_map (fun x -> if List.mem x p.p_inputs then [] else [List.find (fun eq -> x = (fst eq)) p.p_eqs]) tri in 
    let p = {p_eqs = list_eqs; p_inputs = p.p_inputs ; p_outputs = p.p_outputs ; p_vars = p.p_vars} in p
  with 
    |Cycle -> raise Combinational_cycle


let schedule (p : program) : program =   
  let g = mk_graph () in
  List.iter (add_node g) p.p_inputs ;
  List.iter (add_node g) p.p_outputs ;
  (* as one can write in register for the beginnig, I add it to the nodes directly *)
  List.iter (fun eq -> match (snd eq) with Ereg x -> add_node g x | _ -> ()) p.p_eqs ;
  List.iter (fun eq -> 
    let vars = read_exp eq in
    add_node g (fst eq) ;
    List.iter (add_node g) vars ;
    print_string (fst eq) ;
    List.iter (fun x -> add_edge g x (fst eq)) vars
  ) p.p_eqs ;
  print_graph g (Printf.printf "%s  ");
  try 
    let tri = topological g in (* tri est alors une liste de variables triées dans l'ordre, il faut obtenir leurs équations de définition *)
    let list_eqs = List.concat_map (fun x -> if List.mem x p.p_inputs then [] else [List.find (fun eq -> x = (fst eq)) p.p_eqs]) tri in 
    let p = {p_eqs = list_eqs; p_inputs = p.p_inputs ; p_outputs = p.p_outputs ; p_vars = p.p_vars} in p
  with 
    |Cycle -> raise Combinational_cycle

    
    
    
