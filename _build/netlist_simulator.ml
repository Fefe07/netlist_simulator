
open Netlist_ast

let print_only = ref false
let number_steps = ref (-1)

let print_bool b = 
  if b then Printf.printf "True" else Printf.printf "False"

let print_bool_tab t = 
  Printf.printf "[|" ;
  for i = 0 to Array.length t -2 do 
    print_bool t.(i);
    Printf.printf " ; " 
  done ;
  print_bool t.(Array.length t - 1) ;
  Printf.printf "|]\n" 

let print_value v = 
  match v with 
  | VBit b      -> print_bool b ; print_newline () 
  | VBitArray t -> print_bool_tab t 

let print_ram (r:bool array array)(i:int) : unit = 
  Printf.printf "RAM %d : \n" i;
  Array.iter (fun x -> Printf.printf "\t"; print_bool_tab x) r

let find (err_mess : string) env x = 
  try Hashtbl.find env x 
  with 
    | Not_found -> failwith ("Erreur Not_Found dans "^ err_mess)

let int_of_bool b = 
  if b then 1 else 0

let int_of_bool_array t = 
  let s = ref 0 in 
  let n = Array.length t in 
  for i = 0 to n - 1 do 
    s := !s + (int_of_bool t.(i)) lsl (n-i-1)
  done ;
  !s

let int_of_value v = 
  match v with 
  | VBitArray t -> int_of_bool_array t
  | VBit b -> failwith "mauvais type de valeur pour une adresse"


let critical_path (p:program) : int =
  (* On suppose que le programme est bien planifié*) 
  let lengths = Hashtbl.create 42 in 
  List.iter (fun x -> Hashtbl.add lengths x 0) p.p_inputs ;
  let maxi = ref 0 in 
  let treat_eq (eq:equation) : unit = 
    let prec = Scheduler.read_exp eq in 
    let l = List.fold_left (fun acc x -> max acc (Hashtbl.find lengths x)) 0 prec in 
    match snd eq with 
    | Ereg _ | Eram _ | Erom _ -> Hashtbl.add lengths (fst eq) 0 ; maxi := max !maxi l
    | _ -> Hashtbl.add lengths (fst eq) (l+1) ; maxi := max !maxi (l+1)
  in 
  List.iter treat_eq p.p_eqs ;
  !maxi

let delay_ident (p:program)(x:ident) : program = 
  let p_eqs' = List.fold_right (fun eq acc -> 
    let y,expr = eq in 
    if x = y then (x,Ereg (x^"2")) :: (x^"2",expr)::acc 
    else eq :: acc  
  ) p.p_eqs [] in 
  {p_eqs = p_eqs' ; p_inputs = p.p_inputs ; p_outputs = p.p_outputs ; p_vars = p.p_vars}

let simulator (p : program) (number_steps : int) : unit =
  (* let ram = Array.make 42 false in  *)
  let rams = Hashtbl.create 42 in 
  List.iteri (fun i eq-> let z,e = eq in match e with Eram(addr_size,word_size,_,_,_,_) -> Hashtbl.add rams i (Array.make (1 lsl addr_size) (Array.make word_size false)) | _ -> ()) p.p_eqs ;
  let env = Hashtbl.create 42 in  (* ident - value dictionnary *) 
    
  

  for i = 1 to number_steps do 
    Printf.printf "i = %d\n" i ;

    (* A METTRE DANS LA BOUCLE ? OUI *)
    List.iter (fun x ->
        
      (* a completer - Done*)
      Printf.printf "%s = ?" x ;
      Hashtbl.add env x begin
      let s = read_line () in 
      let n = String.length s in 
      if n>1 then VBitArray (let t = Array.init n (fun i -> s.[i] = '1') in  print_bool_tab t ; t) (* n-2 because of the \0 character -> NO !*)
      else (VBit (s.[0] == '1'))
      
      end

    ) p.p_inputs ; 

    let value_from_arg a = 
      match a with 
      | Avar x -> find "value_from_arg" env x
      | Aconst c -> c 
    in 

    let todo = ref ( fun () -> () ) in

    let execute (j:int)(eq : equation) : unit =
      let z, e = eq in
      match e with
      | Earg a -> Hashtbl.add env z (value_from_arg a)
      | Ereg x -> Hashtbl.add env z (find "Ereg" env x) 
      | Enot a -> begin 
        match (value_from_arg a) with 
        | VBit b -> Hashtbl.add env z (VBit (not b))
        | _ -> failwith "negation d'un tableau"
      end
      | Ebinop (op, a, b) -> begin 
        match value_from_arg a, value_from_arg b with
        | VBit c, VBit d ->  Hashtbl.add env z (VBit begin match op with 
          |Or -> c || d 
          |Nand -> not (c && d)
          |And -> c && d 
          |Xor -> c && (not d) || (not c) && d
      end)
        | VBitArray c, VBitArray d ->  Hashtbl.add env z (VBitArray begin match op with 
          |Or -> Array.map2 (||) c d
          |Nand -> Array.map2 (fun x y -> not(x&&y)) c d
          |And -> Array.map2 (&&) c d 
          |Xor -> Array.map2 (fun x y -> (x && not y)||(not x && y)) c d
      end)
        | _ -> failwith "binary operation between bit and tab"
    end
      | Emux (a,b,c) -> Hashtbl.add env z begin 
        match value_from_arg a with 
        | VBit d -> if d then value_from_arg b else value_from_arg c 
        | _ -> failwith "mauvais argument pour mux"
      end
      | Econcat (a,b) -> Hashtbl.add env z begin
        let bit_array_from_bit = function 
        | VBit b -> VBitArray (Array.make 1 b)
        | VBitArray t -> VBitArray t in  
        let c,d= match bit_array_from_bit(value_from_arg a), bit_array_from_bit (value_from_arg b)with VBitArray c, VBitArray d -> c,d | _ -> assert false in 
        VBitArray (Array.append c d)
        (* | _ -> failwith "mauvais argument pour concat" *)
      end
      | Eslice(i1,i2,a) -> Hashtbl.add env z begin 
        match value_from_arg a with 
        |VBitArray b -> VBitArray(Array.sub b i1 (i2-i1+1))
        | _ -> failwith "mauvais arguments pour slice"
      end
      |Eselect (i,a) -> Hashtbl.add env z begin 
        match value_from_arg a with 
        |VBitArray b -> VBit( b.(i))
        | VBit b -> Printf.printf "i = %d, b = " i ; print_bool b ; failwith "mauvais arguments pour select"
      end
      | Eram (addr_size,word_size,read_addr,write_enable,write_addr,write_data) -> 
        (* Tout comme les registres, il faut créer une RAM par instruction *)
        (* Ces Rams seront identifiées par un indice j (le numero d'instruction) *)

        let ram = find "RAM" rams j in 
        Hashtbl.add env z begin 
          let v= value_from_arg read_addr in 
          Printf.printf "v = " ; let t = match v with VBitArray t -> t | _ -> assert false in print_bool_tab t ; 
          let addr = int_of_value v in 
          Printf.printf "addr = %d\n" addr ;
          let tmp = ram.(addr) in 
          (* Il faut calculer ça à la fin !!!!! *)
          let temp = !todo in 
          todo :=( fun () ->  
            (* Printf.printf "test\n" ; *)
            temp () ;
            begin 
              if (match value_from_arg write_enable with VBit b -> b | _ -> failwith "write_enable is not a boolean") 
              then ram.(int_of_value (value_from_arg write_addr)) <- 
                (match (value_from_arg write_data) with 
                  | VBitArray t -> Printf.printf "Wrote %d in ram %d at addr %d\n" (int_of_bool_array t) j (int_of_value (value_from_arg write_addr)) ; t
                  | _ -> failwith "not a correct value to write")  
            end
          ) ;
          VBitArray tmp
        end
        
      | _ -> assert(false) (* failwith "pas implementé" *)


    in 

    List.iteri execute p.p_eqs ;
    (* Printf.printf "Coucou \n" ; *)
    !todo () ;
    
    List.iter (fun x -> Printf.printf "%s =>" x ; print_value (find "Output" env x)) p.p_outputs;
    Hashtbl.iter (fun i r -> print_ram r i) rams 
  done 


let compile filename =
  try
    let p = Netlist.read_file filename in
    begin try
        let p = Scheduler.schedule p in
        (* Printf.printf "Chemin critique = %d\n" (critical_path p) ;
        let p' = delay_ident p "_l_23" in 
        Printf.printf "Chemin critique = %d\n" (critical_path p') ; *)
        simulator p !number_steps 
        
      with
        | Scheduler.Combinational_cycle ->
            Format.eprintf "The netlist has a combinatory cycle.@.";
    end;
  with
    | Netlist.Parse_error s -> Format.eprintf "An error accurred: %s@." s; exit 2

let main () =
  Arg.parse
    ["-n", Arg.Set_int number_steps, "Number of steps to simulate"]
    compile
    "" 
;;

main ()
