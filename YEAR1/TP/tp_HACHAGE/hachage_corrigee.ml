type ('a, 'b) table_hachage = {hache: 'a -> int; donnees: ('a * 'b) list array };;


(* SECTION 1*)
let hachage_entier w k=k mod w ;;

(* hachage_entier 14 201 ;; *)


let hachage_chaine w s=
  let x=ref 0 and p=ref 1 in
  for i=0 to String.length s - 1 do
    x:=(!x + (int_of_char s.[i])* !p) mod w ;
    p:= ( !p*128) mod w
  done ;
  !x
;;

(* hachage_chaine 12 "Bonjour !" ;;
 * hachage_chaine 12 "oh oui youpi dansons la carioca" ;; *)

let hachage_chaine w s =
  let x=ref 0 in
  for i=String.length s - 1 downto 0 do
    x:= ( !x * 128 + int_of_char s.[i]) mod w
  done ;
  !x
;;

(* SECTION 2*)
(* hachage_chaine 12 "Bonjour !" ;;
 * hachage_chaine 12 "oh oui youpi dansons la carioca" ;; *)



let creer_table h w={hache= h ; donnees=Array.make w []} ;;

(* creer_table (hachage_entier 5) 5 ;; *)

let petit_exemple = {
    hache = hachage_entier 3;
     donnees =
       [|[(15, "truc"); (468, "ocaml"); (498, "confinement"); (144, "TP")];
         [(1, "machin"); (154, "coucou")]; [(185, "info"); (512, "MPSI")]|]
    }
;;

                                                                                    
let recherche t k=
  let rec aux l=match l with
    | [] -> false
    | x::q -> fst x=k || aux q
  in aux t.donnees.(t.hache k)
;;

(* recherche petit_exemple 498 ;;
 * recherche petit_exemple 499 ;; *)


let element t k=
  let rec aux l=match l with
    | [] -> raise Not_found
    | x::q when fst x=k -> snd x
    | x::q -> aux q
  in aux t.donnees.(t.hache k)
;;

(* element petit_exemple 498 ;;
 * element petit_exemple 499 ;; *)

let element t k=
  let rec aux l=match l with
    | [] -> raise Not_found
    | (a,b)::q when a=k -> b
    | _::q -> aux q
  in aux t.donnees.(t.hache k)
;;  

let ignore a=() ;; (* fonction de type 'a -> unit, qui ignore son argument *)

let recherche t k=
  try
    ignore (List.assoc k t.donnees.(t.hache k)) ; (* cette ligne est bien de type unit *)
    true
  with Not_found -> false
;;

let ajout t k e= if not (recherche t k) then let hk=t.hache k in t.donnees.(hk) <- (k,e)::t.donnees.(hk) ;;

(* ajout petit_exemple 38 "metallica" ;;
 * petit_exemple.donnees.(2) ;; *)



let suppression t k=
  let rec aux q =match q with
    | [] -> []
    | x::p when fst x=k -> p
    | x::p -> x::(aux p)   
  in let hk=t.hache k in t.donnees.(hk) <- aux t.donnees.(hk) 
;;

(* suppression petit_exemple 498 ;;
 * petit_exemple.donnees.(0) ;; *)



(* SECTION 4*)

type ('a, 'b) table_dyn = {hache: int -> 'a -> int ; mutable taille: int ; mutable donnees: ('a * 'b) list array} ;;

let creer_table_dyn h w={hache= h ; taille=0 ; donnees=Array.make w []} ;;

let recherche_dyn t k=
  let w=Array.length t.donnees in
  let hk=t.hache w k in
  try
    let _= (List.assoc k t.donnees.(hk)) in true
  with Not_found -> false
;;


let element_dyn t k=
  let w=Array.length t.donnees in
  let hk=t.hache w k in
  List.assoc k t.donnees.(hk) ;
;;

let rearrange_dyn t=
  let w=Array.length t.donnees in
  let nv_donnees=Array.make (2*w) [] in
  let nv_h=t.hache (2*w) in
  let rec aux l=match l with
    | [] -> ()
    | x::q -> nv_donnees.(nv_h (fst x)) <- x::nv_donnees.(nv_h (fst x)) ; aux q
  in 
  for i=0 to w-1 do
    aux t.donnees.(i)
  done ;
  t.donnees <- nv_donnees
;;
let ajout_dyn t k e=
  let w=Array.length t.donnees in
  if not (recherche_dyn t k) then  begin
    let hk=t.hache w k in t.donnees.(hk) <- (k,e)::t.donnees.(hk) ;
    t.taille <- t.taille + 1 ;
    if t.taille > 2*w then rearrange_dyn t
  end 
;;

let rearrange_bis_dyn t=
  let w=Array.length t.donnees in
  let nv_donnees=Array.make (w/2) [] in
  let nv_h=t.hache (w/2) in
  let rec aux l=match l with
    | [] -> ()
    | x::q -> nv_donnees.(nv_h (fst x)) <- x::nv_donnees.(nv_h (fst x)) ; aux q
  in 
  for i=0 to w-1 do
    aux t.donnees.(i)
  done ;
  t.donnees <- nv_donnees
;;




let suppression_dyn t k=
  let w=Array.length t.donnees in
  let hk=t.hache w k in
  let rec aux q = match q with
    | [] -> raise Not_found
    | x::p when fst x=k -> p
    | x::p -> x::(aux p)
  in try 
     t.donnees.(hk) <- aux t.donnees.(hk) ;
     t.taille <- t.taille - 1 ; 
     if t.taille < w/2 then rearrange_bis_dyn t
  with Not_found ->  ()
;;