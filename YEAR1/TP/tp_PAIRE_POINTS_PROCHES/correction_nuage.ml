#load "graphics.cma" ;;
open Graphics ;;
open_graph " 650x650" ;;

let trace_point c f=
  let x,y=f*fst c, f*snd c in
  moveto x y ;
  fill_circle x y 4 ;;

let trace_cercle c f r=
  let x,y=f*fst c, f*snd c in
  moveto x y ;
  draw_circle x y (f*r) ;;

let max_tab t=
(* max des coordonnees *)
  let n=Array.length t and m=ref 0 in
  for i=0 to n-1 do
    m:= max (max !m (fst t.(i))) (snd t.(i))
  done ;
  !m
;;

let trace_nuage tab=
  set_color blue ;
  let f=600 / (max_tab tab) in
  for i = 0 to Array.length tab - 1 do
    trace_point tab.(i) f
  done ;;

let random_nuage n d=
  let t=Array.make n (0,0) in
  for i=0 to n-1 do
    t.(i) <- (Random.int d, Random.int d)
  done ;
  t
;;

let nuage = random_nuage 1000 300 ;;
trace_nuage nuage;;



type ('a, 'b) table_hachage = { hache: 'a -> int; donnees: ('a * 'b) list array };;

let hachage_entier w k=k mod w ;;


(* on suppose que r|d *)
let hachage_couple d r k=let x,y=k and p=d/r in (p*x+y) ;;

let creer_table h w={hache= h ; donnees=Array.make w []} ;;
let recherche t k=
  let rec aux l=match l with
    | [] -> false
    | x::q -> fst x=k || aux q
  in aux t.donnees.(t.hache k)
;;  
let element t k=
  let rec aux l=match l with
    | [] -> raise Not_found
    | x::q when fst x=k -> snd x
    | x::q -> aux q
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


let suppression t k=
  let rec aux q =match q with
    | [] -> []
    | x::p when fst x=k -> p
    | x::p -> x::(aux p)   
  in let hk=t.hache k in t.donnees.(hk) <- aux t.donnees.(hk) 
;;


let distance_max = 30 ;;

let creation_hach tab d=
  let p=d/distance_max in
  let hach (x,y)=x/distance_max+p*(y/distance_max) in
  let th=creer_table hach (p*p) in
  for i=0 to Array.length tab-1 do
    ajout th tab.(i) i
  done ;
  th
;;

let th=creation_hach nuage 300 ;;

let candidats_accessibles d x y th=
  let hach=th.hache in 
  let candidats=ref [] and t1=[|-distance_max; 0; distance_max|] in
  for i=0 to 2 do
    for j=0 to 2 do
      let x2,y2=x+t1.(i), y+t1.(j) in
      if 0<=x2 && x2<d && 0<=y2 && y2<d then
	candidats:=th.donnees.(hach (x2,y2)) @ !candidats
    done 
  done ;
  !candidats 
;;

let accessibles d x y th tab=
  let ca=candidats_accessibles d x y th in
  let rec aux acc q=match q with
    | [] -> acc
    | ((a,b),i)::p when (x-a)*(x-a)+(y-b)*(y-b) <= distance_max*distance_max -> aux (i::acc) p
    | _::p -> aux acc p
  in aux [] ca
;;

let x,y = (155, 98) ;;
let q=accessibles 300 x y th nuage ;;

let color_acc q x y tab=
  set_color red ;
  let f=600 / (max_tab tab) in
  let rec aux q=match q with
    | [] -> ()
    | i::p -> trace_point tab.(i) f ; aux p
  in aux q ;
  set_color black ;
  trace_point (x,y) f ;
  trace_cercle (x,y) f distance_max ;
;;

color_acc q x y nuage ;;