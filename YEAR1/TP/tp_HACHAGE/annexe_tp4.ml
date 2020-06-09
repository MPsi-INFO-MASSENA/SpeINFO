(* SECTION 1 : Tests a decommenter une fois les fonctions ecrites *)

(* hachage_entier 14 201;;
 * hachage_chaine 12 "Bonjour !" ;;
 * hachage_chaine 12 "oh oui youpi dansons la carioca";; *)


(* SECTION 2 : Tests a decommenter une fois les fonctions ecrites *)
type ('a, 'b) table_hachage = { hache: 'a -> int; donnees: ('a * 'b) list array };;


(* creer_table (hachage_entier 5) 5 ;; *)

(* let petit_exemple = {
 *     hache = hachage_entier 3;
 *      donnees =
 *        [|[(15, "truc"); (468, "ocaml"); (498, "confinement"); (144, "TP")];
 *          [(1, "machin"); (154, "coucou")]; [(185, "info"); (512, "MPSI")]|]
 *     }
 * ;; *)

(* recherche petit_exemple 498 ;; (\* 498 = 0 mod 3, ne visiter que la premiÃ¨re liste *\) *)
(* recherche petit_exemple 499 ;; (\* 499 = 1 mod 3, visiter la deuxiÃ¨me *\) *)

(* element petit_exemple 498 ;; *)
(* element petit_exemple 499 ;; *)

(* ajout petit_exemple 38 "metallica" ; petit_exemple.donnees.(2) ;; (\* 38 = 2 mod 3 *\) *)
(* suppression petit_exemple 498 ; petit_exemple.donnees.(0) ;; *)


(* SECTION 3 *)

#load "graphics.cma" ;;
open Graphics ;;
open_graph " 650x650" ;;

let distance_max = 30 ;;

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


(* color_acc q x y tab prend en paramÃ¨tres une liste d'indices de tab, x et y, le tableau tab, et colore en noir le point (x,y) et en rouge les points tab.(i) pour i dans q *)

(* quand la section 3 sera faite: *)

(* let x,y = (155, 98) ;; *)
(* let q=accessibles 300 x y th nuage ;;  *)
(* color_acc q x y tab *)


