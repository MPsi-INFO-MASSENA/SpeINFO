type 'a tab_redim = {mutable nb: int ; mutable tab: 'a array} ;;

let creer_tab () = {nb = 0; tab = [| |]} ;;

let acces t i =
  if i< t.nb then
    t.tab.(i)
  else
    failwith "depassement d'indice"
;;

let modif t i x =
  if i< t.nb then
    t.tab.(i) <- x
  else
    failwith "depassement d'indice"
;;

let ajout t x=
  if t.nb = Array.length t.tab then
    let u=Array.make (2*t.nb+1) x in 
    for i=0 to t.nb-1 do 
      u.(i) <- t.tab.(i)
    done ;
    t.nb <- t.nb + 1 ;
    t.tab <- u
  else
    (t.tab.(t.nb) <- x ; t.nb <- t.nb + 1)
;;

let suppr t =
  if t.nb > 0 then
    (t.nb <- t.nb - 1 ; t.tab.(t.nb))
  else
    failwith "tableau vide"
;;

let rec nbc n b=match n with
  | 0 -> 0
  | _ -> 1+ nbc (n/b) b
;;

let rec puissance x n=match n with
  | 0 -> 1
  | _ -> x* puissance x (n-1)
;;


(* let t=creer_tab () ;;
 * for i=0 to 9 do
 *   ajout t i
 * done ;;
 * print_int (acces t 3) ; print_string " " ;
 * for i=0 to 5 do
 *   print_int (suppr t) ; print_string " "
 * done ;
 * print_int (acces t 1) ; print_string " " ;
 * ajout t 12 ;
 * for i=0 to 4 do
 *   print_int (suppr t) ; print_string " "
 * done ;; *)

	      


let puissance2 x n=
  let rec aux acc n = match n with
    | 0 -> acc
    | _ -> aux (acc*x) (n-1)
  in aux 1 n
;;

(* puissance2 3 10 ;; *) 

let rec expo x n = match n, n mod 2 with
  | 0,_ -> 1
  | _,0 -> expo (x*x) (n/2) 
  | _ -> x*expo (x*x) (n/2)
;;

(* expo 3 10 ;; *)

let rec somme n = match n with
  | 0 -> 0
  | _ -> n + somme (n-1)
;;

let somme2 n = 
  let rec aux acc n = match n with
  | 0 -> acc
  | _ -> aux (acc+n) (n-1)
  in aux 0 n
;;

(* somme 1000000 ;;
 * somme2 1000000 ;; *)


let rec fibo n = match n with
  | 0 | 1 -> 1
  | _ -> fibo (n-1) + fibo (n-2)
;;

(* #trace fibo ;;
 * fibo 7 ;; *)



#load "graphics.cma" ;;
open Graphics ;;
open_graph " 1000x1000" ;;

let triangle p1 p2 p3 couleur =
set_color couleur;
fill_poly [|p1; p2; p3|];;

(* triangle (212,84) (812,84) (512,700) black;; *)

let milieu p1 p2=
  let x1, y1 = p1 and x2, y2 = p2 in
  (x1+x2)/2, (y1+y2)/2
;;

let sierpinski n=
  let p1, p2, p3 = (212,84), (812,84), (512,700) in
  triangle p1 p2 p3 black ;
  let rec aux n p1 p2 p3=
    let m1, m2, m3 = milieu p2 p3, milieu p1 p3, milieu p2 p1 in
    triangle m1 m2 m3 white ;
    if n>0 then begin
      aux (n-1) p1 m2 m3 ;
      aux (n-1) p2 m1 m3 ;
      aux (n-1) p3 m2 m1 ;
    end 
  in aux n p1 p2 p3 ;;

(* sierpinski 6 ;; *)

