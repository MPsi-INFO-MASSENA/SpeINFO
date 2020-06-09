(* SECTION 1 ET 2 *)

let creer_pile () = Stack.create () ;;
let pile_vide p = Stack.is_empty p ;;
let empiler p x = Stack.push x p ;;
let depiler p = Stack.pop p ;;
let sommet p = Stack.top p ;;


(* SECTION 2 *)

let n=50 ;;
let tab_visites = Array.make_matrix n n false ;;
tab_visites.(0).(0) <- true ;;

#load "graphics.cma" ;;
open Graphics ;;

let s=string_of_int (10*n+10) in
let t= " "^s^"x"^s in
open_graph t ;;

let trace c1 c2=
  set_color black ;
  let x1, y1 = c1 and x2, y2 = c2 in
  moveto (10+10*x1) (10+10*y1) ;
  lineto (10+10*x2) (10+10*y2)
;;
