(* SECTION I *)

type 'a tab_redim = {mutable nb: int ; mutable tab: 'a array}  ;;
let creer_tab () = {nb = 0 ; tab = [| |]} ;;

(* SECTION III *)

(* #load "graphics.cma" ;;
 * open Graphics ;;
 * open_graph " 1000x1000" ;;
 * 
 * let triangle p1 p2 p3 couleur =
 *   set_color couleur;
 *   fill_poly [|p1; p2; p3|]
 * ;; *)