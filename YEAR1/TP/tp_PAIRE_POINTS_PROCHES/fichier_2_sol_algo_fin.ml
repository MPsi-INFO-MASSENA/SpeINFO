(* Il faut bien entendu avoir ecrit toutes les fonctions intermediaires ! *)

let plus_proche_efficace t =
  let tx, ty = tri_xy t in
  let rec aux tx ty =
    let n = Array.length tx in
    if n <= 6 then
      plus_proche_naif tx
    else begin
        let m = n/2 in
        let txg, txd = Array.sub tx 0 m, Array.sub tx m (n-m) in
        let a=tx.(m) in
        let tyg, tyd = reparti ty a in
        let cg = aux txg tyg and cd = aux txd tyd in
        let d = min (distance (fst cg) (snd cg)) (distance (fst cd) (snd cd)) in
        let x0 = (fst txg.(m-1) +. fst txd.(0)) /. 2. in
        let tb = extrait_bande ty x0 d in
        let db, im, jm = parcours_bande tb d in
        if db < d then
          tb.(im), tb.(jm)
        else if d = distance (fst cg) (snd cg) then
          cg
        else
          cd
      end
  in aux tx ty
;;

let egal_couple (x1, y1) (x2, y2) =
  x1 = x2 && y1 = y2 || x1 = y2 && x2 = y1
;;

let temps f x =
    let t = Sys.time() in
    let fx = f x in
    Printf.printf "Temps d'execution: %fs\n" (Sys.time() -. t);
    fx

let test n =
  (* comparaison pour un nuage de taille n *)
  let tab = rd_nuage n in
  trace_nuage tab ;
  let p, q = temps plus_proche_naif tab in
  coloration p q ;
  egal_couple (p,q) (temps plus_proche_efficace tab) (* verification ! *) 
;;


test 1000 ;;
