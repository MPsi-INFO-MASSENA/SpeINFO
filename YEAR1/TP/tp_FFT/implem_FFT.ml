let p=12289 ;;
(* let p=167772161 ;; *)
let ( ++ ) x y=(x+y) mod p ;;
let ( ** ) x y=(x*y) mod p ;;
let ( -- ) x y=x ++ (p-y) ;;


let rec bezout n m=match m with
  | 0 -> (1,0)
  | _ -> let q,r=n/m, n mod m in let u, v=bezout m r in v,u-q*v
;;

let ( // ) x y=
  let m,_=bezout y p in
  if m>0 then
    x ** m
  else
    x ** ((m mod p)+p)
;;

let rec add_p a b=
  let n,m=Array.length a, Array.length b in
  if n<m then add_p b a else 
    let c=Array.copy a in
    for i=0 to m-1 do
      c.(i) <- c.(i) ++ b.(i)
    done ;
    c
;;

let prod_mon a n=
  Array.append (Array.make n 0) a
;;

let prod_scal a x=
  Array.map (function y -> y ** x) a
;;


let prod_p a b=
  let c=ref [| |] in
  for i=0 to Array.length b -1 do
    c:= add_p !c (prod_mon (prod_scal a b.(i)) i)
  done ;
  !c
;;

let prod_terme v w=
  let n=Array.length v in
  let z=Array.make n 0 in
  for i=0 to n-1 do
    z.(i) <- v.(i) ** w.(i)
  done ;
  z
;;


let racines_unite=
  let v=Array.make 13 41 in
  for i=11 downto 0 do
    v.(i) <- v.(i+1) ** v.(i+1)
  done ; v;;

let calculR0 f k=
  let v=Array.make k 0 in
  for i=0 to k-1 do
    v.(i) <- f.(i) ++ f.(i+k)
  done ;
  v
;;

let calculR1b f k w=
  let v=Array.make k 0 and q=ref 1 in
  for i=0 to k-1 do
    v.(i) <- (f.(i) -- f.(i+k)) ** !q ;
    q:= !q ** w
  done ;
  v
;;

let recomposition t0 t1 k=
  let n=2*k in
  let v=Array.make n 0 in
  for i=0 to k-1 do
    v.(2*i) <- t0.(i) ;
    v.(2*i+1) <- t1.(i)
  done ;
  v
;;

let rec evaluation f n w=
  if n=1 then f else begin
    let k=n/2 in 
    let r0, r1b=calculR0 f k, calculR1b f k w in
    let w2 = w ** w in
    let t0, t1=evaluation r0 k w2, evaluation r1b k w2 in
    recomposition t0 t1 k
  end 
;;

let interpolation t n w=
  prod_scal (evaluation t n (1 // w)) (1 // n)
;;

let produitFFT a b n w=
  let pa, pb=evaluation a n w, evaluation b n w in
  interpolation (prod_terme pa pb) n w
;;

