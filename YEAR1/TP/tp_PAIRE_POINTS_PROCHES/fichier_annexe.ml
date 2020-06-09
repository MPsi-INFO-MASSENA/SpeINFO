let rd_couple () = Random.float 1000., Random.float 1000. ;;

let rd_nuage n =
  let t= Array.make n (0.,0.) in
  for i = 0 to n-1 do
    t.(i) <- rd_couple ()
  done ;
  t
;;

let tab1 = [|(137.952822280455479, 579.517066764289893);
  (745.24288848465, 673.134497490137164);
  (466.113061566003125, 188.103956342865786);
  (944.098271788395095, 410.935431876780797);
  (186.89530977266773, 283.014495971758549);
  (136.342566575285474, 698.315452710874752);
  (345.004922664389085, 939.068336631267471);
  (891.680623462061476, 287.663161181994212)|]
;;

#load "graphics.cma" ;;
open Graphics ;;
open_graph " 1000x1000" ;;

let round x =
  (* arrondi un flottant a l'entier le plus proche *)
  if x -. (floor x) < 0.5 then int_of_float (floor x) else int_of_float (floor x) + 1
;;

let trace_nuage nuage =
  for i=0 to Array.length nuage - 1 do
    let x,y=nuage.(i) in
    fill_circle (round x) (round y) 3
  done ;;

let coloration p q =
  let x1, y1 = p and x2, y2 = q in
  set_color red ;
  fill_circle (round x1) (round y1) 5 ;
  set_color blue ;
  fill_circle (round x2) (round y2) 5 ;
  set_color black ;
;;

let tab_int = [|2;50;13;8;2;12;48;16;10;46;81;21;30|] ;;

let tab1x = [|(136.342566575285474, 698.315452710874752);
    (137.952822280455479, 579.517066764289893);
    (186.89530977266773, 283.014495971758549);
    (345.004922664389085, 939.068336631267471);
    (466.113061566003125, 188.103956342865786);
    (745.24288848465, 673.134497490137164);
    (891.680623462061476, 287.663161181994212);
    (944.098271788395095, 410.935431876780797)|]
;;

let tab1y =  [|(466.113061566003125, 188.103956342865786);
    (186.89530977266773, 283.014495971758549);
    (891.680623462061476, 287.663161181994212);
    (944.098271788395095, 410.935431876780797);
    (137.952822280455479, 579.517066764289893);
    (745.24288848465, 673.134497490137164);
    (136.342566575285474, 698.315452710874752);
    (345.004922664389085, 939.068336631267471)|]
;;

let tb1 =
  [|(466.113061566003125, 188.103956342865786);
    (345.004922664389085, 939.068336631267471)|]
;;