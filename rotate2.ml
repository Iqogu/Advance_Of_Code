let div_e x k = 
  let q = x / k in
  if x < 0 && x mod k != 0 then q - 1 else q  (* Car Ocaml arrondit les divisions négatives vers 0 et pas -infini peut faire rater des passages par 0*)
;;

let roue (current_number, mdp_count) line =
  try
    let action_char = String.get line 0 in
    let rotation_str = String.sub line 1 (String.length line - 1) in
    let rotation = int_of_string rotation_str in

    let new_number = 
      match action_char with
      | 'L' -> current_number - rotation
      | 'R' -> current_number + rotation
      | _   -> 
        Printf.printf "Action inconnue (%c). Ligne ignorée: %s\n" action_char line;
        current_number
    in
    (*Pas de changement jusque là*)

    let div x = div_e x 100 in (* Le reste obtenue est le nombre de tour effectués exemple si ont avance de 900 tour a droite div_e renverra 9 donc ont ajoutera 9 *)

    let tours_count = 
      if new_number > current_number then (* test pour vérifier si on avance vers la droite (positif) ou a gauche (négatif) *)
        (div new_number)
      else if new_number < current_number then
        (div (current_number - 1)) - (div (new_number - 1)) (* ont ajoute -1 a current_number dans le cas où ont serait déjà a 0 pour pas qu'il compte une seconde fois ce tour*)
      else
        0
    in

    let final_number = ((new_number mod 100) + 100) mod 100;
      in
    
    let final_mdp_count = tours_count + mdp_count (* additionne les count *)
      in

    (final_number,final_mdp_count)
    (* Pas de changement ensuite *)

    with
    | _ -> 
    Printf.printf "Erreur de parsing (ligne ignorée): [%s]\n" line;
    (current_number, mdp_count)

let read_file filename =
  let ic = In_channel.open_text filename in
  let lines = In_channel.input_lines ic in
  In_channel.close ic;
  lines

let () =
  let filename = "rotation.txt" in 
  let initial_value = 50 in
  
  let initial_state = (initial_value, 0) in
  try
    let all_lines = read_file filename in
    
    let final_state = 
      List.fold_left roue initial_state all_lines
    in

    
    let final_value, count = final_state in

    Printf.printf "Valeur finale après tous les mouvements : %d\n" final_value;
    Printf.printf "Le nombre de passages à 0 (mdp) est : %d\n" count
  with
  | Sys_error msg -> 
    Printf.printf "Erreur : Impossible d'ouvrir le fichier %s. %s\n" filename msg