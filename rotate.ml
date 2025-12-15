let roue (current_number, mdp_count) line =
  try
    let action_char = String.get line 0 in  (* Récupère la lettre *)
    let rotation_str = String.sub line 1 (String.length line - 1) in (* Récupère le nombre *)
    let rotation = int_of_string rotation_str in (* Convertir d'entier a int*)

    let new_number = 
      match action_char with
      | 'L' -> current_number - rotation (* Décrémente Gauche *)
      | 'R' -> current_number + rotation (* Incrémente Droite *)
      | _   -> 
        Printf.printf "Action inconnue (%c). Ligne ignorée: %s\n" action_char line; (*Affiche le 1er caractères la ligne et la valeur actuel en cas de non correspondance*)
        current_number
    in

    let final_number = ((new_number mod 100) + 100) mod 100; (* Calcul dans le cas ou ont serait > a 99 ou < 0 si il ne l'ai pas le résultat est inchanger a la fin *)
      in
    
    let final_mdp_count = (* Vérifie simplement si la valeur actuel est 0 *)
      if final_number = 0 then
        mdp_count + 1 
      else
        mdp_count 
      in

    (final_number,final_mdp_count)
    with
  | _ -> 
    Printf.printf "Erreur de parsing (ligne ignorée): [%s]\n" line;
    (current_number, mdp_count)(* retourne l'état précédant en cas *)

let read_file filename =
  let ic = In_channel.open_text filename in
  let lines = In_channel.input_lines ic in
  In_channel.close ic;
  lines

let () =
  let filename = "rotation.txt" in 
  let initial_value = 50 in (* La valeur de départ *)
  
  let initial_state = (initial_value, 0) in (* tuple pour obtenir la valeur actuel + le nombre de passage sur 0 *)  
  try
    let all_lines = read_file filename in
    
let final_state = 
      List.fold_left roue initial_state all_lines (* Ma fonction sur mon fichier d'instrution avec mon tuple *)
    in

    
    let final_value, count = final_state in (* On désassemble le tuple final pour afficher les deux résultats *)

    Printf.printf "Valeur finale après tous les mouvements : %d\n" final_value; (* pas nécéssaire mais je m'en servais pour comparer les résultats *)
    Printf.printf "Le nombre de passages à 0 (mdp) est : %d\n" count (* résultats finale *)
  with
  | Sys_error msg -> 
    Printf.printf "Erreur : Impossible d'ouvrir le fichier %s. %s\n" filename msg