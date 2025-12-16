let char_to_int c =
  (int_of_char c) - (int_of_char '0') (* convertit un caractère chiffre ('0' à '9') en son entier correspondant *)

let find_max digits =
  let rec aux idx best_idx best_val = function
    | [] | [_] -> Some (best_val, best_idx) (* retire le dernire car ont doit former un nombre avec un chiffre forcément a sa droite *)
    | h :: t ->
        if h > best_val then
          aux (idx + 1) idx h t  (* fonction pour trouver le max si head > a la valeur la plus haute précédente ont le garde (et ont rappelle la fonction pour l'élément suivant) *)
        else
          aux (idx + 1) best_idx best_val t (* si non ont passe a l'élément suivant *)
  in
  match digits with
  | h :: t -> aux 1 0 h t (* et ont commence a l'élément indice 1 de valeur 0 *)
  | _ -> None

let find_max_of_list = function
  | [] -> None
  | h :: t -> Some (List.fold_left max h t)  (* renvoie le maximum de la liste *)

let calc line =
  let digits =
    line |> String.to_seq |> List.of_seq |> List.map char_to_int (* IA je n'arrivais pas a découper ma ligne dans une liste caractères par caractères *)
  in

  match find_max digits with
  | None -> None (*  Rien de valide *)
  | Some (max1, idx1) ->  (* M1 trouvé + son indice *)
      let rec drop n = function
        | l when n <= 0 -> l
        | [] -> []
        | _ :: t -> drop (n - 1) t  (* retire les n premiers éléments de la liste *)
      in
      let sublist = drop (idx1 + 1) digits in (* retire tout les chiffres avant l'indice + l'indice *)
      match find_max_of_list sublist with (* recherche du plus grand dans la nouvelle liste *)
      | None -> None
      | Some max2 ->
        let result_string = (string_of_int max1) ^ (string_of_int max2) in (* On repasse en chaine de caractères pour concaténer *)
        let final_number = int_of_string result_string in (* et ont revient en entier pour le résultat final *)
        Some final_number


let () =
  let filename = "Bat.txt" in
  let total_sum = ref 0 in

  try
    let ic = open_in filename in
    let rec loop line_number =
      try
        let line = input_line ic in
        begin
          match calc line with (* appel calc pour chaque ligne *)
          | None ->
              Printf.printf "Line %d: [%s] -> Analyse échouée\n" line_number line
          | Some value ->
              total_sum := !total_sum + value 
        end;
        loop (line_number + 1)
      with End_of_file ->
        close_in ic
    in
    loop 1;
    Printf.printf "TOTAL FINAL DE L'ADDITION : %d\n" !total_sum
  with
  | Sys_error msg ->
      Printf.printf "Erreur fichier : %s\n" msg

