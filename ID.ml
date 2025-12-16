type range = { debut : int; fin : int } (* type pour representer une plage de fraicheur debut = premier éléments, fin = dernier *)

let is_frais id r =
  id >= r.debut && id <= r.fin (* Vérifie si l'ID est entre debut et fin si oui il est frais *)

let solve filename =
  let ic = open_in filename in
  
  let rec read_ranges acc = 
    match input_line ic with
    | "" -> List.rev acc (* Ligne vide pour reconnaitre celle qui sépare les plages et les ID donc plus de plages ont renvoie la liste de plages *)
    | line ->
        let parts = String.split_on_char '-' line in (* sépare la ligne en 2 pour faire 2 éléments différents dans un tableau*)
        let r = { 
          debut = int_of_string (List.nth parts 0); (* prend l'éléments de départs *)
          fin = int_of_string (List.nth parts 1)  (* prend celui d'arriver *)
        } in
        read_ranges (r :: acc) (* ajoute la nouvelle plage en tête de la liste *)
    | exception End_of_file -> List.rev acc   (* en cas de fin de fichier renvoie la liste *)
  in

  let frais = read_ranges [] in (* liste de toutes les plages de fraicheur du fichiers *)

  let rec count_frais count =
    match input_line ic with 
    | line ->
        if line = "" then count_frais count (* ignore ligne vide *)
        else
          let id = int_of_string (String.trim line) in (* converti la ligne en entier *)
          let exists = List.exists (is_frais id) frais in  (* appel is_frais et renvoie True si au moins une plage contient L'ID *)
          count_frais (if exists then count + 1 else count) (* Si exists = True alors +1 pour le conter de frais si non on continue (appel récursif) *)
    | exception End_of_file -> count (* Quand on est a la fin du fichier ont renvoie le nombre total d'élements frais *)
  in

  let total_frais = count_frais 0 in
  close_in ic;
  total_frais

let () =
  let filename = "ID.txt" in 
  let result = solve filename in
  Printf.printf "Nombre d'ingrédients frais : %d\n" result