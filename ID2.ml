type range = { debut : int; fin : int } (* pas de changement *)

let fusion_ranges ranges = (* fusonne les plages qui se supperposent et celle qui se suivent pour éviter de compter plusieur fois les mêmes valeurs *)
  let sorted = List.sort (fun a b -> compare a.debut b.debut) ranges in (* tri toutes les plages par rapport a leur debut *)
  match sorted with
  | [] -> [] 
  | first :: rest -> (* ont sépare la premieres plage des autres *)
      let (fusion, final_range) = 
        List.fold_left (fun (acc, actuel) next -> (* parcours des plages *)
          if next.debut <= actuel.fin + 1 then (* si le début de la plage suivante est inférieur a la fin de la plage actuel *)
            (acc, { actuel with fin = max actuel.fin next.fin }) (* si c'est le cas ont change la fin de la plage actuel pour la plus grande entre celle ci et la suivante *)
          else
            (actuel :: acc, next) (* si non la plage actuel est terminée ont essaye avec la suivante *)
        ) ([], first) rest
      in
      final_range :: fusion (* ajout de la derniere liste restante dans le fold_left au début *)

let solve filename =
  let ic = open_in filename in
  
  let rec read_ranges acc = 
    match input_line ic with
    | "" -> List.rev acc 
    | line ->
        let parts = String.split_on_char '-' line in 
        let r = { 
          debut = int_of_string (List.nth parts 0); 
          fin = int_of_string (List.nth parts 1)  
        } in
        read_ranges (r :: acc) 
    | exception End_of_file -> List.rev acc   (* Pas de différence avec l'exo 1 *)
  in

  let raw_ranges = read_ranges [] in (* listes de toutes les plages lu depuis le fichier *)
  close_in ic;

  let fusion = fusion_ranges raw_ranges in (* appel la fonction de fusion *)
  
  List.fold_left (fun acc r -> acc + (r.fin - r.debut + 1)) 0 fusion (* parcours les plages de fusion puis soustrait l'element de fin par celui du début pour obtenir l'écart ajout de 1 *)
  (* car il faut inclure l'elements ou l'on commence et ensuite on additionne ce resultat a la taille de la plage suivante ce qui donne le résultat final *)

let () =
  let filename = "ID.txt" in 
  let result = solve filename in
  Printf.printf "Nombre total d'IDs frais uniques : %d\n" result