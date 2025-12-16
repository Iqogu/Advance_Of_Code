let char_to_int c =
  (int_of_char c) - (int_of_char '0')

let rec drop n = function
  | l when n <= 0 -> l
  | [] -> []
  | _ :: t -> drop (n - 1) t
(* les mêmes fonctions qui font la même chose qu'avant au dessus *)


let rec length = function (* renvoie la taille de la liste *)
  | [] -> 0
  | _ :: t -> 1 + length t


  let find_max lst limit = (* Troue le plus grand chiffre de lst en respectant la limite de premiers éléments*)
  let rec aux idx best_idx best_val count = function
    | [] -> Some (best_val, best_idx)
    | _ when count = limit -> Some (best_val, best_idx) (* Ont s'arrête a limite pour être sur qu'il reste sufisamment d'élément pour former un nombre de taille 12 *)
    | h :: t ->
        if h > best_val then
          aux (idx + 1) idx h (count + 1) t
        else
          aux (idx + 1) best_idx best_val (count + 1) t  (* comparaison pour trouver la valeur la plus grande *)
  in
  match lst with
  | [] -> None
  | h :: t -> aux 1 0 h 1 t

let calc line k =
  let digits =
    line |> String.to_seq |> List.of_seq |> List.map char_to_int (* même chose que l'exercice précédent *)
  in

  let rec build_number remaining_digits remaining_k acc = 
    if remaining_k = 0 then Some acc (* Si le nombre est terminé et est bien de taille k donc k = 0 *)
    else
      let len = length remaining_digits in
      if len < remaining_k then None (* Normalement ça doit pas être le cas mais si il ne reste plus assez de chiffres pour former un nombre de taille k *)
      else
        let limit = len - remaining_k + 1 in (* Calcul de la limite pour la fonction précédente (taille de la liste - le nombre de chiffres nécéssaire au nombre) *)
        match find_max remaining_digits limit with (* appel de la fonction au dessus sur la list et le nombre d'éléments max dans lequel ont a le droit de regarder *)
        | None -> None
        | Some (max_digit, idx) -> (* cas succès *)
            build_number (* on rappelle nuild_number *)
              (drop (idx + 1) remaining_digits) (* ont retire de la liste tout ce qu'il y a avant la valeur choisit *)
              (remaining_k - 1) (* On modifie la taille k restante *)
              (acc * 10 + max_digit) (* ont ajoute le chiffre au nombre ont multiplie l'ancien nombre par 10 pour ajouter un 0 et ont fait ensuite une addition plus simple que concaténer *)
  in
  build_number digits k 0 (* appel de base avec la liste complète le k de base et 0 éléments actuel *)

(* peu de différence a part l'ajout de k pour définir la taille max de mon nombre *)
let main () =
  let filename = "Bat.txt" in
  let total_sum = ref 0 in
  let k = 12 in

  try
    let ic = open_in filename in
    let rec loop line_number =
      try
        let line = input_line ic in
        begin
          match calc line k with
          | None ->
              Printf.printf "Line %d: [%s] -> Ligne trop courte\n" line_number line
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

let () = main ()
