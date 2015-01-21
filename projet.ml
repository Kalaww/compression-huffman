open Bitio


type arbre = 
	| Feuille of int * int 
	| Noeud of int * arbre * arbre 

type foret = arbre list

type dico_entry = {key: int; bin: int list}

type dictionnaire = dico_entry list

type cmd_arg = {comp: bool ref; decomp: bool ref}


exception DICO_NO_ENTRY of char

exception COMPRESSION_FAIL of string



let fin_fichier = -1
let octets_magiques = [1;0;0;0; 0;1;1;1; 0;1;0;0; 1;0;1;0; 0;0;0;1; 1;1;1;1; 0;1;0;0; 1;0;0;0]
let octets_cedric = [1;1;0;0; 1;1;1;0; 1;1;0;1; 1;1;0;0; 1;1;1;0; 1;1;0;1; 1;0;0;1; 0;0;1;0]


let histogramme_set fc = 
	let h = Array.make 256 0 in
	let q = ref true in
	while !q do
		try
			let a = input_byte fc in
			h.(a) <- succ h.(a)
		with End_of_file -> q := false
	done;
	h;;


let foret_of_histogramme histo = 
	let histo_list = Array.to_list histo in
	let rec foret_of_histogramme_rec h c = match h with
		| [] -> []
		| a::k -> if a > 0 
					then Feuille(a, c)::(foret_of_histogramme_rec k (c+1))
					else foret_of_histogramme_rec k (c+1)
	in foret_of_histogramme_rec histo_list 0


let rec arbre_string a = match a with
	| Feuille(k, j) -> "["^(string_of_int k)^","^(string_of_int j)^"]"
	| Noeud(k, g, d) -> "("^(string_of_int k)^","^(arbre_string g)^","^(arbre_string d)^")"


let rec foret_string foret = match foret with
	| [] -> ""
	| a::k -> "> "^(arbre_string a)^"\n"^(foret_string k)


let rec foret_insert f e = match f with
	| [] -> [e]
	| Noeud(a,b,c)::k -> (match e with
						| Noeud(a1,b1,c1) -> if a1 <= a then e::Noeud(a,b,c)::k else Noeud(a,b,c)::foret_insert k e
						| Feuille(a1,b1) -> if a1 <= a then e::Noeud(a,b,c)::k else Noeud(a,b,c)::foret_insert k e)
	| Feuille(a,b)::k -> (match e with
						| Noeud(a1,b1,c1) -> if a1 <= a then e::Feuille(a,b)::k else Feuille(a,b)::foret_insert k e
						| Feuille(a1,b1) -> if a1 <= a then e::Feuille(a,b)::k else Feuille(a,b)::foret_insert k e)


let rec foret_sort f = match f with
	| [] -> []
	| a::k -> foret_insert (foret_sort k) a


let rec foret_get_final_arbre f = match f with
	| [] -> []
	| [a] -> [a]
	| Noeud(a,b,c)::Noeud(a1,b1,c1)::k -> foret_get_final_arbre(foret_sort(Noeud(a+a1, Noeud(a,b,c), Noeud(a1,b1,c1))::k))
	| Noeud(a,b,c)::Feuille(a1,b1)::k -> foret_get_final_arbre(foret_sort(Noeud(a+a1, Noeud(a,b,c), Feuille(a1,b1))::k))
	| Feuille(a,b)::Noeud(a1,b1,c1)::k -> foret_get_final_arbre(foret_sort(Noeud(a+a1, Feuille(a,b), Noeud(a1,b1,c1))::k))
	| Feuille(a,b)::Feuille(a1,b1)::k -> foret_get_final_arbre(foret_sort(Noeud(a+a1, Feuille(a,b), Feuille(a1,b1))::k))


let int_to_bin i =
	let rec int_to_bin_b l i = 
		if (List.length l) = 8 then l else
		let k = 128 / int_of_float (2. ** float_of_int (List.length l)) in
		if i >= k then int_to_bin_b (1::l) (i-k) else int_to_bin_b (0::l) i
	in List.rev (int_to_bin_b [] i)


let bin_to_int l = 
	let rec bin_to_int_b l i = match l with
		| [] -> 0
		| [a] -> a*i
		| a::k -> a*i+(bin_to_int_b k i*2)
	in bin_to_int_b (List.rev l) 1


let rec arbre_encode a = match a with
	| Feuille(x,y) -> if y = fin_fichier then [0; 1;1;1;1; 1;1;1;1 ;1]
					else if y = 255 then [0; 1;1;1;1; 1;1;1;1 ;0]
					else 0::(int_to_bin y)
	| Noeud(x,y,z) -> [1]@(arbre_encode y)@(arbre_encode z)


let rec list_string l = match l with
	| [] -> ""
	| [a] -> string_of_int a
	| a::k -> (string_of_int a)^(list_string k)


let dico_of_arbre a = 
	let rec dico_of_arbre_b a p = match a with
		| Feuille(x,y) -> [{key=y; bin=(List.rev p)}]
		| Noeud(x,g,d) -> (dico_of_arbre_b g (0::p))@(dico_of_arbre_b d (1::p))
	in dico_of_arbre_b a []


let dico_entry_string d = 
	(string_of_int d.key)^": "^(list_string d.bin)


let rec dico_string d = match d with
	| [] -> ""
	| a::k -> "["^(dico_entry_string a)^"]"^(dico_string k)


let rec dico_get d e = match d with
	| [] -> raise (DICO_NO_ENTRY (char_of_int e))
	| a::k -> if a.key = e then a.bin else dico_get k e


let text_encode f_in f_out dico = 
	let q = ref true in
	while !q do
		try
			let k = input_byte f_in in
			let l = dico_get dico k in
			List.iter (fun i -> output_bit f_out i) l;
		with
			End_of_file -> q := false;
	done;
	List.iter (fun i -> output_bit f_out i) (dico_get dico (fin_fichier))


let print_l a = 
	print_string (a^"\n")


let read_n_bits file n = 
	let rec read_n_bits_b file n = 
		if n = 0 then []
		else (input_bit file)::(read_n_bits_b file (n-1))
	in List.rev (read_n_bits_b file n)


let rec list_compare l1 l2 = match l1, l2 with
	| [], [] -> true
	| [], _ -> false
	| _, [] -> false
	| a1::k1, a2::k2 -> a1 = a2 && list_compare k1 k2


let rec bin_to_arbre file = match input_bit file with
	| 0 -> 	let code_bit = read_n_bits file 8 in
			if list_compare [1;1;1;1; 1;1;1;1] code_bit
			then begin
				if (input_bit file) = 0 then Feuille(1, 255) else Feuille(1, fin_fichier)
			end else Feuille(1, (bin_to_int code_bit))
	| 1 ->	let g = bin_to_arbre file in
			let d = bin_to_arbre file in
			Noeud(1,g,d)
	| _ -> Feuille(1,-2)


let rec arbre_get_data file arbre= match arbre with
	| Feuille(a,b) -> b
	| Noeud(a,b,c) -> if input_bit file = 0 
						then arbre_get_data file b
						else arbre_get_data file c


let rec text_decode f_in f_out arbre = 
	let c = arbre_get_data f_in arbre in
	if c != fin_fichier 
	then begin
		output_byte f_out c;
		text_decode f_in f_out arbre;
	end else ()


let compression filename_in filename_out = 
	(* Open fichier in *)
	let f_in = open_in_bin filename_in in
	(* Memorisation position du début *)
	let pos_in_debut = pos_in f_in in
	(* Création de l'histogramme *)
	let histo = histogramme_set f_in in
	(* Création de la forêt *)
	let foret = Feuille(1,fin_fichier)::(foret_of_histogramme histo) in
	(* Tri de la forêt *)
	let foret_triee = foret_sort foret in
	(* Récuperation de l'arbre de l'algorithme de Huffman *)
	let arbre_final = List.hd(foret_get_final_arbre foret_triee) in
	(* Création du dictionnaire des caractères *)
	let dico = dico_of_arbre arbre_final in
	(* Open fichier out *)
	let f_out = open_out_bit filename_out in
	(* 4 octets magiques *)
	List.iter (output_bit f_out) octets_magiques;
	(* Extension CEDCED92 *)
	List.iter (output_bit f_out) [0;0;0;0; 0;1;0;0];
	List.iter (output_bit f_out) octets_cedric;
	(* Representation de l'arbre *)
	List.iter (output_bit f_out) (arbre_encode arbre_final);
	(* Repositionne au début du fichier in *)
	seek_in f_in pos_in_debut;
	(* Texte compressé *)
	begin
		try text_encode f_in f_out dico
		with DICO_NO_ENTRY mot ->
				raise (COMPRESSION_FAIL ("le caractère "^(String.make 1 mot)^" du texte n'a pas de valeur dans l'arbre"))
	end;
	(* aligne le fichier sur une frontiere d'octet *)
	output_align_bit f_out;
	(* Extension CEDCED92 *)
	List.iter (output_bit f_out) [0;0;0;0; 0;1;0;0];
	List.iter (output_bit f_out) octets_cedric;
	(* close *)
	close_out_bit f_out;
	close_in f_in



let decompression filename_in filename_out =
	let f_in = open_in_bit filename_in in
	(* Test de l'entete magique *)
	let verif_entete = list_compare octets_magiques (read_n_bits f_in (List.length octets_magiques)) in
	if verif_entete = false
	then begin
		print_string ("Le fichier "^filename_in^" n'est pas au bon format.\n");
		close_in_bit f_in;
		exit 1;
	end else ();
	(* Nombre d'octects libre à lire *)
	let octects_a_lire = bin_to_int (read_n_bits f_in 8) in
	if octects_a_lire >= 4
	then begin
		(* Test si l'espace libre est encodé pour Cedric *)
		let verif_entete_cedric = list_compare octets_cedric (read_n_bits f_in (List.length octets_cedric)) in
		(* Traitement de la plage des modules *)
		if verif_entete_cedric = true
			then ignore (read_n_bits f_in ((octects_a_lire-4)*8))
			else ignore (read_n_bits f_in ((octects_a_lire-4)*8));
	end else ignore (read_n_bits f_in (octects_a_lire*8));
	(* Recuperation de l'arbre *)
	let arbre = bin_to_arbre f_in in
	let f_out = open_out_bin filename_out in
	(* Decode le texte *)
	text_decode f_in f_out arbre;
	(* Aligne la positon courante sur un octet *)
	input_align_bit f_in;
	(* Nombre d'octects libre à lire *)
	let octects_a_lire_fin = bin_to_int (read_n_bits f_in 8) in
	if octects_a_lire_fin >= 4
	then begin
		(* Test si l'espace libre est encodé pour Cedric *)
		let verif_entete_cedric = list_compare octets_cedric (read_n_bits f_in (List.length octets_cedric)) in
		(* Traitement de la plage des modules *)
		if verif_entete_cedric = true then () else ();
	end else ();
	(* close *)
	close_in_bit f_in;
	close_out f_out


let cmd_usage nom = "Usage: "^nom^" [options] [fichier d'entré] [fichier de sortie]\nOptions:\n\t-c : compression\n\t-d : décompression\n"


let cmd_arg_set cmd a = match a with
	| 'c' -> cmd.comp := true
	| 'd' -> cmd.decomp := true
	| _ -> ();;


let init = 
	let cmd = Sys.argv in
	if Array.length cmd != 4
	then begin
		print_string (cmd_usage cmd.(0));
		exit 1;
	end else ();
	if cmd.(1).[0] != '-'
	then begin
		print_string (cmd_usage cmd.(0));
		exit 1;
	end else ();
	let args = {comp=(ref false); decomp=(ref false)} in
	String.iter (fun i -> cmd_arg_set args i) cmd.(1);
	if !(args.comp) = true && !(args.decomp) = true
	then begin
		print_string ("Impossible de compresser et décompresser en même temps\n"^cmd_usage cmd.(0));
		exit 1;
	end else ();
	if !(args.comp) && (String.length cmd.(3) < 3 || String.compare ".hf" (String.sub cmd.(3) ((String.length cmd.(3)) -3) 3) != 0)
	then begin
		print_string "Le fichier de sortie doit avoir l'extenstion '.hf'\n";
		exit 1;
	end else ();
	if !(args.decomp) && (String.length cmd.(2) < 3 || String.compare ".hf" (String.sub cmd.(2) ((String.length cmd.(2)) -3) 3) != 0)
	then begin
		print_string "Le fichier d'entré doit avoir l'extenstion '.hf'\n";
		exit 1;
	end else ();
	if !(args.comp) = true
	then begin
		begin
			try compression cmd.(2) cmd.(3)
			with COMPRESSION_FAIL phrase -> 
				print_l ("Une erreur est survenue pendant la compression : "^phrase);
				Sys.remove cmd.(3)
		end
	end else ();
	if !(args.decomp) = true then decompression cmd.(2) cmd.(3) else ();;

init;;


