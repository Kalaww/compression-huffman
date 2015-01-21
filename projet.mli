(** Compression de Huffman *)

(** {2 Types et Structures de données } *)

(** Structure d'arbre  *)
type arbre = 
	Feuille of int * int (** Feuille de l'arbre : le premier int est l'occurrence d'un octet et le deuxième l'octet *)
	| Noeud of int * arbre * arbre (** Noeud de l'arbre : le premier int est la somme des occurrences des sous arbres, les deux arbres sont les arbres gauche et droite *)

(** Forêt : liste d'arbre *)
type foret = arbre list

(** Stocke la valeur d'un octet (int) et sa représentation selon l'arbre obtenu avec l'algorithme de Huffman (liste d'int) *)
type dico_entry = { key : int; bin : int list; }

(** Dictionnaire : liste contenant des mappages d'une clé et de sa valeur *)
type dictionnaire = dico_entry list

(** Stocke les différentes informations générales sur l'execution courante *)
type cmd_arg = { comp : bool ref; decomp : bool ref; }



(** {2 Exceptions }*)

(** Lever si une clé n'est pas trouvé dans un dictionnaire *)
exception DICO_NO_ENTRY of char

(** Lever si une erreur est survenue pendant la compression *)
exception COMPRESSION_FAIL of string


(** {2 Constantes } *)

(** Valeur pour la représentation de la fin de fichier pour l'arbre *)
val fin_fichier : int

(** Suite d'octects magiques *)
val octets_magiques : int list

(** Suite d'octects magiques de Cédric Desgranges *)
val octets_cedric : int list



(** {2 Fonctions } *)

(** Génère un histogramme à partir d'un fichier *)
val histogramme_set : in_channel -> int array

(** Converti un histogramme en forêt *)
val foret_of_histogramme : int array -> arbre list

(** Représentation en String d'un arbre *)
val arbre_string : arbre -> string

(** Représentation en String d'une forêt *)
val foret_string : arbre list -> string

(** Insertion d'un arbre dans une forêt selon le nombre d'occurence croissante *)
val foret_insert : arbre list -> arbre -> arbre list

(** Tri une forêt selon le nombre d'occurence croissante *)
val foret_sort : arbre list -> arbre list

(** Converti une forêt en un arbre unique selon l'algorithme de Huffman *)
val foret_get_final_arbre : arbre list -> arbre list

(** Converti un entier en une suite binaire *)
val int_to_bin : int -> int list

(** Converti une suite binaire en un entier *)
val bin_to_int : int list -> int

(** Encode un arbre en une suite binaire selon l'algorithme du sujet *)
val arbre_encode : arbre -> int list

(** Représentation en String d'une liste *)
val list_string : int list -> string

(** Converti un arbre en un dictionnaire selon l'algorithme de Huffman*)
val dico_of_arbre : arbre -> dico_entry list

(** Représentation en String d'un dico_entry *)
val dico_entry_string : dico_entry -> string

(** Représentation en String d'un dictionnaire *)
val dico_string : dico_entry list -> string

(** Récupère la valeur d'une clé dans un dictionnaire *)
val dico_get : dico_entry list -> int -> int list

(** Encode en suite binaire le texte de f_in dans f_out selon le dictionnaire dico *)
val text_encode :
  in_channel -> Bitio.bit_out_channel -> dico_entry list -> unit

(** print_string avec \n *)
val print_l : string -> unit

(** Lis n bits et retourne une liste binaire des bits lus *)
val read_n_bits : Bitio.bit_in_channel -> int -> int list

(** Compare deux listes *)
val list_compare : 'a list -> 'a list -> bool

(** Converti une suite binaire en arbre selon l'argorithme du sujet *)
val bin_to_arbre : Bitio.bit_in_channel -> arbre

(** Recupère le caractère correspondant à une suite binaire *)
val arbre_get_data : Bitio.bit_in_channel -> arbre -> int

(** Decode le texte de f_in avec arbre et l'écrit dans f_out *)
val text_decode : Bitio.bit_in_channel -> out_channel -> arbre -> unit

(** Compression du fichier filename_in en filename_out *)
val compression : string -> string -> unit

(** Décompression du fichier filename_in en filename_out *)
val decompression : string -> string -> unit

(** Usage de la ligne de commande *)
val cmd_usage : string -> string

(** Switch des options de ligne de commande *)
val cmd_arg_set : cmd_arg -> char -> unit

(** Main *)
val init : unit
