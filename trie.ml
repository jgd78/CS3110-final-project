
type t = Head of t list | Node of (string * t list * bool) | Leaf 

let empty = Head ([Leaf])

(** [add_word_help trie word] is a helper for [add_word] that deals with adding
  a character to a single node*)
let rec add_word_help trie word =
  let curchar = String.sub word 0 1 in 
  let tail = String.sub word 1 ((String.length word) - 1) in
    match trie with
    | Node (c, children, is_word) -> Node (c, update_children children tail,
    String.length word = 1 || is_word)
    | Leaf -> Node (curchar, update_children [Leaf] tail,
    String.length word = 1)
    | _ -> failwith "Function does not accept Head as input"
(** [update_children children word] is a helper for [add_word] that deals with 
adding a character to a list of nodes, aka the children of another node*)
and update_children children word =
  if word = "" then children else
    let curchar = String.sub word 0 1 in 
    let tail = String.sub word 1 ((String.length word) - 1) in 
    match children with
    | (Node (c, childs, is_word))::xs -> if c = curchar then
    (add_word_help (Node (c, childs, String.length word = 1 || is_word)) word)
    ::xs else
    (Node (c, childs, is_word))::(update_children xs word)
    | [Leaf]
    | [] -> [(Node (curchar, update_children [Leaf] tail, 
    String.length word = 1))]
    | _ -> failwith "Function does not accept Head as input"

let add_word trie word = 
  let word = String.lowercase_ascii word in
  match trie with
  | Head  (children) -> Head (update_children children word)
  | _ -> add_word_help trie word

(** [add_words trie words] adds every word in [words] to [trie]*)
let rec add_words trie words =
  match words with
  | [] -> trie
  | x::xs -> add_words (add_word trie x) xs

(** [read_words trie channel] reads every single word from a the file such
  that open_in file is [channel] and adds it to a trie*)
let rec read_words trie channel =
  let to_add = try input_line channel with
    | End_of_file -> "\n"
  in
  if to_add = "\n" then trie else
    let trie = add_word trie to_add in
    read_words trie channel;;

let add_words_from_file (filename:string) : t =
  read_words empty (open_in filename);;

(** [contains_help trie word] is a helper function for 
  [contains] that deals with a single node, passing its children, 
  if any, to [check_charf]*)
let rec contains_help trie word =
  let tail = String.sub word 1 ((String.length word) - 1) in 
  match trie with
  | Node (c, children, is_word) -> if String.length tail < 1 then
  is_word && check_char children tail else
  check_char children tail
  | Leaf -> false
  | Head (_) -> failwith "Function does not accept Head as input"
(** [check_char children word] is a helper function for [contains]
    that deals with lists of nodes, aka the children of a single node*)
and check_char children word =
  if word = "" then true else
  let curchar = String.sub word 0 1 in 
  match children with
  | (Node (c, childs, is_word))::xs ->
  if c = curchar then 
  contains_help (Node (c, childs, is_word)) word
  else check_char xs word
  | [Leaf]
  | [] -> false
  | _ -> failwith "Trie invalid"

let contains (trie:t) (word:string) : bool =
  let word = String.lowercase_ascii word in 
  match trie with
  | Head (children) -> check_char children word
  | _ -> contains_help trie word

(** [contains_prefix_help trie pref] is a helper function for 
  [contains_prefix] that deals with a single node, passing its children, 
  if any, to [check_pref]*)
let rec contains_prefix_help trie pref =
  let tail = String.sub pref 1 ((String.length pref) - 1) in 
  match trie with
  | Node (c, children, is_word) -> check_pref children tail
  | Leaf -> false
  | Head (_) -> failwith "Function does not accept Head as input"
  (** [check_pref children pref] is a helper function for [contains_prefix]
    that deals with lists of nodes, aka the children of a single node*)
and check_pref children pref =
  if pref = "" then true else
  let curchar = String.sub pref 0 1 in 
  match children with
  | (Node (c, childs, is_word))::xs ->
  if c = curchar then 
  contains_prefix_help (Node (c, childs, is_word)) pref
  else check_pref xs pref
  | [Leaf]
  | [] -> false
  | _ -> failwith "Trie invalid"

let contains_prefix trie pref =
  let pref = String.lowercase_ascii pref in 
  match trie with
  | Head (children) -> check_pref children pref
  | _ -> contains_help trie pref

(** [to_list_help trie acc curword] is a helper for [to_list] that deals with
    adding a single node to a list of words*)
let rec to_list_help trie acc curword =
  match trie with
  | Node (c, children, is_word) -> (* let curword = curword ^ c in *)
  if is_word then children_to_list children (curword::acc) curword else
  children_to_list children acc curword
  | Leaf -> acc
  | _ -> failwith "Invalid Trie"

(** [children_to_list children acc curword] is a helper for [to_list] that
    deals with adding a list of nodes to a list of words*)
and children_to_list children acc curword =
  match children with
  | (Node (c, childs, is_word))::xs -> let newword = curword ^ c in 
  let acc = to_list_help (Node (c, childs, is_word)) acc newword in 
  children_to_list xs acc curword
  | [Leaf]
  | [] -> acc
  | _ -> failwith "Invalid Trie"

let to_list (trie:t) : string list =
  match trie with
  | Head (children) -> children_to_list children [] ""
  | _ -> failwith "Invalid Trie"
