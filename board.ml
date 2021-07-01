open Trie

type node = {
  letter : char;
  position : int;
}
type size = int

type color = Red | Green | White

type filename = string

exception InvalidSize of size

exception InvalidFile of filename

type t = {
  nodes : node list;
  words : Trie.t;
}

type board_type =
  | Standard of size
  | Random of size
  | Custom_die of (string * size)
  | Custom_board of (string * size)

(* used to generate random boards *)
let consonants = [|'B';'C';'D';'F';'G';'H';'J';'K';'L';'M';
                   'N';'P';'Q';'R';'S';'T';'V';'W';'X';'Y';'Z'|]
let vowels = [|'A';'E';'I';'O';'U'|]

(* a Trie of all the possible english words that can be entered in the game *)
let english_words = add_words_from_file "usa.txt"

(* mapping of letters to scrabble point system *)
let scrabble_points = [(1, ['A';'E';'I';'O';'U';'L';'N';'S';'T';'R']);
                       (2, ['D';'G']);
                       (3, ['B';'C';'M';'P']);
                       (4, ['F';'H';'V';'W';'Y']);
                       (5, ['K']);
                       (8, ['J';'K']);
                       (10, ['Q';'Z']) ]

(** [get_letter_score] gets the scrabble points of a letter, which we are using
    in this game to be how we calculate word scores.  *)
let get_letter_score (c:char) : int =
  let rec helper lst =
    match lst with
    | [] -> 0
    | (score, [])::t -> helper t
    | (score, letters)::t -> if List.mem (Char.uppercase_ascii c) letters
      then score else helper t
  in helper scrabble_points

(** [create_die_arr f s] creates an array of character arrays of die as listed
    in filename [f] of length [s]*[s]. This array contains the 6 sided
    configurations of all die for the [s]*[s] board as specified by file [f],
    where [f] is a .txt file formatted as specified in the instrunctions. *)
let create_die_arr (filename:string) (board_dim:int) : (char array) array =
  if Filename.check_suffix filename ".txt" then begin
    let open_file = (try (open_in filename) with
        | Sys_error f -> raise (InvalidFile f)) in
    let rec read_loop chan file (acc: (char array) list) (acc2: char list) =
      match (input_char chan) with
      |exception End_of_file -> begin
          if (List.length acc = ((board_dim * board_dim) - 1)) then
            (Array.of_list (List.rev acc2)::acc)
          else raise (InvalidSize board_dim)
        end
      | '\n' -> if List.length acc2 = 6 then begin
          read_loop chan file (Array.of_list (List.rev acc2)::acc) []
        end else raise (InvalidFile(filename))
      | l  -> read_loop chan file acc (l::acc2)  in
    Array.of_list (List.rev (read_loop open_file filename [] []))
  end else raise (InvalidFile(filename))

(** [create_node l p] creates a node, to be placed in a board, with
    the field letter set as l, position set as p, and points set to
    a randomnly generated number from 1-5. *)
let create_node (letter:char) (position:int) : node =
  {letter= letter; position=position}

(** [random_char] generates a random character from the English alphabet.
    A 4x4 standard die has bound of 6. *)
let random_char die_arr (bound:int) =
  let random_int = Random.int bound in
  Array.get die_arr random_int

(** [size] gets the size of a board as an integer. For example, 4x4 board's
    size is 4.  *)
let size board =
  int_of_float (sqrt (float_of_int (List.length (board.nodes))))

(** [generate_random size] generates a random board of dimensions size x size.*)
let generate_random (size:int) =
  let rec create_board (index:int) (board:t) =
    if index < 0 then board else begin
      let vowels_or_consonants = Random.int 3 in
      if vowels_or_consonants = 0 then begin
        let letter = Array.get consonants (Random.int (Array.length consonants))
        in
        let node = create_node letter index in
        create_board (index-1) {nodes=(node::board.nodes);words=board.words}
      end
      else begin
        let letter = Array.get vowels (Random.int (Array.length vowels)) in
        let node = create_node letter index in
        create_board (index-1) {nodes=(node::board.nodes);words=board.words}
      end
    end
  in create_board ((size*size)-1) {nodes=[];words=Trie.empty}

(** [generate_standard arr s] generates a [s]*[s] standard board using
    preconfigured die in arr.*)
let generate_standard die_arr size =
  let () = Random.self_init() in
  let rec create_board (index:int) (board:t) =
    if index < 0 then board else begin
      let die = Array.get die_arr index in
      let letter = random_char die 6 in
      let node = create_node letter index in
      create_board (index-1) {nodes=(node::board.nodes);words=board.words}
    end
  in create_board (size*size - 1) {nodes=[];words=Trie.empty}

(** [generate_custom f s] generates a [s]*[s] board using the board setup as
    described in file [f].*)
let generate_custom (filename:filename) (board_dim:int): t =
  if Filename.check_suffix filename ".txt" then begin
    let open_file = (try (open_in filename) with
        | Sys_error f -> raise (InvalidFile f)) in
    let rec read_loop chan file (acc: node list) (pos: int) : node list =
      match (input_char chan) with
      |exception End_of_file -> begin
          if (pos = board_dim*board_dim) then acc
          else raise (InvalidSize board_dim)
        end
      | '\n' -> read_loop chan file acc pos
      | l -> if l = ' ' then read_loop chan file acc pos else begin
          (read_loop chan file ((create_node l pos)::acc) (pos+1))
        end in
    let node_list = (List.rev(read_loop open_file filename [] 0)) in
    {nodes=node_list; words=Trie.empty}
  end else raise (InvalidFile filename)

(** [positions of neighbors node b] returns the list of positions of
    neighboring elements on the board. *)
let positions_of_neighbors (node:node) (board:t) : int list =
  let size = int_of_float
      (Pervasives.sqrt ((float_of_int ((List.length board.nodes)+1)))) in
  let pos = node.position in
  if (pos mod size) = 0 then begin
    List.filter (fun x -> x >= 0 && x < (size*size))
      [pos+size;pos-size;pos+1;pos+size+1;pos-size+1]
  end
  else if (pos mod size) = (size-1) then begin
    List.filter (fun x -> x >= 0 && x < (size*size))
      [pos-size;pos-1;pos-size-1;pos+size-1;pos+size]
  end
  else if (pos / size) = 0 then begin
    List.filter (fun x -> x >= 0 && x < (size*size))
      [pos+size;pos-1;pos+1;pos+size+1;pos+size-1]
  end
  else if (pos / size) = (size-1) then begin
    List.filter (fun x -> x >= 0 && x < (size*size))
      [pos-size;pos-size-1;pos+1;pos-1;pos-size+1]
  end
  else begin
    List.filter (fun x -> x >= 0 && x < (size*size))
      [pos-size;pos-size-1;pos+size-1;pos+1;pos-1;pos+size+1;pos-size+1;
       pos+size]
  end

(** [get_node index board] returns the node with index [index] in [board]'s
    node list.*)
let get_node (index:int) (board:t) : node =
  List.nth board.nodes index

(** [letters_of_neighbors pos_list b] returns the list of letters of all nodes
    with the corresponding positions in pos_list *)
let letters_of_neighbors (pos_list:int list) (board:t) : char list =
  List.map (fun x -> let node = get_node x board in node.letter) pos_list

(** [nodes_of_neighbors] gets the neighboring nodes given a node on the board.*)
let nodes_of_neighbors (node:node) (board:t) : node list =
  let neighbor_positions = positions_of_neighbors node board in
  let rec rec_nodes positions acc =
    match positions with
    | [] -> acc
    | h::t -> rec_nodes t ((get_node h board)::acc)
  in rec_nodes neighbor_positions []

(** [get_string_from_nodes] when given a list of nodes, will return the string
    formed by the nodes (but in reverse order - the reason for this is to
    optimize accessing nodes from the front as prepending is faster than
    appending) *)
let get_string_from_nodes (nodes:node list) : string =
  let char_array = List.map (fun x -> x.letter) nodes in
  let string_array = List.map (fun x -> Char.escaped x) char_array in
  List.fold_left (fun acc x -> x^acc) "" string_array

(** [get_BFS_neighbors] returns a sequence of nodes; gets neighbors of last
    node in the sequence, and then subtracts from these neighbors any nodes that
    are already present in the sequence. This is used only for the BFS
    traversal;which is finding all the possible words on a board in
    [populate_board] *)
let get_BFS_neighbors (nodes:node list) board : node list =
  match nodes with
  | [] -> []
  | last_node::t -> let neighbor_nodes = nodes_of_neighbors last_node board in
    List.filter (fun x -> not (List.mem x nodes)) neighbor_nodes

(** [process_neighbors] is used for the BFS traversal to find all the possible
    words on a board. This is only part of the algorithm; it only adds one
    letter to the node_lst, for example, if the queue passed in "ABC" then this
    would only process one valid neighboring character, like "ABCD," "ABCE",
    "ABCF" ... and so on. *)
let rec process_neighbors q (node_lst:node list) (str:string) (board:t)
    (words_acc:Trie.t) (neighbor_nodes:node list) : Trie.t =
  match neighbor_nodes with
  | [] -> process_queue q board words_acc
  | n_node::t -> begin
      let new_nodes = n_node::node_lst in
      let new_str = str^Char.escaped n_node.letter in
      if Trie.contains english_words new_str = true then
        let words_acc = Trie.add_word words_acc new_str in
        let () = Queue.add new_nodes q in
        process_neighbors q node_lst str board words_acc t
      else let () = Queue.add new_nodes q in
        process_neighbors q node_lst str board words_acc t
    end

(** [process_queue] is used for the BFS traversal to find all the possible
    words on a board. It processes the entire queue holding all of the possible
    node sequences; essentially a loop for while the queue isn't empty *)
and process_queue q (board:t) (words_acc:Trie.t) : Trie.t =
  if Queue.is_empty q then words_acc else
    let node_lst = Queue.take q in
    let neighbor_nodes = get_BFS_neighbors node_lst board in
    let new_str = get_string_from_nodes node_lst in
    if Trie.contains_prefix english_words new_str = false
    then process_queue q board words_acc
    else
    if Trie.contains english_words new_str = true then
      let words_acc = Trie.add_word words_acc new_str in
      process_neighbors q node_lst new_str board words_acc neighbor_nodes
    else process_neighbors q node_lst new_str board words_acc neighbor_nodes


(** [process_node] is used for the BFS traversal to find all the possible words
    on a board. It returns a list of valid english words starting with the
    character in the node parameter. *)
let rec process_node (nodes:(node list) list) (board:t) (words_acc:Trie.t):
  Trie.t =
  let q = Queue.create () in
  match List.map (fun x -> Queue.add x q) nodes with
  | [] -> process_queue q board words_acc
  | h::t -> process_queue q board words_acc

(** [populate_board_words] is a helper function for [populate_board] to start
    the BFS algorithm on the ndoes of the board. Since [process_node] takes in a
    list of nodes as each vertex for its BFS traversal, this function
    accumulates the [node0;node1;node2] into [[node0];[node1];[node2]] *)
let rec populate_board_words (nodes:node list) (board:t) (word_list:Trie.t) :
  Trie.t =
  let nodes_for_q = List.fold_left (fun acc x -> [x]::acc) [] nodes in
  process_node nodes_for_q board Trie.empty

(** [populate_board] actually populates the board with all the possible words
    that it can form from a BFS traversal. *)
let rec populate_board (board:t) : t =
  let trie = populate_board_words board.nodes board Trie.empty in
  {nodes=board.nodes;words=trie}

(** [generate] creates a board given a board type.  *)
let generate (board_type:board_type) : t =
  let () = Random.self_init () in
  match board_type with
  | Standard size -> if size = 4 then begin
      populate_board (generate_standard (create_die_arr "4x4.txt" 4) 4)
    end else if size = 5 then begin
      populate_board (generate_standard (create_die_arr "5x5.txt" 5) 5)
    end else raise (InvalidSize size)
  | Random size -> populate_board ((generate_random size))
  | Custom_die(file, size) -> if size >= 4 && size <= 20 then begin
      populate_board (generate_standard (create_die_arr file size) size)
    end else raise (InvalidSize size)
  | Custom_board(file, size) -> if size >=4 && size <=20 then begin
      populate_board (generate_custom file (size)) end else
      raise(InvalidSize size)

(** [get_node_letter l lst acc] filters the node list [lst] and returns a
    only the nodes containing letter [l]. *)
let rec get_node_letter letter board_nodes acc =
  match board_nodes with
  | [] -> acc
  | h::t -> if (h.letter = letter) then (get_node_letter letter t (h::acc))
    else (get_node_letter letter t acc)

(** [validate_node node idx b str] does a depth first search for word [str]
    in board [b], starting from node [node]. Returns true if [str] was found
    looking horizontally, vertically, and diagonally searching from the starting
    point, and returns false if not. *)
let rec validate_node (node:node) (index:int) (board:t) (str:string)
    (visited_pos:int list) : bool =
  let new_visited_pos = (node.position::visited_pos) in
  if index = String.length str - 1 then true else begin
    let next_letter = str.[index + 1] in
    let neighbor_positions = positions_of_neighbors node board in
    let possible_neighbors = List.filter
        (fun x -> (get_node x board).letter = next_letter) neighbor_positions in
    let rec process_nlist neighbor_lst acc = match neighbor_lst with
      | [] -> acc
      | h::t -> begin
          if (List.mem h new_visited_pos = false) then begin
            process_nlist t (acc || (validate_node (get_node h board)
                                       (index + 1) board str new_visited_pos))
          end else process_nlist t acc
        end in
    (process_nlist possible_neighbors false)
  end

(** [is_word_on_board] takes a word and checks if it exists on the board. *)
let is_word_on_board (word:string) (board:t) : bool =
  let upper_word = String.uppercase_ascii word in
  let first_char = upper_word.[0] in
  let nodes_fst_letter = (get_node_letter first_char board.nodes []) in
  let rec node_loop lst acc =
    match lst with
    | [] -> acc
    | h :: t -> node_loop t (acc || validate_node h 0 board upper_word []) in
  (node_loop nodes_fst_letter false)

(** [is_valid_word w b] returns true if [w] is contained in the english
    dictionary and could be formed following the rules on board [b], and false
    otherwise. *)
let is_valid_word (word:string) (board:t) : bool =
  if Trie.contains english_words (String.lowercase_ascii word) then
    is_word_on_board word board else false

(** [string_to_chars] takes in a string and converts it to a list of
    characters. *)
let string_to_chars (word:string) : char list =
  let rec helper index acc =
    if index < 0 then acc
    else helper (index-1) (word.[index]::acc) in
  helper (String.length word - 1) []

(** [word_score] computes the score of a word in the context of a board. *)
let word_score (word:string) (board:t) : int =
  let char_lst = string_to_chars word in
  List.fold_left (fun acc c -> get_letter_score c + acc) 0 char_lst

(** [get_possible_words] gets all of the possible words of a board, which is
    contained in board.words (which we populate via the [populate_board] method)
*)
let get_possible_words (board:t) : string list =
  Trie.to_list (board.words)

(*helps format. Does top boarder of board*)
let rec hborder size =
  match size with
  |0 -> ()
  |n -> print_string"-"; hborder (size-1)

(*helps format. Does middle of board*)
let rec mid_board board size left=
  match board.nodes with
  | [] -> ()
  | h::t -> begin
      if left = 1 then
        begin
          if t = [] then
            let () = if (h.position + 1) mod size = 0 then begin
                ANSITerminal.(print_string [Underlined]
                                (Char.escaped h.letter)); print_string "|\n"
              end else
                ANSITerminal.(print_string [Underlined]
                                ((Char.escaped h.letter) ^ " ")) in
            (mid_board {board with nodes=t} size (left-1))
          else
            let () = if (h.position + 1) mod size = 0 then begin
                (print_char h.letter ; print_string " " ; print_string "|\n|")
              end else
                (print_char h.letter; print_string " ") in
            (mid_board {board with nodes=t} size left)
        end
      else
        begin
          if t = [] then
            let () = if (h.position + 1) mod size = 0 then begin
                (print_char h.letter ; print_string " " ; print_string "|\n")
              end else
                (print_char h.letter; print_string " ") in
            (mid_board {board with nodes=t} size (left-1))
          else
            let () = if (h.position + 1) mod size = 0 then begin
                (print_char h.letter ; print_string "|\n|")
              end else
                (print_char h.letter; print_string " ") in
            (mid_board {board with nodes=t} size left)
        end
    end
(** [format] formats the board in string form.  *)
let rec format board size =
  hborder (2*size+2);
  print_string "\n|";
  mid_board board size size;
  hborder (2*size+2)

(** [validate_node2 node idx b str pos lst] does a depth first search for word
    [str] in board [b], starting from node [node]. Returns the list of nodes
    through which word [str] is formed if word can be formed on board;
    if word cannot be formed, returns an empty list. The word can be formed
    looking horizontally, vertically, and diagonally searching from the starting
    point. *)
let rec validate_node2 (node:node) (index:int) (board:t) (str:string)
    (visited_pos:int list) (node_lst:node list): node list =
  let new_visited_pos = (node.position::visited_pos) in
  if index = (String.length str) - 1 then node_lst
  else begin
    let next_letter = str.[index + 1] in
    let neighbor_positions = positions_of_neighbors node board in
    let possible_neighbors = List.filter
        (fun x -> (get_node x board).letter = next_letter) neighbor_positions in
    let rec process_nlist neighbor_lst = match neighbor_lst with
      | [] -> []
      | h::t -> begin
          if (List.mem h new_visited_pos = false) then begin
            let result = (validate_node2 (get_node h board)
                            (index + 1) board str new_visited_pos
                            ((get_node h board)::node_lst)) in
            if (List.length result  = index + 1) then result
            else if String.length str = List.length result then result
            else process_nlist t
          end
          else process_nlist t
        end in (process_nlist possible_neighbors)
  end

(** [is_valid_word2 w b] returns the node list of letters used to form the word
    [w] on board [b]. *)
let is_valid_word2 (word:string) (board:t) : node list =
  if String.length word = 0 then [] else begin
    let upper_word = String.uppercase_ascii word in
    let first_char = upper_word.[0] in
    let nodes_fst_letter = (get_node_letter first_char board.nodes []) in
    let rec node_loop lst =
      match lst with
      | [] -> []
      | h :: t -> begin
          let result = validate_node2 h 0 board upper_word [] [h] in
          if ((List.length result)) = String.length word
          then result else node_loop t
        end
    in (List.rev (node_loop nodes_fst_letter))
  end

(** [nodes_and_colors word board] returns a list of letter and color tuples,
    with the letter being that of the node on the board, and color being either
    Green, Red, or White. This is to highlight the nodes on the board that make
    up the word passed in to this method. *)
let nodes_and_colors (word:string) (board:t) : (char*color) list =
  let helper =
    let nodes_of_word = is_valid_word2 word board in
    if (List.length nodes_of_word > 0) then
      if Trie.contains english_words word then
        List.fold_left (fun acc node -> if List.mem node nodes_of_word then
                           ((node.letter, Green)::acc) else
                           ((node.letter, White)::acc) )
          [] board.nodes
      else
        List.fold_left (fun acc node -> if List.mem node nodes_of_word then
                           ((node.letter, Red)::acc) else
                           ((node.letter, White)::acc) )
          [] board.nodes
    else
      List.fold_left (fun acc node -> (node.letter, White)::acc) [] board.nodes
  in List.rev helper


(** Used to help test *)
let testing_board1 () =
  let node_list = [{letter='I'; position=0};
                   {letter='F'; position=1};
                   {letter='D'; position=2};
                   {letter='D'; position=3};
                   {letter='M'; position=4};
                   {letter='S'; position=5};
                   {letter='A'; position=6};
                   {letter='Y'; position=7};
                   {letter='Q'; position=8};
                   {letter='P'; position=9};
                   {letter='T'; position=10};
                   {letter='R'; position=11};
                   {letter='N'; position=12};
                   {letter='A'; position=13};
                   {letter='I'; position=14};
                   {letter='C'; position=15};] in
  {nodes=node_list; words=Trie.empty}

let testing_board2 () =
  let node_list = [{letter='B'; position=0};
                   {letter='A'; position=1}; 
                   {letter='D'; position=2};
                   {letter='E'; position=3}] in
  let b = {nodes=node_list; words=Trie.empty} in
  populate_board b

let node0 = {letter='B'; position=0}
let node1 = {letter='A'; position=1}
let node2 = {letter='T'; position=2}
let node3 = {letter='D'; position=3}
let node4 = {letter='E'; position=4}
let node5 = {letter='L'; position=5}
let node6 = {letter='S'; position=6}
let node7 = {letter='N'; position=7}
let node8 = {letter='E'; position=8}

let testing_board3 () =
  let node_list = [node0;node1;node2;node3;node4;node5;node6;node7;node8] in
  let b = {nodes=node_list; words=Trie.empty} in
  populate_board b
