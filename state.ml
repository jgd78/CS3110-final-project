(* State represents the dynamic state of a game *)
open Board
open Trie

(** type to represent a state *)
type t = {
  board : Board.t;
  score : int;
  words : Trie.t;
  leaderboard: (int * (int list)) list;
  hints_left : int;
  start_time : float;
  last_times : float list;
}

(** [init] initializes a state given a board. *)
let init (board:Board.t) (leaderboard): t = 
  {board=board;score=0;words=Trie.empty;leaderboard = leaderboard;
   hints_left=3;start_time=Unix.time ();
   last_times=[]}


(** [score] provides the score of the list of words that the player has been 
    entering to play the board in the state.  *)
let score (state:t) : int = 
  state.score

(** [board] provides the board that the state has been keeping track for the 
    player.  *)
let board (state:t) : Board.t = 
  state.board

(** [words] provides the list of words that the player has been entering to 
    play the game.  *)
let words (state:t) : string list =
  Trie.to_list state.words

let get_random lst =
  Random.self_init ();
  let index = Random.int (List.length lst) in 
  List.nth lst index

let rec get_difference lst1 lst2 =
  match lst1 with
  | [] -> []
  | x::xs -> if List.mem x lst2 then get_difference xs lst2 else
      x::(get_difference xs lst2)

let rec hintify_help chars =
  match chars with
  | [] -> ""
  | x::xs -> x ^ mystery_spaces xs
and mystery_spaces remaining =
  match remaining with
  | [] -> ""
  | x::xs -> " _" ^ mystery_spaces xs

let rec string_to_list str =
  match str with
  | "" -> []
  | _ -> (String.sub str 0 1)::
         (string_to_list (String.sub str 1 ((String.length str) - 1)))

let hintify str =
  hintify_help (string_to_list str)

(** [hint state] is a random string based on a word the user has not yet found
  in the game represented by [state]. The format is the first letter of the
  word followed by underscores with spaces between each character*)
let hint (state:t) : (t * string) =
  if state.hints_left <= 0 then failwith "No hints remaining" else
    let updated_state = 
      {board = state.board; score = state.score - 5; words = state.words;
       leaderboard = state.leaderboard; hints_left = state.hints_left - 1;
       start_time = state.start_time; last_times = state.last_times} in 
    let hinted =
      get_random (get_difference (Board.get_possible_words state.board) 
                    (Trie.to_list state.words))
    in 
    (updated_state, hintify hinted)

(** [num_hints state] is the number of hints the user has left*)
let num_hints state =
  state.hints_left

let leaderboard (state:t) = 
  state.leaderboard

let rec get_dif times =
  match times with
  | [] -> 1000.0
  | x::xs -> begin
      match xs with
      | [] -> 1000.0
      | y::ys -> x -. y
    end

let fast_enough times =
  match times with
  | [] -> false
  | x::xs -> let time_between = get_dif times in 
    time_between < 5.0

let multiplier score = 
  score * 3

(** [update] updates a state with a newly added word by validating if the word 
    is a valid word (on the board and in the English language - this is 
    dependent upon Board's implementation of [is_valid_word]). *)
let update (state:t) (word:string) : t =
  let times = (Unix.time ())::state.last_times in 
  let new_words = Trie.add_word state.words word in 
  let word_score = Board.word_score word state.board in 
  let new_score = if fast_enough times 
    then state.score + (multiplier word_score) else state.score + word_score in
  {board=state.board;score=new_score;words=new_words; 
   leaderboard = (leaderboard state);
   hints_left=state.hints_left;start_time=state.start_time;
   last_times=(Unix.time ())::(state.last_times)}

(** [key_compare x y] is a comparison function to sort the leaderboard.*)
let key_compare x y = 
  if fst x < fst y then -1
  else if fst x > fst y then 1
  else 0 

(** [print_score_list lst] is a helper function to print the
    score list [lst] for each size within a leaderboard. *)
let rec print_score_list = function
  | [] -> ()
  | h::t -> print_int h; print_string "\n"; print_score_list t

(** [print_leaderboard_helper l] is a helper function to print the
    leaderboard [l]. *)
let rec print_leaderboard_helper l = 
  match l with
  | [] -> ()
  | (size, score_lst)::t -> let str_size = string_of_int size in 
    let size_string = "\n"^str_size^"x"^str_size^":\n" in 
    ANSITerminal.(print_string [green] size_string); 
    print_score_list score_lst; 
    print_leaderboard_helper t 

let print_leaderboard l = print_string "\n"; 
  if l = [] then () else 
    (ANSITerminal.(print_string [red] "******LEADERBOARD********"); 
     print_leaderboard_helper (List.rev (List.sort key_compare l)))

(** [add_leaderboard_help l] is a helper function used by add_leaderboard
    to add to the leaderboard [old_leaderboard] *)
let rec add_leaderboard_help (old_leaderboard) (score:int list) (size:int) 
    (acc) =  
  match old_leaderboard with 
  | [] -> ((size, score)::acc)
  | (h_size, lst)::t -> if h_size = size then begin
      let new_lst = (score@lst) in 
      let sorted_list = List.rev (List.sort compare new_lst) in 
      add_leaderboard_help t sorted_list size (acc) 
    end else add_leaderboard_help t score size ((h_size, lst)::acc)


let rec add_leaderboard (old_leaderboard) (score:int list) (size:int) =  
  add_leaderboard_help old_leaderboard score size []


