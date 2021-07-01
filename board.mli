open Trie
(**
   Representation of static board data.

   This module represents the board used for the game. It handles generating
   a board, checking if a word is valid in a board, and computing scores.
*)

(** The abstract type of values representing boards. *)
type t

(** The size of the square board, the height and width. *)
type size = int

(** The color of a letter. If the letter is found on the board and part of an
    english word that the user entered, it is green. If it is found on the board
    and not part of an english word, it is red. Default is white.  *)
type color = Red | Green | White

(** The file name of file with required data. *)
type filename = string

(** Raised when board size [size] is not allowed. *)
exception InvalidSize of size

(** Raised when file [file] is not formatted correctly. *)
exception InvalidFile of filename

(** The type of a board, where standard is configured using 6 sided die for
    each board position, and where random randomnly choose a letter from the
    alphabet for each board element. *)
type board_type =
  | Standard of size
  | Random of size
  | Custom_die of (filename * size)
  | Custom_board of (filename * size)

(** [generate size] generates a board of dimensions [size] x [size]. It uses
    the standard die to decide each letter on the board.
    @param size is the size of the board you want to generate
    @return board is the board created
    Raises: [InvalidSize] if a board of the specified size cannot be generated.
    [InvalidFile] if the file provided is not correctly formatted.
    Requires: Standard board must be of size 4, random boards can be of any
    size.
    Example: [generate (Standard 4)]*)
val generate : board_type -> t


(** [size b] returns the height/width of the board.
    @param b is a board.
    @return the size of the board.
    Raises: Does not raise any exceptions.
    Requires: The board must be square.
    Example: [size board = 4] *)
val size: t -> int

(** [is_valid_word w b] check whether word [w] can be formed on board [b]
    both using horizontal, vertical, and diagonal directions, and whether
    it is contained in the english dictionary.
    The word can appear backward on the board.
    @param w is the word that is being check as valid.
    @param b is the board in which the word is being found.
    @return is a bool true/false indicating whether the word was found.
    Raises: Does not raise any exceptions
    Example: [is_valid_word "hello" b = true]*)
val is_valid_word: string -> t -> bool

(** [word_score w b] is the score of the word w in board b. The score is
    equivalent to the word length.
    @param w is the word whose score is desired.
    @param b is the board in which you want to check its score.
    @return an integer denoting the score of the word.
    Requires: word w must be a valid word in board b.
    Raises: No Exceptions
    Example: [word_score "hello" b = 5]*)
val word_score: string -> t -> int

(** [get_possible_words b] is a list of all the possible words that the board
    can form.
    @param b is the board you want to form words from
    @return a string list containing all possible words
    Raises: No Exceptions
    Example: [get_possible_words b = ["hello;"; "hi"; "nice"]] *)
val get_possible_words: t -> string list

(** [format b size] is a printing function suitable for use
     with the toplevel's [#install_printer] directive.
     It outputs a textual representation of a board
     on the given formatter.
     @param b is the board you want to print out
     @param size is the height/width of the board you want to print out.
     @return unit the standard
     Requires: Board b must be square.
     Raises: No Exceptions
     Example: [format b 4] prints out the board to the console. *)
val format : t -> size -> unit

(** [testing_board1 ()] returns a Standard board for testing purposes.
    @param unit the standard
    @return board for testing
    Raises: No Exceptions
    Example: [testing_board1 ()] returns a board.*)
val testing_board1: unit -> t


(** [testing_board2 ()] returns a Standard board for testing purposes.
    @param unit the standard
    @return board for testing
    Raises: No Exceptions
    Example: [testing_board1 ()] returns a board.*)
val testing_board2: unit -> t


(** [testing_board3 ()] returns a Standard board for testing purposes.
    @param unit the standard
    @return board for testing
    Raises: No Exceptions
    Example: [testing_board1 ()] returns a board.*)
val testing_board3: unit -> t

(** [nodes_and_colors word board] returns a list of letter and color tuples,
    with the letter being that of the node on the board, and color being either
    Green, Red, or White. This is to highlight the nodes on the board that make
    up the word passed in to this method. If the word is found on the board but
    is not an English word, it is colored red. If the word is found on the
    board and is an English word, it is colored green. If the word is not found
    on the board, it is colored white just like the rest of the letters on the
    board.
    @param word the word needed to be colored on the board
    @param board for the word to be searched on
    @return letter and color tuple list to help main know what letters to color
    Raises: No Exceptions
    Example: [nodes_and_colors "bad" (testing_board3 ())] returns [('B', Green);
     ('A', Green); ('T', White); ('D', Green); ('E', White); ('L', White); ('S',
     White); ('N', White); ('E', White)]
*)
val nodes_and_colors : string -> t -> (char*color) list
