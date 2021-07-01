# boggle
Our project is an OCaml version of Facebook’s Word Blitz game, also known as Boggle, but with our own creative tweaks that will 
make the game different and more original. For those who are not familiar with Boggle, it is a word game in which players are 
presented a grid of characters and they are expected to form words given those sequence of characters. These words can be formed 
by looking at neighboring letters in either the horizontal, vertical, or diagonal directions. 

# features
On top of the basic features of Boggle, we added
* a leaderboard
* a time sensitive score (the faster you enter words, the higher score you get)
* a display of all of the possible words that was missed
* the option to import custom die and boards
* hints to find more words
* highlighting of the word found on the board as either green, red, or white

# authors & tasks
## Alanna
* Implemented and specified functions in board.ml and board.mli
* Implemented and specified functions in state.ml and state.mli
* Added better scoring
* Wrote the breadth-first traversal to find all of the possible words on a board
* Wrote the extended depth-first traversal method to find the found nodes corresponding to the word on a board to highlight words found as green or red
* Wrote the formatting method in main 
* Wrote tests for the BFS and node highlighting

## Austin
* Implemented and specified functions in trie.ml and trie.mli
* Added a hints system to the game
* Added a score multiplier
* Added a way to track the user’s average time between words
* Fixed minor scoring and display bugs

## James
* Implemented and specified functions in command.ml and command.mli
* Wrote Main
* Added initial game word art
* Implemented the format function to display correct and incorrect words as green and red, respectively
* Updated commands and improved the UI

## Khyati
* Implemented and specified functions in board.ml and board.mli
* Wrote tests for functions in test.ml
* Added functionality to upload custom die and custom boards in board.mli and board.mli
* Added functionality to create and print leaderboard, and added leaderboard command
* Helped with formatting on display
* Debugged depth first traversal method to find node list on board for a word that can be formed on the board


