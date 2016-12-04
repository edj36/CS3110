(* [is_valid] is a boolean indicating the validity of [move] in [state]
 * [shuffle] -> true
 * [swapall] -> true
 * [end] -> true
 * [pass] -> true
 * [swapsome] -> true if all the letters swapping are in the hand
 * [play] -> true if it's valid (see SPEC for [validate]) *)
val is_valid : Data.move -> Data.game_state -> bool

(* [validate] is a bool representation indicating if the move is valid or not
 * check following criteria
 * - the word is made only by the letters in the hands (Done)
 * - check if word fits in the board
 * - the new words is a valid word in dictionary
 * - when it's turn 1, you have to place over (H,8)
 * - new words have to satisfy one of following rules
 *   1, Adding one or more letters to a word or letters already on the board
 *     (only case that you can play 1 letter)
 *   2, Placing a word at right angles to a word already on the board
 *   3, Placing a complete word parallel to a word already played
      so that adjacent letters also form complete words *)
val validate : Data.move -> Data.game_state -> Data.game_state
