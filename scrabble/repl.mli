(* A [repl] is repl module. waits for the user input *)

(* [main_manu] : unit -> unit
 * [main_manu] displays a main manu of the game. Waits for the string input
 * and moves on to next window based on the input
 * [play] -> call initialize_game
 * [help] -> displays tutorial and waits for the next string
 * [quit] -> quit the game *)
val main_menu : unit -> unit
