open Data
open API

type state
type move
type dictionary

val search : string -> dictionary -> bool
val validate : move -> state -> bool
