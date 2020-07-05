open Card
(**
   Parsing of player commands. Adapted from A2.
*)

(** The type [object_phrase] represents the object phrase that can be part of a 
    player command.  Each element of the list represents a word of the object 
    phrase, where a {i word} is defined as a consecutive sequence of non-space 
    characters.  Thus, no element of the list should contain any leading,
    internal, or trailing spaces.  The list is in the same order as the words 
    in the original player command.  For example:
    - If the player command is ["discard queen of hearts"], then the object
      phrase is ["queen"; "of"; "hearts"]

    An [object_phrase] is not permitted to be the empty list. *)
type object_phrase = string list

(** The type [command] represents a player command that is decomposed
    into a verb and possibly an string object phrase. *)
type command =
  | Draw of string 
  | Discard of object_phrase
  | Stop


(** Raised when an empty command is parsed. *)
exception Empty

(** Raised when a malformed command is encountered. *)
exception Malformed

(** [parse str] parses a player's input into a [command], as follows. The first
    word (i.e., consecutive sequence of non-space characters) of [str] becomes 
    the verb.
    Examples: 
    - [parse "discard queen of hearts"] is [Discard {rank = "Queen"; suit = Hearts; value = 10}]
    - [parse "stop"] is [Stop]. 

    Requires: [str] contains only alphanumeric (A-Z, a-z, 0-9) and space 
    characters (only ASCII character code 32; not tabs or newlines, etc.).

    Raises: [Empty] if [str] is the empty string or contains only spaces. 

    Raises: [Malformed] if the command is malformed. A command
    is {i malformed} if the verb is neither "draw" nor "discard" nor "stop",
    or if the verb is "stop" and there is a non-empty object phrase,
    or if the verb is "draw" or "discard" and there is an empty object phrase.*)
val parse : string -> command