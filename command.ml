open Card

type object_phrase = string list

type command =
  | Draw of string 
  | Discard of object_phrase
  | Stop

exception Empty
exception Malformed

let process str =
  String.trim str
  |> String.split_on_char ' '
  |> List.filter (fun f -> f <> "")

let parse str =
  (* returns string list of string command *)
  if str = "" || str = String.make (String.length str) ' ' then raise (Empty) 
  else match (process str) with
    | "draw" :: t -> if t != []
      then Draw (List.hd t)
      else raise (Malformed)
    | "discard" :: t -> if t != []
      then Discard t
      else raise (Malformed)
    | "stop" :: t -> if t = []
      then Stop
      else raise (Malformed)
    | _ -> raise (Malformed)