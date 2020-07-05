type value = int
type rank = string
type suit = Spades | Hearts | Clubs | Diamonds

type card = {
  rank : rank;
  suit : suit;
  value : value;
}

type deck = card list

exception NoSuchCard

let get_val rank =
  match String.lowercase_ascii rank with
  | "ace" -> 11
  | "king" | "queen" | "jack" -> 10
  | x -> int_of_string x

let suits = [Spades; Hearts; Clubs; Diamonds]

let points c =
  c.value

let suit c =
  c.suit

let ordered_deck = [
  {rank = "2"; suit = Spades; value = 2};
  {rank = "3"; suit = Spades; value = 3};
  {rank = "4"; suit = Spades; value = 4};
  {rank = "5"; suit = Spades; value = 5};
  {rank = "6"; suit = Spades; value = 6};
  {rank = "7"; suit = Spades; value = 7};
  {rank = "8"; suit = Spades; value = 8};
  {rank = "9"; suit = Spades; value = 9};
  {rank = "10"; suit = Spades; value = 10};
  {rank = "Jack"; suit = Spades; value = 10};
  {rank = "Queen"; suit = Spades; value = 10};
  {rank = "King"; suit = Spades; value = 10};
  {rank = "Ace"; suit = Spades; value = 11};

  {rank = "2"; suit = Hearts; value = 2};
  {rank = "3"; suit = Hearts; value = 3};
  {rank = "4"; suit = Hearts; value = 4};
  {rank = "5"; suit = Hearts; value = 5};
  {rank = "6"; suit = Hearts; value = 6};
  {rank = "7"; suit = Hearts; value = 7};
  {rank = "8"; suit = Hearts; value = 8};
  {rank = "9"; suit = Hearts; value = 9};
  {rank = "10"; suit = Hearts; value = 10};
  {rank = "Jack"; suit = Hearts; value = 10};
  {rank = "Queen"; suit = Hearts; value = 10};
  {rank = "King"; suit = Hearts; value = 10};
  {rank = "Ace"; suit = Hearts; value = 11};

  {rank = "2"; suit = Clubs; value = 2};
  {rank = "3"; suit = Clubs; value = 3};
  {rank = "4"; suit = Clubs; value = 4};
  {rank = "5"; suit = Clubs; value = 5};
  {rank = "6"; suit = Clubs; value = 6};
  {rank = "7"; suit = Clubs; value = 7};
  {rank = "8"; suit = Clubs; value = 8};
  {rank = "9"; suit = Clubs; value = 9};
  {rank = "10"; suit = Clubs; value = 10};
  {rank = "Jack"; suit = Clubs; value = 10};
  {rank = "Queen"; suit = Clubs; value = 10};
  {rank = "King"; suit = Clubs; value = 10};
  {rank = "Ace"; suit = Clubs; value = 11};

  {rank = "2"; suit = Diamonds; value = 2};
  {rank = "3"; suit = Diamonds; value = 3};
  {rank = "4"; suit = Diamonds; value = 4};
  {rank = "5"; suit = Diamonds; value = 5};
  {rank = "6"; suit = Diamonds; value = 6};
  {rank = "7"; suit = Diamonds; value = 7};
  {rank = "8"; suit = Diamonds; value = 8};
  {rank = "9"; suit = Diamonds; value = 9};
  {rank = "10"; suit = Diamonds; value = 10};
  {rank = "Jack"; suit = Diamonds; value = 10};
  {rank = "Queen"; suit = Diamonds; value = 10};
  {rank = "King"; suit = Diamonds; value = 10};
  {rank = "Ace"; suit = Diamonds; value = 11};
]

let deck1 = [
  {rank = "9"; suit = Spades; value = 9};
  {rank = "5"; suit = Clubs; value = 5};
  {rank = "Queen"; suit = Hearts; value = 10};
  {rank = "Queen"; suit = Clubs; value = 10};
  {rank = "Jack"; suit = Spades; value = 10};
  {rank = "3"; suit = Diamonds; value = 3};
  {rank = "9"; suit = Hearts; value = 9};
  {rank = "6"; suit = Diamonds; value = 6};
  {rank = "7"; suit = Hearts; value = 7};
  {rank = "Jack"; suit = Diamonds; value = 10};
  {rank = "6"; suit = Clubs; value = 6};
  {rank = "Ace"; suit = Diamonds; value = 11};
  {rank = "Ace"; suit = Clubs; value = 11};
  {rank = "8"; suit = Clubs; value = 8};
  {rank = "Jack"; suit = Clubs; value = 10};
  {rank = "5"; suit = Diamonds; value = 5};
  {rank = "3"; suit = Hearts; value = 3};
  {rank = "2"; suit = Spades; value = 2};
  {rank = "6"; suit = Spades; value = 6};
  {rank = "Ace"; suit = Spades; value = 11};
  {rank = "King"; suit = Hearts; value = 10};
  {rank = "2"; suit = Hearts; value = 2};
  {rank = "3"; suit = Spades; value = 3};
  {rank = "10"; suit = Hearts; value = 10};
  {rank = "8"; suit = Diamonds; value = 8};
  {rank = "5"; suit = Hearts; value = 5};
  {rank = "5"; suit = Spades; value = 5};
  {rank = "Jack"; suit = Hearts; value = 10};
  {rank = "4"; suit = Clubs; value = 4};
  {rank = "Ace"; suit = Hearts; value = 11};
  {rank = "4"; suit = Hearts; value = 4};
  {rank = "9"; suit = Diamonds; value = 9};
  {rank = "7"; suit = Spades; value = 7};
  {rank = "King"; suit = Clubs; value = 10};
  {rank = "4"; suit = Diamonds; value = 4};
  {rank = "7"; suit = Diamonds; value = 7};
  {rank = "King"; suit = Spades; value = 10};
  {rank = "7"; suit = Diamonds; value = 7};
  {rank = "2"; suit = Clubs; value = 2};
  {rank = "3"; suit = Clubs; value = 3};
  {rank = "King"; suit = Diamonds; value = 10};
  {rank = "6"; suit = Hearts; value = 6};
  {rank = "10"; suit = Diamonds; value = 10};
  {rank = "8"; suit = Spades; value = 8};
  {rank = "2"; suit = Diamonds; value = 2};
  {rank = "8"; suit = Hearts; value = 8};
  {rank = "Queen"; suit = Diamonds; value = 10};
  {rank = "9"; suit = Clubs; value = 9};
  {rank = "Queen"; suit = Spades; value = 10};
  {rank = "4"; suit = Spades; value = 4};
  {rank = "10"; suit = Clubs; value = 10};
  {rank = "10"; suit = Spades; value = 10};
]


let spades = [
  {rank = "2"; suit = Spades; value = 2};
  {rank = "3"; suit = Spades; value = 3};
  {rank = "4"; suit = Spades; value = 4};
  {rank = "5"; suit = Spades; value = 5};
  {rank = "6"; suit = Spades; value = 6};
  {rank = "7"; suit = Spades; value = 7};
  {rank = "8"; suit = Spades; value = 8};
  {rank = "9"; suit = Spades; value = 9};
  {rank = "10"; suit = Spades; value = 10};
  {rank = "Jack"; suit = Spades; value = 10};
  {rank = "Queen"; suit = Spades; value = 10};
  {rank = "King"; suit = Spades; value = 10};
  {rank = "Ace"; suit = Spades; value = 11};
]

let hearts = [
  {rank = "2"; suit = Hearts; value = 2};
  {rank = "3"; suit = Hearts; value = 3};
  {rank = "4"; suit = Hearts; value = 4};
  {rank = "5"; suit = Hearts; value = 5};
  {rank = "6"; suit = Hearts; value = 6};
  {rank = "7"; suit = Hearts; value = 7};
  {rank = "8"; suit = Hearts; value = 8};
  {rank = "9"; suit = Hearts; value = 9};
  {rank = "10"; suit = Hearts; value = 10};
  {rank = "Jack"; suit = Hearts; value = 10};
  {rank = "Queen"; suit = Hearts; value = 10};
  {rank = "King"; suit = Hearts; value = 10};
  {rank = "Ace"; suit = Hearts; value = 11};
]

let clubs = [
  {rank = "2"; suit = Clubs; value = 2};
  {rank = "3"; suit = Clubs; value = 3};
  {rank = "4"; suit = Clubs; value = 4};
  {rank = "5"; suit = Clubs; value = 5};
  {rank = "6"; suit = Clubs; value = 6};
  {rank = "7"; suit = Clubs; value = 7};
  {rank = "8"; suit = Clubs; value = 8};
  {rank = "9"; suit = Clubs; value = 9};
  {rank = "10"; suit = Clubs; value = 10};
  {rank = "Jack"; suit = Clubs; value = 10};
  {rank = "Queen"; suit = Clubs; value = 10};
  {rank = "King"; suit = Clubs; value = 10};
  {rank = "Ace"; suit = Clubs; value = 11};
]

let diamonds = [
  {rank = "2"; suit = Diamonds; value = 2};
  {rank = "3"; suit = Diamonds; value = 3};
  {rank = "4"; suit = Diamonds; value = 4};
  {rank = "5"; suit = Diamonds; value = 5};
  {rank = "6"; suit = Diamonds; value = 6};
  {rank = "7"; suit = Diamonds; value = 7};
  {rank = "8"; suit = Diamonds; value = 8};
  {rank = "9"; suit = Diamonds; value = 9};
  {rank = "10"; suit = Diamonds; value = 10};
  {rank = "Jack"; suit = Diamonds; value = 10};
  {rank = "Queen"; suit = Diamonds; value = 10};
  {rank = "King"; suit = Diamonds; value = 10};
  {rank = "Ace"; suit = Diamonds; value = 11};
]

let suit_to_string suit =
  match suit with
  | Spades -> "Spades"
  | Hearts -> "Hearts"
  | Clubs -> "Clubs"
  | Diamonds -> "Diamonds"

let card_to_string c =
  String.capitalize_ascii c.rank ^ " of " ^ (suit_to_string c.suit)

let card_of_cmd cmd_phrase =
  match cmd_phrase with
  | r :: x :: "Spades" :: [] -> {rank = String.capitalize_ascii r; 
                                 suit = Spades; value = get_val r}
  | r :: x :: "spades" :: [] -> {rank = String.capitalize_ascii r; 
                                 suit = Spades; value = get_val r}
  | r :: x :: "Hearts" :: [] -> {rank = String.capitalize_ascii r; 
                                 suit = Hearts; value = get_val r}
  | r :: x :: "hearts" :: [] -> {rank = String.capitalize_ascii r; 
                                 suit = Hearts; value = get_val r}
  | r :: x :: "Clubs" :: [] -> {rank = String.capitalize_ascii r; 
                                suit = Clubs; value = get_val r}
  | r :: x :: "clubs" :: [] -> {rank = String.capitalize_ascii r; 
                                suit = Clubs; value = get_val r}
  | r :: x :: "Diamonds" :: [] -> {rank = String.capitalize_ascii r; 
                                   suit = Diamonds; value = get_val r}
  | r :: x :: "diamonds" :: [] -> {rank = String.capitalize_ascii r; 
                                   suit = Diamonds; value = get_val r}
  | _ -> raise (NoSuchCard)

let rec score_of_suit suit_to_count card_list =
  match card_list with
  | [] -> 0
  | {rank; suit; value} :: t -> if suit = suit_to_count
    then (get_val rank) + score_of_suit suit_to_count t
    else score_of_suit suit_to_count t

let rec suit_count suit_to_count card_list =
  match card_list with
  | [] -> 0
  | {rank; suit; value} :: t -> if suit = suit_to_count
    then 1 + suit_count suit_to_count t
    else suit_count suit_to_count t

let suit_of_card c = c.suit

let rec find_min_card hand curr_min_card =
  match hand with
  | [] -> curr_min_card
  | c :: t -> if c.rank < curr_min_card.rank then find_min_card t c
    else find_min_card hand curr_min_card