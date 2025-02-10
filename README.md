# (* Define token types *)
## type token =
  | Keyword of string
  | Operator of string
  | Punctuation of char
  | IntLiteral of int
  | Identifier of string
  | Unknown of string

  (* List of keywords *)
let keywords = ["if"; "else"; "while"; "let"; "in"; "then"]

  (* List of operators *)
  let operators = ["+"; "-"; "*"; "/"; "=="; "!="]
  
  (* List of punctuation symbols *)
  let punctuation = ['('; ')'; '{'; '}'; ';']
  
  (* Check if a word is a keyword *)
let is_keyword word = List.mem word keywords

  (* Check if a word is an operator *)
  let is_operator word = List.mem word operators
  
  (* Check if a character is a punctuation symbol *)
  let is_punctuation ch = List.mem ch punctuation
  
  (* Check if a word is an integer literal *)
  let is_int_literal word =
    try
      ignore (int_of_string word); true
    with Failure _ -> false
  
    (* Tokenize input string *)
let tokenize input =
  let words = Str.split (Str.regexp "[ \t\n\r]+") input in

  let rec classify tokens = function
  | [] -> List.rev tokens  (* Reverse the list before returning *)
  | w :: ws ->
      let token =
        if is_keyword w then Keyword w
        else if is_operator w then Operator w
        else if String.length w = 1 && is_punctuation w.[0] then Punctuation w.[0]
        else if is_int_literal w then IntLiteral (int_of_string w)
        else if Str.string_match (Str.regexp "[a-zA-Z_][a-zA-Z0-9_]*") w 0 then Identifier w
        else Unknown w
      in
      classify (token :: tokens) ws
in
classify [] words

(* Function to print tokens *)
let print_token = function
  | Keyword w -> Printf.printf "Keyword: %s\n" w
  | Operator w -> Printf.printf "Operator: %s\n" w
  | Punctuation c -> Printf.printf "Punctuation: %c\n" c
  | IntLiteral i -> Printf.printf "IntLiteral: %d\n" i
  | Identifier w -> Printf.printf "Identifier: %s\n" w
  | Unknown w -> Printf.printf "Unknown: %s\n" w

  let () =
  let input = "if x == 35 then { a = b + 9; } else { b = a - 1; }" in
  let tokens = tokenize input in
  List.iter print_token tokens



