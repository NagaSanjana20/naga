##### firstly a Project Dierectory is created with the name "ocaml-lexer" and then a source file is created with the name "Lexical_Analyzer.ml" 
##### Explaination - In this project, i have executed a lexical Analyzer in Ocaml that helps in processing an input string and classifies the given data into keywords, operators, punctuations, Integer literals, identifiers, Unknown tokens.

#### Defining the token types as given
type token =
  | Keyword of string
  | Operator of string
  | Punctuation of char
  | IntLiteral of int
  | Identifier of string
  | Unknown of string

#### Keywords 
let keywords = ["if"; "else"; "while"; "let"; "in"; "then"]

#### Operators
  let operators = ["+"; "-"; "*"; "/"; "=="; "!="]
  
#### Punctuation symbols 
  let punctuation = ['('; ')'; '{'; '}'; ';']

#####  Explaination - All the keywords, Operators, punctuation symbols are defined.

#### Checking whether the word is a keyword 
let is_keyword word = List.mem word keywords

#### Checking whether the word is an operator 
  let is_operator word = List.mem word operators
  
#### Checking whether the given character is a punctuation symbol 
  let is_punctuation ch = List.mem ch punctuation
  
#### Checking if a word is an integer literal 
  let is_int_literal word =
    try
      ignore (int_of_string word); true
    with Failure _ -> false
    
#### Tokenize the input string 
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

##### Explaination - After defining, the token are checked and categorized to specific categories they are belonged to. As it checks by spliting the input string into words and checks each one whether it is the word is (checks in an order) keyword, operator, Punctuation, integer Literal, Identifier, Unknown tokens.

#### Function for printing the tokens 
let print_token = function
  | Keyword w -> Printf.printf "Keyword: %s\n" w
  | Operator w -> Printf.printf "Operator: %s\n" w
  | Punctuation c -> Printf.printf "Punctuation: %c\n" c
  | IntLiteral i -> Printf.printf "IntLiteral: %d\n" i
  | Identifier w -> Printf.printf "Identifier: %s\n" w
  | Unknown w -> Printf.printf "Unknown: %s\n" w

#### Testcase 1: 
  let () =
  let input = "if x == 35 then { a = b + 9; } else { b = a - 1; }" in
  let tokens = tokenize input in
  List.iter print_token tokens
  
#### Testcase 2:
  let () =
  let input = "$$$ invalid token" in
  let tokens = tokenize input in
  List.iter print_token tokens

##### explaination - then create a funtion to print the data in the required formal and define the function by inputing the string given i.e., Testcase 1 & 2. And then to complile the lexer give the "ocamlc str.cma Lexical_Analyzer.ml -o lexer" command in the ocaml and the run it by giving "./lexer" then the expected output is given.
