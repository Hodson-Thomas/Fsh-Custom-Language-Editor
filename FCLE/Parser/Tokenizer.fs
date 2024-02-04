namespace Parser

module Tokenizer = 

  type Token = 
  // Values
  | Int of int              // Interger value
  | Float of float          // Float value
  | String of string        // String value
  | Bool of bool            // Boolean value
  | Identifier of string    // Identifier
  // Operators
  | Plus                    // (+) Plus operator
  | Minus                   // (-) Minus operator
  | Times                   // (*) Times operator
  | Divide                  // (/) Divide operator
  | Modulus                 // (%) Modulus operator
  // Binary operators
  | BAnd                    // (&) Binary And operator
  | BOr                     // (|) Binayr Or operator
  | BXor                    // (^) Binary Xor operator
  | BNot                    // (!) Binary Not operator
  // Comparators
  | Equals                  // (=) Equals comparator
  | NotEquals               // (!=) Not Equals comparator
  | Lss                     // (<) Less comparator
  | LEq                     // (<=) Less or Equal comparator 
  | Grt                     // (>) Greater comperator
  | GEq                     // (>=) Greater or Equal comparator
  // Symbols
  | OCurl                   // ({) Opened Curly Brace
  | CCurl                   // (}) Closed Curly Brace
  | OPar                    // (() Opened Paranthesis
  | CPar                    // ()) Closed Paranthesis
  | OBrace                  // ([) Opened brace
  | CBrace                  // (]) Closed Paranthesis
  | Quote                   // (") Quote
  | Assign                  // (:=) Assign
  | Eoi                     // (;) End of instruction
  | Comma                   // (,) Comma
  // Keywords
  | Function                // (fnc) Function keywork
  | Let                     // (let) let keyword
  | Return                  // (return) return keyword
  | For                     // (for) For loop keyword
  | Loop                    // (loop) Loop keyword
  | While                   // (while) While loop keyword
  | TInt                    // (int) Type integer keyword
  | TFloat                  // (decimal) Type float keyword
  | TString                 // (text) Type string keyword
  | TBool                   // (bool) Type bool keyword
  | Struct                  // (struct) Struct keyword
  

  module private Configs =  

    let boolean_regex: string = "^(false|true)$"
    let integer_regex: string = "^[0-9]+$"
    let float_regex: string = "^[0-9]+(.[0-9]+)?$"
    let identifier_regex: string = "^[a-z A-Z]+(_[a-z A-Z]+)*$"

  module Debug = 


    /// <summary>
    /// Converts a token to a string.
    /// </summary>
    /// 
    /// <param name="token">The token.</param>
    /// 
    /// <returns>A string.</returns>
    /// 
    /// <example>
    ///   <code>
    ///     let token = ... in
    ///     printfn "My token : %s." (token_to_string token)
    ///   </code>
    /// </example>
    let token_to_string (token: Token) : string = 
      let string = 
        match token with
        | Int i -> sprintf "INT <%O>" i
        | Float f -> sprintf "FLOAT <%O>" f
        | String s -> sprintf "STRING <%O>" s
        | Bool b -> sprintf "BOOL <%O>" b
        | Identifier s -> sprintf "ID <%O>" s
        | Plus -> "+"
        | Minus -> "-"      
        | Times -> "*"     
        | Divide -> "/"     
        | Modulus -> "%"   
        | BAnd -> "&" 
        | BOr -> "|"  
        | BXor -> "^"          
        | BNot -> "!"        
        | Equals -> "="        
        | NotEquals -> "!="   
        | Lss -> "<"
        | LEq -> "<="        
        | Grt -> ">"
        | GEq -> ">="     
        | OCurl -> "{"
        | CCurl ->  "}"        
        | OPar -> "("
        | CPar -> ")"   
        | OBrace -> "["    
        | CBrace -> "]"
        | Quote -> "\""
        | Assign -> ":="     
        | Eoi -> ";"   
        | Comma -> ","           
        | Function -> "FNC"         
        | Let -> "LET"      
        | Return -> "RETURN"          
        | For -> "FOR"   
        | Loop -> "LOOP"         
        | While -> "WHITE"       
        | TInt -> "TYPE INT"     
        | TFloat -> "TYPE FLOAT"      
        | TString -> "TYPE STRING"
        | TBool -> "TYPE BOOL"
        | Struct -> "STRUCT"
      in sprintf "(%s)" string   


    /// <summary>
    /// Converts a list of tokens to a string.
    /// </summary>
    /// 
    /// <param name="tokens">The token list.</param>
    /// 
    /// <returns>A string.</returns>
    /// 
    /// <example>
    ///   <code>
    ///     let tokens = [ ... ] in 
    ///     printfn "My tokens : %s." tokens
    ///   </code>
    /// </example>
    let tokens_to_string (tokens: list<Token>) : string =
      let strings = List.map (fun token -> token_to_string token) tokens in
      List.fold (fun (a: string) (b: string) -> a + " " + b) "" strings


  /// <summary>
  /// Contains all validation elements.
  /// </summary>
  module Validators = 
    
    /// <summary>
    /// Checks if the given token is a value.
    /// </summary>
    /// 
    /// <param name="token">The token.</param>
    /// 
    /// <returns>A boolean.</returns>
    /// 
    /// <example>
    ///   <code>
    ///     let token = ... in 
    ///     if is_token_value token
    ///       then printfn "The token is a value."
    ///       else printfn "The token is not a value."
    ///   </code>
    /// </example>
    let is_token_value (token: Token) : bool = 
      match token with
      | Int _ | Float _ | String _ | Bool _ -> true 
      | _ -> false


    /// <summary>
    /// Checks if the given token is an identifier.
    /// </summary>
    /// 
    /// <param name="token">The token.</param>
    /// 
    /// <returns>A boolean.</returns>
    /// 
    /// <example>
    ///   <code>
    ///     let token = ... in 
    ///     if is_token_identifier token
    ///       then printfn "The token is an identifier."
    ///       else printfn "The token is not an identifier."
    ///   </code>
    /// </example>
    let is_token_identifier (token: Token) : bool = 
      match token with
      | Identifier _ -> true
      | _ -> false


    /// <summary>
    /// Checks if the given token is a logical operator.
    /// </summary>
    /// 
    /// <param name="token">The token.</param>
    /// 
    /// <returns>A boolean.</returns>
    /// 
    /// <example>
    ///   <code>
    ///     let token = ... in 
    ///     if is_token_logic_operator token
    ///       then printfn "The token is a logical operator."
    ///       else printfn "The token is not a logical operator."
    ///   </code>
    /// </example>
    let is_token_logic_operator (token: Token) : bool = 
      match token with
      | Plus | Minus | Times | Divide | Modulus -> true
      | _ -> false


    /// <summary>
    /// Checks if the given token is a binary operator.
    /// </summary>
    /// 
    /// <param name="token">The token.</param>
    /// 
    /// <returns>A boolean.</returns>
    /// 
    /// <example>
    ///   <code>
    ///     let token = ... in 
    ///     if is_token_binary_operator token
    ///       then printfn "The token is a binary operator."
    ///       else printfn "The token is not a binary operator."
    ///   </code>
    /// </example>
    let is_token_binary_operator (token: Token) : bool = 
      match token with
      | BAnd | BOr | BNot | BXor -> true
      | _ -> false


    /// <summary>
    /// Checks if the given token is an operator.
    /// </summary>
    /// 
    /// <param name="token">The token.</param>
    /// 
    /// <returns>A boolean.</returns>
    /// 
    /// <example>
    ///   <code>
    ///     let token = ... in 
    ///     if is_token_operator token
    ///       then printfn "The token is an operator."
    ///       else printfn "The token is not an operator."
    ///   </code>
    /// </example>
    let is_token_operator (token: Token) : bool = 
      (is_token_binary_operator token) || (is_token_logic_operator token)  


    /// <summary>
    /// Checks if the given token is a comparator.
    /// </summary>
    /// 
    /// <param name="token">The token.</param>
    /// 
    /// <returns>A boolean.</returns>
    /// 
    /// <example>
    ///   <code>
    ///     let token = ... in 
    ///     if is_token_comparator token
    ///       then printfn "The token is a comparator."
    ///       else printfn "The token is not a comparator."
    ///   </code>
    /// </example>
    let is_token_comparator (token: Token) : bool = 
      match token with
      | Equals | NotEquals | Lss | LEq | Grt | GEq -> true
      | _ -> false
      

    /// <summary>
    /// Checks if the given token is a symbol.
    /// </summary>
    /// 
    /// <param name="token">The token.</param>
    /// 
    /// <returns>A boolean.</returns>
    /// 
    /// <example>
    ///   <code>
    ///     let token = ... in 
    ///     if is_token_symbol token
    ///       then printfn "The token is a symbol."
    ///       else printfn "The token is not a symbol."
    ///   </code>
    /// </example>
    let is_token_symbol (token: Token) : bool = 
      match token with
      | OCurl | CCurl | OPar | CPar | OBrace 
      | CBrace | Quote | Assign | Eoi | Comma  -> true
      | _ -> false


    /// <summary>
    /// Checks if the given token is a keyword.
    /// </summary>
    /// 
    /// <param name="token">The token.</param>
    /// 
    /// <returns>A boolean.</returns>
    /// 
    /// <example>
    ///   <code>
    ///     let token = ... in 
    ///     if is_token_keyword token
    ///       then printfn "The token is a keyword."
    ///       else printfn "The token is not a keyword."
    ///   </code>
    /// </example>
    let is_token_keyword (token: Token) : bool = 
      match token with
      | Function | Let | Return | For | Loop | While 
      | TInt | TFloat | TString | TBool | Struct -> true 
      | _ -> false

  
  module private Logic = 

    open System.Text.RegularExpressions


    /// <summary>
    /// Checks if the given string matches to a string token.
    /// </summary>
    /// 
    /// <param name="string">The string.</param>
    /// 
    /// <results>A boolean.</results>
    /// 
    /// <example>
    ///   <code>
    ///     let string = "..." in 
    ///     if is_string_string_token string
    ///       then printfn "The string matches to a string token."
    ///       else printfn "The string does not match to a string token."
    ///   </code>
    /// </example>
    let is_string_string_token (string: string) : bool = 
      let rec aux (chars: list<char>) (buff: Option<char>) : bool =   
        match chars, buff with
        | [], _ -> true
        | h :: t, None -> if h = '"' then false else aux t (h |> Some) 
        | h :: t, Some c -> 
          if (h = '"') && (c = '\\') then aux t (h |> Some)
          else if h = '"' then false
          else aux t (h |> Some)
      in
      aux (string.ToCharArray() |> List.ofArray) None


    /// <summary>
    /// Attempts to convert a string to value-like token.
    /// </summary>
    /// 
    /// <param name="string">The string.</param>
    /// 
    /// <results>
    /// If the string matches to a value-like token then 
    /// it returns the token else it returns None.
    /// </results>
    /// 
    /// <example>
    ///   <code>
    ///     let string = ... in 
    ///     match string_to_value with
    ///     | Some _ -> printfn "The string matches to a value-like token."
    ///     | None -> pritnfn "The string does not match to a value-like token."
    ///   </code>
    /// </example>
    let string_to_value (string: string) : Option<Token> = 
      if Regex.IsMatch(string, Configs.boolean_regex) then string |> Utils.Utils.string_to_bool |> Bool |> Some
      else if Regex.IsMatch(string, Configs.identifier_regex) then string |> int |> Int |> Some
      else if Regex.IsMatch(string, Configs.float_regex) then string |> float |> Float |> Some 
      else if is_string_string_token string then string |> String |> Some
      else None


    /// <summary>
    /// Indicates if the given string overlaps multiple token definitions.
    /// </summary>
    /// 
    /// <param name="string">The string.</param>
    /// 
    /// <returns>A boolean.</returns>
    /// 
    /// <example>
    ///   <code>
    ///     let string = "..." in
    ///     if string_overlap_multiple_tokens string 
    ///       then printfn "The string overlaps multiple token definitions."
    ///       else printfn "The string does not overlap multiple token definitions."
    ///   </code>
    /// </example>
    let string_overlap_multiple_tokens (string: string) : bool = 
      match string with
      | "!" | "<" | ">" -> true
      | _ -> false



    /// <summary>
    /// Attempts to convert a string to a token.
    /// </summary>
    /// 
    /// <param name="string">The string.</param>
    /// 
    /// <results>
    /// If the string matches to a token then it returns the token
    /// else it returns None.
    /// </results>
    /// 
    /// <example>
    ///   <code>
    ///     let string = "..." in 
    ///     match string_to_token string with
    ///     | Some _ -> printfn "The string matches to a token."
    ///     | None -> printfn "The string does not match to a token."
    ///   </code>
    /// </example>
    let string_to_token (string: string) : Option<Token> = 
      match string with
      // Operators
      | "+" -> Some Plus
      | "-" -> Some Minus
      | "*" -> Some Times
      | "/" -> Some Divide
      | "%" -> Some Modulus
      // Binary operators
      | "&" -> Some BAnd
      | "|" -> Some BOr 
      | "^" -> Some BXor
      | "!" -> Some BNot
      // Comparators
      | "=" -> Some Equals
      | "!=" -> Some NotEquals
      | "<" -> Some Lss
      | "<=" -> Some LEq
      | ">" -> Some Grt
      | ">=" -> Some GEq
      // Symbols
      | "{" -> Some OCurl
      | "}" -> Some CCurl
      | "(" -> Some OPar
      | ")" -> Some CPar
      | "[" -> Some OBrace
      | "]" -> Some CBrace
      | "\"" -> Some Quote
      | ":=" -> Some Assign
      | ";" -> Some Eoi
      | "," -> Some Comma
      // Keywords
      | "fct" -> Some Function
      | "let" -> Some Let
      | "return" -> Some Return
      | "for" -> Some For
      | "loop" -> Some Loop
      | "while" -> Some While
      | "int" -> Some TInt
      | "decimal" -> Some TFloat
      | "text" -> Some TString
      | "bool" -> Some TBool
      | "struct" -> Some Struct
      // Values
      | _ -> string_to_value string


    let char_list_to_tokens (chars: list<char>) : list<Token> = 
      []


  let string_to_tokens (string: string) : Option<list<Token>> = 
    None 