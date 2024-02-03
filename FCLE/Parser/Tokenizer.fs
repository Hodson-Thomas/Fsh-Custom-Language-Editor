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
  | TString                 // (string) Type string keyword
  | TBool                   // (bool) Type bool keyword
  | Struct                  // (struct) Struct keyword
  

  
