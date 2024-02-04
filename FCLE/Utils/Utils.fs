namespace Utils

module Utils = 

  let string_to_bool (string: string) : bool =  
    match string with
    | "false" -> false
    | "true" -> true
    | _ -> raise (Errors.Errors.BooleanParseFailded (sprintf "Can not convert <%s> to a boolean." string))