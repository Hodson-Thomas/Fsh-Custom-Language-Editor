
let rec loop _ =  
  let _ = printfn ">" in 
  let line = System.Console.ReadLine().Trim() in 
  if line.Length = 0 then loop () else 
  if line = "exit" then () else
  let _ = printfn "Analysing !" in
  loop ()


[<EntryPoint>]
let main _ = 
  let _ = printfn "" in
  let _ = System.Console.ReadKey() in
  0
