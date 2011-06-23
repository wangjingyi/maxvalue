open System
 
let main() = 
    let a = "6 * 3 + 2 * 5"
    let v = Select.max a
 
    Console.WriteLine(v) |> ignore
 
    let a = "0.1 * 0.1 + 0.1"
    let v = Select.max a
    Console.WriteLine(v) |> ignore
 
    let a = "-3 * 3 + 3"
    let v = Select.max a
    Console.WriteLine(v) |> ignore
    
    Console.ReadKey() |> ignore
 
do main()
