namespace fsln

open System

module Interactive =
    
    let loop (solution: Solution) =
        let mutable loop = true
        while loop do
            Operations.render solution
            Console.ReadKey(true) |> printfn "%A"