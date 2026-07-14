namespace FSLN

open System
open System.IO
open System.Threading

type InputThread() =
    let key_in = new AutoResetEvent(false)
    let key_out = new AutoResetEvent(false)
    let mutable read_key: ConsoleKeyInfo = Unchecked.defaultof<_>
    let LOCK_OBJ = obj()

    let thread =
        Thread(fun () ->
            while true do
                ignore(key_in.WaitOne())
                try
                    let key = Console.ReadKey(true)
                    lock LOCK_OBJ (fun () -> read_key <- key)
                    ignore(key_out.Set())
                with _ -> ()
        )

    member this.Start() : unit =
        thread.IsBackground <- true
        thread.Start()

    member this.TryReadKey(timeout_millis: int, key: outref<ConsoleKeyInfo>) : bool =
        let success =
            key_out.WaitOne(0) || (key_in.Set() && key_out.WaitOne(timeout_millis))

        if success then
            key <- lock LOCK_OBJ (fun () -> read_key)
        else
            Console.In.Close()
            Console.SetIn(new StreamReader(Console.OpenStandardInput()))

        success

    member this.Dispose() : unit =
        thread.Interrupt()
        key_in.Dispose()
        key_out.Dispose()
