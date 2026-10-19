module TuringMachine

open System
open System.Text

[<EntryPoint>]
let main _ =
    let line =
        match Console.ReadLine() with
        | null -> ""
        | value -> value

    if line |> Seq.exists (fun character -> character <> '0' && character <> '1') then
        printfn "Invalid input: expected a binary string."
        0
    else
        // The canonical machine: a binary incrementer.  In q0 a 1 is rewritten
        // to 0 and the head moves left (carry); a 0 becomes 1 and the machine
        // halts; a blank becomes 1 (a new leading digit) and the machine halts.
        let tape = StringBuilder(line)
        let mutable head = tape.Length - 1
        let mutable halted = false
        while not halted do
            let symbol = if head >= 0 then tape.[head] else '_'
            if symbol = '1' then
                tape.[head] <- '0'
                head <- head - 1
            elif symbol = '0' then
                tape.[head] <- '1'
                halted <- true
            else
                tape.Insert(0, '1') |> ignore
                halted <- true

        printfn "Initial tape: %s" line
        printfn "Final tape: %s" (tape.ToString())
        0
