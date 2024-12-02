module internal Fundament.Capstone.Compiler.Useful

let inline BLOCKED_ON<'T> m : 'T =
    let message = sprintf "Blocked on: %s" (String.concat "\n" m)
    raise (System.NotImplementedException(message))

let inline outOfRange<'T> e : 'T =
    raise (System.ArgumentOutOfRangeException($"Unknown enum value: %d{(e)}"))

let inline readList reader readFn = reader |> Seq.map readFn |> List.ofSeq

module MultiMap =
    let add key value map =
        Map.add
            key
            (match Map.tryFind key map with
             | None -> [ value ]
             | Some values -> value :: values)
            map
