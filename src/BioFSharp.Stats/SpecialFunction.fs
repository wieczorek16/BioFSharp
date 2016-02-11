namespace BioFSharp.Stats
#nowarn "40"

open System.Collections.Generic

module SpecialFunction =

    let memoize f = 
        let cache = new Dictionary<_, _>()
        (fun x -> 
            match cache.TryGetValue(x) with
            | true, y -> y
            | _ -> 
                let v = f(x)
                cache.Add(x, v)
                v)

    let rec factorial =
        memoize (
            fun x -> 
                if (x = 0) then 
                    1 
                else 
                    x * factorial(x - 1))
    

