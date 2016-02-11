﻿namespace BioFSharp.Stats.Fitting

module Bootstrap =
    
    // When we sample with replacement, the two sample values are independent.
    // Practically, this means that what we get on the first one doesn't affect what we get on the second.
    // Mathematically, this means that the covariance between the two is zero
    /// Samples from an array of obj wit replacement (with putting back)
    let sampleWithReplacement (rnd:System.Random) (source:array<_>) (k:int) =
        if source.Length < 1 then failwithf "Source must not be empty."     
        Array.init k (fun i -> source.[rnd.Next(0,source.Length - 1)])



    // Implementation according to: http://krkadev.blogspot.de/2010/08/random-numbers-without-repetition.html
    /// Samples from an array of obj without replacement (without putting back)
    let sampleWithOutReplacement (rnd:System.Random) (source:array<_>) (k:int) =
        let n = source.Length
        let used = new System.Collections.Generic.Dictionary<int,int>()
        // recursive do-while implementation
        let rec loop (off:int) (n:int) (i:int) =    
            let value = n - i - 1 
            if used.ContainsKey(off) then
               loop (used.[off]) (n) (i)
            else
               used.Add(off,value)
               off

        Array.init k (fun i -> source.[(loop (rnd.Next(n - i)) n i)] )

//    let sampleWithOutReplacement (rnd:System.Random) (source:array<_>) (k:int) =
//        let n = source.Length
//        let used = new System.Collections.Generic.Dictionary<int,int>()
//        // recursive do-while implementation
//        let rec loop (index:int) (off:int) (n:int) (i:int) =    
//            let value = n - i - 1 
//            let redirect = if used.ContainsValue(value) then
//                            Some(off)
//                           else
//                            if used.ContainsKey(off) then used.[off] <- value else used.Add(off,value)
//                            None
//        
//            if redirect.IsSome then   (*While*)
//                loop (index + 1) (redirect.Value) (n) (i)
//            else
//                off
//        Array.init k (fun i -> source.[(loop 0 (rnd.Next(n - i)) n i)] )

