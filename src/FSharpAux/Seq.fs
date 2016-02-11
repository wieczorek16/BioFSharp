﻿namespace FSharpAux


[<AutoOpen>]
module Seq =    
    
    /// Initialie a sequence of length and repeted value (like R! repeat : but swapped input)
    let initRepeatValue length value =
        Seq.initInfinite ( fun v -> value) |> Seq.take (length)

    /// Initialie a sequence of length and repeted values (like R! repeat: but swapped input)
    let initRepeatValues length values =
        Seq.initInfinite ( fun v -> values) |> Seq.take (length) |> Seq.concat

    /// Sorts sequence in descending order
    let sortByDesc f s =
        System.Linq.Enumerable.OrderByDescending(s, new System.Func<'a,'b>(f) )

    /// Iterates over the elements of the input sequence and groups adjacent
    /// elements. A new group is started after the specified predicate holds 
    /// about the element of the sequence (and at the beginning of the iteration).
    // with tail recursion and contionious passing 
    // [3;3;2;4;1;2] |> Seq.groupAfter (fun n -> n%2 = 1);;
    let groupAfter f (input:seq<_>) =     
        let rec group (en:System.Collections.Generic.IEnumerator<_>) cont acc c  =            
                if not(f en.Current) && en.MoveNext() then
                    group en (fun l -> cont <| c::l) acc en.Current
                else
                    (fun l -> cont <| c::l) []
        seq{
            use en = input.GetEnumerator()
            while en.MoveNext() do
                yield group en id [] en.Current }

    /// Iterates over elements of the input sequence and groups adjacent elements.
    /// A new group is started when the specified predicate holds about the element
    /// of the sequence (and at the beginning of the iteration).
    ///
    /// For example: 
    ///    Seq.groupWhen isOdd [3;3;2;4;1;2] = seq [[3]; [3; 2; 4]; [1; 2]]
    let groupWhen f (input:seq<_>) = seq {
        use en = input.GetEnumerator()
        let running = ref true
    
        // Generate a group starting with the current element. Stops generating
        // when it founds element such that 'f en.Current' is 'true'
        let rec group() = 
            [ yield en.Current
              if en.MoveNext() then
                if not (f en.Current) then yield! group() 
              else running := false ]
    
        if en.MoveNext() then
            // While there are still elements, start a new group
            while running.Value do
            yield group() |> Seq.ofList }

    
    /// Break sequence into n-element subsequences
    let groupsOfAtMost (size: int) (s: seq<'v>) : seq<list<'v>> =
        seq {
            let en = s.GetEnumerator ()
            let more = ref true
            while !more do
            let group =
                [
                let i = ref 0
                while !i < size && en.MoveNext () do
                    yield en.Current
                    i := !i + 1
                ]
            if List.isEmpty group then
                more := false
            else
                yield group
        }


    /// Iterates over elements of the input sequence and increase the counter
    /// if the function returens true
    let countIf f (input:seq<_>) =         
        let en = input.GetEnumerator()
        let rec loop (en:System.Collections.Generic.IEnumerator<_>) (counter:int) =
            if en.MoveNext() then
                if (f en.Current) then
                    loop en (counter + 1)
                else
                    loop en counter                    
            else
                counter
        loop en 0


    let pivotize (aggregation:seq<'T> -> 'A) (defaultValue:'A) (keyList:seq<'key>) (valueList:seq<'key*seq<'T>>) =
        let m = valueList |> Map.ofSeq
        keyList |> Seq.map (fun k -> if m.ContainsKey(k) then
                                        aggregation m.[k]
                                     else
                                        defaultValue )


    
    
    /// Returns head of a seq as option or None if seq is empty
    let tryHead s = Seq.tryPick Some s
    
    /// Returns head of a seq or default value if seq is empty
    let headOrDefault defaultValue s  =         
        match (tryHead s) with
        | Some(x) -> x
        | None    -> defaultValue

    
    
    //#region seq double extension
    
    /// Seq module extensions specialized for seq<float>
    module Double = 

        /// Generates sequence (like R! seq.int)
        let seqInit (from:float) (tto:float) (length:float) =
            let stepWidth = (tto - from) / (length - 1.)
            Seq.init (int(length)) ( fun x -> (float(x) * stepWidth) + from)    

        /// Generates sequence given step width (like R! seq)
        let seqInitStepWidth (from:float) (tto:float) (stepWidth:float) =
            seq { from .. stepWidth .. tto }


        let filterNaN (sq:seq<float>) =
            sq |> Seq.filter ( fun x -> not(System.Double.IsNaN(x)) )

        let filterNanBy (f:'a -> float) (sq:seq<'a>) =
            sq |> Seq.filter ( fun x -> not(System.Double.IsNaN(f x)) )

        let filterInfinity (sq:seq<float>) =
            sq |> Seq.filter ( fun x -> not(System.Double.IsInfinity(x)) )
    
        let filterInfinityBy (f:'a -> float) (sq:seq<'a>) =
            sq |> Seq.filter ( fun x -> not(System.Double.IsInfinity(f x)) )

        let filterNanAndInfinity (sq:seq<float>) =
            sq |> Seq.filter ( fun x -> not(System.Double.IsNaN(x) || System.Double.IsInfinity(x)) )

        let filterNanAndInfinityBy (f:'a -> float) (sq:seq<'a>) =
            sq |> Seq.filter ( fun v -> let x = f v
                                        not(System.Double.IsNaN(x) || System.Double.IsInfinity(x)) )

        /// Returns true if sequence contains nan
        let existsNaN (sq:seq<float>) =
            sq |> Seq.exists ( fun x -> System.Double.IsNaN(x) )

        let existsNanBy (f:'a -> float) (sq:seq<'a>) =
            sq |> Seq.exists ( fun x -> System.Double.IsNaN(f x) )


    //#endregion seq double extension

    
