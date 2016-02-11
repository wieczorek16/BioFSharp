namespace BioFSharp.Stats.Descriptive

open FSharpAux
open BioFSharp.Stats

module QuantileNormalization =
    
    
    /// Computes the quantile normalization of a given dataset  
    // http://en.wikipedia.org/wiki/Quantile_normalization
    // technique for making two distributions or more identical in statistical properties.
    // to normalize two or more distributions to each other, rank the original values and group them by rank, then set to the average of the original values.         
    let quantileNorm (data : seq<#seq<float>>) =
        
        // Helper function to group RankedValue by rank and calculate average of orignal values
        let groupByAndAverage (input: Rank.RankedValue<float> seq) =
            input
            |> Seq.groupBy  (fun i -> i.RankIndex)
            |> Seq.map (fun (key, values) -> key, values |> Seq.averageBy (fun r -> r.Value) )
        
        // Transform values to their ranks
        let rawRanks = 
            data
            |> Seq.map (fun col -> Rank.breakByMean col |> Seq.sortBy (fun r -> r.Position) ) 
                    
        // Calculate rank to average value mappin
        let rankValueMap =
            rawRanks
            |> Seq.concat
            |> groupByAndAverage
            |> Map.ofSeq
        
        // Get normalized values based on the rank of the original values
        rawRanks
        |> Seq.map (fun col -> 
                        col |> Seq.map (fun r -> rankValueMap.TryFindDefault nan r.RankIndex))
            


    let quantileNormXY (x: float seq) (y: float seq) =
        
        // Helper function to group RankedValue by rank and calculate average of orignal values
        let groupByAndAverage (input: Rank.RankedValue<float> seq) =
            input
            |> Seq.groupBy  (fun i -> i.RankIndex)
            |> Seq.map (fun (key, values) -> key, values |> Seq.averageBy (fun r -> r.Value) )
        
        // Transform values to their ranks
        let xRawRanks = Rank.breakByMean x 
        let yRawRanks = Rank.breakByMean y
        
        // Calculate rank to average value mappin
        let rankValueMap =
            [xRawRanks;yRawRanks]
            |> Seq.concat
            |> groupByAndAverage
            |> Map.ofSeq
        
        // Get normalized values based on the rank of the original values
        let rx = xRawRanks |> Seq.map (fun r -> rankValueMap.TryFindDefault nan r.RankIndex)
        let ry = yRawRanks |> Seq.map (fun r -> rankValueMap.TryFindDefault nan r.RankIndex)
        (rx,ry)

