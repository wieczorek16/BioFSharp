﻿namespace FSharpBio.Statistics.Descriptive


/// Module to compute common statistical measure
module StatisticalMeasure = 
    
    open FSharp.CoreX
    open FSharp.CoreX.NumericLiteralG

    /// Range min max
    type Range = { Min : float;
                   Max : float}
    
    /// Creates range with minimum and maximum
    let createRange min max =
        { Min = min; Max = max;}     

    
    let range (items:seq<float>) =
        use e = items.GetEnumerator()
        let rec loop (minimum:float) (maximum:float) =
            match e.MoveNext() with
            | true  -> loop (min e.Current minimum) (max e.Current maximum)
            | false -> createRange minimum maximum          
        //Init by fist value
        match e.MoveNext() with
        | true  -> loop e.Current e.Current
        | false -> createRange nan nan



    // #region means

    /// <summary>
    ///   Computes the population mean (Normalized by N)
    /// </summary>
    ///
    /// <param name="items">seq of float</param>
    /// <remarks>Returns NaN if data is empty or if any entry is NaN.</remarks>
    /// <returns>population mean (Normalized by N)</returns>   
    let mean (items:seq<float>) =
        use e = items.GetEnumerator()
        let rec loop n (acc:float) =
            match e.MoveNext() with
            | true  -> loop (n + 1 ) (acc + e.Current)
            | false -> if (n > 0) then (acc / (float n)) else nan            
        loop 0 0.0 


//    let inline mean (items: seq< 'T >) : ^U = 
//        use e = items.GetEnumerator()
//        let rec loop n (acc) =
//            match e.MoveNext() with
//            | true  -> loop (n + 1 ) (acc + e.Current)
//            | false -> if (n > 0) then LanguagePrimitives.DivideByInt< (^U) > acc n else Unchecked.defaultof<'U>
//        loop 0 0G       
//
//
//    let inline meanBy (f : 'T -> ^U) (items: seq< 'T >) : ^U = 
//        use e = items.GetEnumerator()
//        let rec loop n (acc) =
//            match e.MoveNext() with
//            | true  -> loop (n + 1 ) (acc + f e.Current)
//            | false -> if (n > 0) then LanguagePrimitives.DivideByInt< (^U) > acc n else Unchecked.defaultof<'U>
//        loop 0 0G    
//        
//    [<CompiledName("AverageBy")>]
//    let inline averageBy (f : 'T -> ^U) (source: seq< 'T >) : ^U = 
//        checkNonNull "source" source
//        use e = source.GetEnumerator() 
//        let mutable acc = LanguagePrimitives.GenericZero< (^U) >
//        let mutable count = 0
//        while e.MoveNext() do
//            acc <- Checked.(+) acc (f e.Current)
//            count <- count + 1
//        if count = 0 then 
//            invalidArg "source" InputSequenceEmptyString;
//        LanguagePrimitives.DivideByInt< (^U) > acc count
    
    /// <summary>
    ///   Computes harmonic mean
    /// </summary>
    ///
    /// <param name="items">seq of float</param>
    /// <remarks>Returns NaN if data is empty or if any entry is NaN.</remarks>
    /// <returns>harmonic mean</returns>   
    let meanHarmonic (items:seq<float>) =
        use e = items.GetEnumerator()
        let rec loop n (acc:float) =
            match e.MoveNext() with
            | true  -> loop (n + 1 ) (acc + (1. / e.Current))
            | false -> if (n > 0) then ((float n) / acc) else nan            
        loop 0 0.0        
               



    /// <summary>
    ///   Computes gemetric mean
    /// </summary>
    ///
    /// <param name="items">seq of float</param>
    /// <remarks>Returns NaN if data is empty or if any entry is NaN.</remarks>
    /// <returns>gemetric mean</returns>   
    let meanGeometric (items:seq<float>) =
        use e = items.GetEnumerator()
        let rec loop n (acc:float) =
            match e.MoveNext() with
            | true  -> loop (n + 1 ) (acc + e.Current)
            | false -> if (n > 0) then (System.Math.Pow(acc,(1. / float n))) else nan            
        loop 0 1.0          
        

    
    /// <summary>
    ///   Computes the log gemetric mean
    /// </summary>
    ///
    /// <param name="items">seq of float</param>
    /// <remarks>Returns NaN if data is empty or if any entry is NaN.</remarks>
    /// <returns>log gemetric mean</returns>   
    let meanLogGeometric (items:seq<float>) =
        use e = items.GetEnumerator()
        let rec loop n (acc:float) =
            match e.MoveNext() with
            | true  -> loop (n + 1 ) (acc + (System.Math.Log(e.Current)))
            | false -> if (n > 0) then (acc / float n) else nan            
        loop 0 0.0



    //GrandMean
    // Computes the mean of the means of several subsample
    
    
    /// <summary>
    ///   Computes the truncated (trimmed) mean
    /// </summary>
    ///
    /// <param name="items">seq of float</param>
    /// <remarks>Returns NaN if data is empty or if any entry is NaN.</remarks>
    /// <returns>truncated (trimmed) mean</returns>  
    let meanTruncated (percent:float) data =
        let n = Seq.length(data)
        if (n > 0) then    
            let k = int (floor (float n * percent))
            data
            |> Seq.sort
            |> Seq.skip k
            |> Seq.take (n - k)
            |> mean

        else
            nan 


    // #endregion means

    // ##### ##### ##### ##### #####
    // Median 
    /// Sample Median
    let median (data:seq<float>) =
        MathNet.Numerics.Statistics.Statistics.Median(data)


    // #region standard deviation, variance and coefficient of variation


    /// <summary>
    ///   Computes the sample standard deviation
    /// </summary>
    ///
    /// <param name="mean">value around which the standard deviation is calculated (mean)</param>
    /// <param name="items">seq of float</param>
    /// <remarks>Returns NaN if data is empty or if any entry is NaN.</remarks>
    /// <returns>standard deviation of a sample</returns> 
    let stDevOfMean mean (items:seq<float>) =
        use e = items.GetEnumerator()
        let rec loop n (acc:float) =
            match e.MoveNext() with
            | true  -> loop (n + 1) (acc + ((e.Current - mean) * (e.Current - mean)))
            | false -> if (n > 1) then sqrt(acc / (float n)) else nan            
        loop 0 0.0        


    /// <summary>
    ///   Computes the sample standard deviation
    /// </summary>
    ///
    /// <param name="items">seq of float</param>
    /// <remarks>Returns NaN if data is empty or if any entry is NaN.</remarks>
    /// <returns>standard deviation of a sample</returns> 
    let stDev (items:seq<float>) =
        let mean = mean items
        stDevOfMean mean items            


    /// <summary>
    ///   Computes the population standard deviation (Bessel's correction by N-1)
    /// </summary>
    ///
    /// <param name="mean">value around which the standard deviation is calculated (mean)</param>
    /// <param name="items">seq of float</param>
    /// <remarks>Returns NaN if data is empty or if any entry is NaN.</remarks>
    /// <returns>population standard deviation (Bessel's correction by N-1)</returns>     
    let stDevPopulationOfMean mean (items:seq<float>) =
        use e = items.GetEnumerator()
        let rec loop n (acc:float) =
            match e.MoveNext() with
            | true  -> loop (n + 1) (acc + ((e.Current - mean) * (e.Current - mean)))
            | false -> if (n > 1) then sqrt(acc / float (n - 1)) else nan            
        loop 0 0.0        


    /// <summary>
    ///   Computes the population standard deviation (Bessel's correction by N-1)
    /// </summary>
    ///
    /// <param name="items">seq of float</param>
    /// <remarks>Returns NaN if data is empty or if any entry is NaN.</remarks>
    /// <returns>population standard deviation (Bessel's correction by N-1)</returns>     
    let stDevPopulation (items:seq<float>) =
        let mean = mean items
        stDevPopulationOfMean mean items 


    /// <summary>
    ///   Computes the sample variance
    /// </summary>
    ///
    /// <param name="mean">value around which the variance is calculated (mean)</param>
    /// <param name="items">seq of float</param>
    /// <remarks>Returns NaN if data is empty or if any entry is NaN.</remarks>
    /// <returns>variance</returns> 
    let varOfMean mean (items:seq<float>) =
        use e = items.GetEnumerator()
        let rec loop n (acc:float) =
            match e.MoveNext() with
            | true  -> loop (n + 1) (acc + ((e.Current - mean) * (e.Current - mean)))
            | false -> if (n > 1) then (acc / float n) else nan            
        loop 0 0.0 


    /// <summary>
    ///   Computes the sample variance
    /// </summary>
    ///
    /// <param name="items">seq of float</param>
    /// <remarks>Returns NaN if data is empty or if any entry is NaN.</remarks>
    /// <returns>variance</returns> 
    let var (items:seq<float>) =
        let mean = mean items
        varOfMean mean items


    /// <summary>
    ///   Computes the unbaised variance estimator of the given values (Bessel's correction by N-1)
    /// </summary>
    ///
    /// <param name="mean">value around which the variance is calculated (mean)</param>
    /// <param name="items">seq of float</param>
    /// <remarks>Returns NaN if data is empty or if any entry is NaN.</remarks>
    /// <returns>unbaised population variance estimator (Bessel's correction by N-1)</returns> 
    let varPopulationOfMean mean (items:seq<float>) =
        use e = items.GetEnumerator()
        let rec loop n (acc:float) =
            match e.MoveNext() with
            | true  -> loop (n + 1) (acc + ((e.Current - mean) * (e.Current - mean)))
            | false -> if (n > 1) then (acc / float (n - 1)) else nan            
        loop 0 0.0 



    /// <summary>
    ///   Computes the unbaised variance estimator of the given values (Bessel's correction by N-1)
    /// </summary>
    ///    
    /// <param name="items">seq of float</param>
    /// <remarks>Returns NaN if data is empty or if any entry is NaN.</remarks>
    /// <returns>unbaised population variance estimator (Bessel's correction by N-1)</returns> 
    let varPopulation (items:seq<float>) =
        let mean = mean items
        varPopulationOfMean mean items


    /// <summary>
    ///   Computes the Coefficient of Variation of a sample
    /// </summary>
    ///
    /// <param name="mean">value around which the standard deviation is calculated (mean)</param>
    /// <param name="items">seq of float</param>
    /// <remarks>Returns NaN if data is empty or if any entry is NaN.</remarks>
    /// <returns>Coefficient of Variation of a sample</returns> 
    let cvOfMean mean (items:seq<float>) =
        use e = items.GetEnumerator()
        let rec loop n (acc:float) =
            match e.MoveNext() with
            | true  -> loop (n + 1) (acc + ((e.Current - mean) * (e.Current - mean)))
            | false -> if (n > 1) then sqrt(acc / float n) / mean else nan            
        loop 0 0.0 


    /// <summary>
    ///   Computes the Coefficient of Variation of a sample
    /// </summary>
    ///
    /// <param name="items">seq of float</param>
    /// <remarks>Returns NaN if data is empty or if any entry is NaN.</remarks>
    /// <returns>Coefficient of Variation of a sample</returns> 
    let cv (items:seq<float>) =
        let mean = mean items
        cvOfMean mean items


    /// <summary>
    ///   Computes the Coefficient of Variation of the population (population standard deviation)
    /// </summary>
    ///
    /// <param name="mean">value around which the standard deviation is calculated (mean)</param>
    /// <param name="items">seq of float</param>
    /// <remarks>Returns NaN if data is empty or if any entry is NaN.</remarks>
    /// <returns>Coefficient of Variation of the population (Bessel's correction by N-1)</returns> 
    let cvPopulationOfMean mean (items:seq<float>) =
        use e = items.GetEnumerator()
        let rec loop n (acc:float) =
            match e.MoveNext() with
            | true  -> loop (n + 1) (acc + ((e.Current - mean) * (e.Current - mean)))
            | false -> if (n > 1) then sqrt(acc / float (n - 1)) / mean else nan            
        loop 0 0.0         


    /// <summary>
    ///   Computes the Coefficient of Variation of the population (population standard deviation)
    /// </summary>
    ///
    /// <param name="mean">value around which the standard deviation is calculated (mean)</param>
    /// <param name="items">seq of float</param>
    /// <remarks>Returns NaN if data is empty or if any entry is NaN.</remarks>
    /// <returns>Coefficient of Variation of the population (Bessel's correction by N-1)</returns> 
    let cvPopulation (items:seq<float>) =
        let mean = mean items
        cvPopulationOfMean mean items


    // #endregion standard deviation, variance and coefficient of variation
    

    /// <summary>
    ///   Computes the covariance between two sequences of values    
    /// </summary>
    ///
    /// <param name="mean1">mean of sequence 1</param>
    /// <param name="mean2">mean of sequence 2</param>
    /// <param name="items1">sequence 1 of float  </param>    
    /// <param name="items2">sequence 2 of float  </param>
    /// <remarks>Returns NaN if data is empty or if any entry is NaN.</remarks>    
    /// <returns>Covariance between two sequences of values</returns> 
    let covarianceOfMeans mean1 mean2 (items1:seq<float>) (items2:seq<float>) = 
        use e1 = items1.GetEnumerator()
        use e2 = items2.GetEnumerator()
        let rec loop n (acc:float) =
            match e1.MoveNext(),e2.MoveNext() with
            | true,true   -> loop (n + 1)     (acc + ((e1.Current - mean1) * (e2.Current - mean2)))
            | false,false -> if (n > 1) then (acc / float n) else nan          
            | _           -> raise (System.ArgumentException("Vectors need to have the same length."))    
        loop 0 0.0         
        

    /// <summary>
    ///   Computes the covariance between two sequences of values    
    /// </summary>
    ///
    /// <param name="items1">sequence 1 of float  </param>    
    /// <param name="items2">sequence 2 of float  </param>
    /// <remarks>Returns NaN if data is empty or if any entry is NaN.</remarks>    
    /// <returns>Covariance between two sequences of values</returns> 
    let covariance (items1:seq<float>) (items2:seq<float>) = 
        let mean1 = mean items1
        let mean2 = mean items2        
        covarianceOfMeans mean1 mean2 items1 items2



    /// <summary>
    ///   Computes the unbiased covariance between two sequences of values (Bessel's correction by N-1)   
    /// </summary>
    ///
    /// <param name="mean1">mean of sequence 1</param>
    /// <param name="mean2">mean of sequence 2</param>
    /// <param name="items1">sequence 1 of float  </param>    
    /// <param name="items2">sequence 2 of float  </param>
    /// <remarks>Returns NaN if data is empty or if any entry is NaN.</remarks>    
    /// <returns>Unbiased covariance between two sequences of values (Bessel's correction by N-1)</returns> 
    let covarianceUnbaisedOfMeans mean1 mean2 (items1:seq<float>) (items2:seq<float>) = 
        use e1 = items1.GetEnumerator()
        use e2 = items2.GetEnumerator()
        let rec loop n (acc:float) =
            match e1.MoveNext(),e2.MoveNext() with
            | true,true   -> loop (n + 1)     (acc + ((e1.Current - mean1) * (e2.Current - mean2)))
            | false,false -> if (n > 1) then (acc / float (n - 1)) else nan          
            | _           -> raise (System.ArgumentException("Vectors need to have the same length."))    
        loop 0 0.0         
        

    /// <summary>
    ///   Computes the unbiased covariance between two sequences of values (Bessel's correction by N-1)   
    /// </summary>
    ///
    /// <param name="items1">sequence 1 of float  </param>    
    /// <param name="items2">sequence 2 of float  </param>
    /// <remarks>Returns NaN if data is empty or if any entry is NaN.</remarks>    
    /// <returns>Unbiased covariance between two sequences of values (Bessel's correction by N-1)</returns> 
    let covarianceUnbaised (items1:seq<float>) (items2:seq<float>) = 
        let mean1 = mean items1
        let mean2 = mean items2        
        covarianceUnbaisedOfMeans mean1 mean2 items1 items2


    

    /// <summary>
    ///   Computes the Skewness for the given values.
    /// </summary>
    /// 
    /// <param name="mean">mean of sequence of values</param> 
    /// <param name="items">sequence of float</param> 
    /// <remarks>
    ///   Skewness characterizes the degree of asymmetry of a distribution
    ///   around its mean. Positive skewness indicates a distribution with
    ///   an asymmetric tail extending towards more positive values. Negative
    ///   skewness indicates a distribution with an asymmetric tail extending
    ///   towards more negative values.
    /// </remarks>
    let skewnessFromMean mean (items:seq<float>) =
        use e = items.GetEnumerator()
        let rec loop n (s2:float) (s3:float) =
            match e.MoveNext() with
            | true  -> let dev = e.Current - mean
                       loop (n + 1) (s2 + dev * dev) (s3 + s2  * dev)
            | false -> if (n > 1) then 
                        let n = float n
                        let m2 = s2 / n
                        let m3 = s3 / n
                        let g = m3 / (System.Math.Pow(m2, 3. / 2.0))
                        let a = System.Math.Sqrt(n * (n - 1.));
                        let b = n - 2.;
                        (a / b) * g                       
                       else
                        nan            
        loop 0 0.0 0.0
    

    /// <summary>
    ///   Computes the Skewness for the given values.
    /// </summary>
    /// 
    /// <param name="mean">mean of sequence of values</param> 
    /// <param name="items">sequence of float</param> 
    /// <remarks>
    ///   Skewness characterizes the degree of asymmetry of a distribution
    ///   around its mean. Positive skewness indicates a distribution with
    ///   an asymmetric tail extending towards more positive values. Negative
    ///   skewness indicates a distribution with an asymmetric tail extending
    ///   towards more negative values.
    /// </remarks>    
    let skewness (items:seq<float>) =
        let mean = mean items                  
        skewnessFromMean mean items
        



    /// <summary>
    ///   Computes the Skewness for the given population. (baised)
    /// </summary>
    /// 
    /// <remarks>
    ///   Skewness characterizes the degree of asymmetry of a distribution
    ///   around its mean. Positive skewness indicates a distribution with
    ///   an asymmetric tail extending towards more positive values. Negative
    ///   skewness indicates a distribution with an asymmetric tail extending
    ///   towards more negative values.
    /// </remarks>
    let skewnessPopulation (data:seq<float>) =
        let n = float ( Seq.length data )
        let mean = data |> mean
        let (s2,s3) = Seq.fold (fun (s2,s3) v -> let dev = v - mean
                                                 ((s2 + dev * dev),(s3 + s2  * dev))) (0.,0.) data
        let m2 = s2 / n
        let m3 = s3 / n
        let g = m3 / (System.Math.Pow(m2, 3. / 2.0))
        g


    /// <summary>
    ///   Computes the Kurtosis for the given values.
    /// </summary>
    /// 
    /// <remarks>
    ///   The framework uses the same definition used by default in SAS and SPSS.
    /// </remarks>
    // http://www.ats.ucla.edu/stat/mult_pkg/faq/general/kurtosis.htm
    let kurtosis (data:seq<float>) =
        let n = float ( Seq.length data )
        let mean = data |> mean
        let (s2,s4) = Seq.fold (fun (s2,s4) v -> let dev = v - mean
                                                 ((s2 + dev * dev),(s4 + s2  * s2))) (0.,0.) data
        let m2 = s2 / n
        let m4 = s4 / n
        
        let v = s2 / (n - 1.)
        let a = (n * (n + 1.)) / ((n - 1.) * (n - 2.) * (n - 3.))
        let b = s4 / (v * v);
        let c = ((n - 1.) * (n - 1.)) / ((n - 2.) * (n - 3.));

        a * b - 3. * c


    /// <summary>
    ///   Computes the Kurtosis for the given population. (baised)
    /// </summary>
    /// 
    /// <remarks>
    ///   The framework uses the same definition used by default in SAS and SPSS.
    /// </remarks>
    // http://www.ats.ucla.edu/stat/mult_pkg/faq/general/kurtosis.htm
    let kurtosisPopulation (data:seq<float>) =
        let n = float ( Seq.length data )
        let mean = data |> mean
        let (s2,s4) = Seq.fold (fun (s2,s4) v -> let dev = v - mean
                                                 ((s2 + dev * dev),(s4 + s2  * s2))) (0.,0.) data
        let m2 = s2 / n
        let m4 = s4 / n
        
        m4 / (m2 * m2) - 3.


    /// Median absolute deviation
    /// MAD
    let medianAbsoluteDev (data:seq<float>) =
        let median = MathNet.Numerics.Statistics.Statistics.Median(data)
        let dev = data |> Seq.map (fun x -> abs(x-median))
        MathNet.Numerics.Statistics.Statistics.Median(dev)


    /// Average absolute deviation (Normalized by N)
    let populationAverageDev (data) =        
        let filterSeq =
            data |> Seq.filter (fun x -> not (System.Double.IsNaN x))
        if (Seq.length(filterSeq) > 0) then
            let median = MathNet.Numerics.Statistics.Statistics.Median(filterSeq)
            let dev = filterSeq |> Seq.map (fun x -> abs(x-median))
            MathNet.Numerics.Statistics.Statistics.Mean(dev)
        else nan

    /// Average absolute deviation (Normalized by N-1)
    let averageDev (data) =
        let filterSeq =
            data |> Seq.filter (fun x -> not (System.Double.IsNaN x))
        if (Seq.length(filterSeq) > 0) then 
            let median = MathNet.Numerics.Statistics.Statistics.Median(filterSeq)
            let dev = filterSeq |> Seq.map (fun x -> abs(x-median))
            let sumDev = dev |> Seq.sum
            sumDev/(float(Seq.length(filterSeq)))
        else nan    

    
    // ########################################################################
    /// A module which implements helper functions to provide special statistical measures
    module UtilityFunctions =

        /// <summary>
        ///   Computes sum of squares
        /// </summary>
        ///
        /// <param name="items">seq of float</param>
        /// <remarks>Returns NaN if data is empty or if any entry is NaN.</remarks>
        /// <returns>sum of squares</returns> 
        let sumOfSquares (xData:seq<float>) (exData:seq<float>) =
            let xX = Seq.zip xData exData 
            Seq.fold (fun acc (x,ex) -> acc + square (x - ex)) 0. xX


        /// <summary>
        ///   Computes the pooled variance of the given values
        /// </summary>
        /// 
        /// <param name="sizes">The number of samples</param>
        /// <param name="variances">The population variances for each samples.</param>
        let pooledVarOf (sizes:seq<int>) (variances:seq<float>) =            
            
            let var,n =
                Seq.zip variances sizes
                |> Seq.fold (fun (varAcc,nAcc) (variance,size) -> let n   = float size
                                                                  let var = variance * (n - 1.)
                                                                  (varAcc + var,nAcc + n)) (0., 0.)  
            var / n 


        /// <summary>
        ///   Computes the pooled variance of the given values
        /// </summary>
        let pooledVar (data:seq<#seq<float>>) =
            let sizes = data |> Seq.map Seq.length            
            
            let var,n =
                Seq.zip data sizes
                |> Seq.fold (fun (varAcc,nAcc) (sample,size) -> let n   = float size
                                                                let var = (varPopulation sample) * (n - 1.)
                                                                (varAcc + var,nAcc + n)) (0., 0.)  
            var / n 

        /// <summary>
        ///   Computes the pooled population variance of the given values (Bessel's correction by N-1)
        /// </summary>
        /// 
        /// <param name="sizes">The number of samples</param>
        /// <param name="variances">The population variances for each samples.</param>
        let pooledVarPopulationOf (sizes:seq<int>) (variances:seq<float>) =            
            
            let var,n =
                Seq.zip variances sizes
                |> Seq.fold (fun (varAcc,nAcc) (variance,size) -> let n   = float (size - 1)
                                                                  let var = variance * n
                                                                  (varAcc + var,nAcc + n)) (0., 0.)  
            var / n 


        /// <summary>
        ///   Computes the pooled population variance of the given values (Bessel's correction by N-1)
        /// </summary>
        let pooledVarPopulation (data:seq<#seq<float>>) =
            let sizes = data |> Seq.map Seq.length            
            
            let var,n =
                Seq.zip data sizes
                |> Seq.fold (fun (varAcc,nAcc) (sample,size) -> let n   = float (size - 1)
                                                                let var = (varPopulation sample) * n
                                                                (varAcc + var,nAcc + n)) (0., 0.)  
            var / n 


        /// <summary>
        ///   Computes the pooled standard deviation of the given values
        /// </summary>
        ///
        /// <param name="sizes">The number of samples</param>
        /// <param name="variances">The population variances for each samples.</param>       
        let pooledStDevOf (sizes:seq<int>) (variances:seq<float>) =  
            sqrt (pooledVarOf sizes variances)


        /// <summary>
        ///   Computes the pooled standard deviation of the given values.
        /// </summary>       
        let pooledStDev (data:seq<#seq<float>>) = 
            sqrt (pooledVar data)


        /// <summary>
        ///   Computes the pooled population standard deviation of the given values (Bessel's correction by N-1)
        /// </summary>
        ///
        /// <param name="sizes">The number of samples</param>
        /// <param name="variances">The population variances for each samples.</param>       
        let pooledStDevPopulationOf (sizes:seq<int>) (variances:seq<float>) =  
            sqrt (pooledVarPopulationOf sizes variances)


        /// <summary>
        ///   Computes the pooled population standard deviation of the given values (Bessel's correction by N-1)
        /// </summary>       
        let pooledStDevPopulation (data:seq<#seq<float>>) = 
            sqrt (pooledVarPopulation data)


    // ########################################################################
    /// A module which implements functional matrix operations.
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Matrix =
        
        open MathNet.Numerics
        open MathNet.Numerics.LinearAlgebra
        open MathNet.Numerics.LinearAlgebra.Double

        /// Returns the covariance matrix for the columns
        let inline columnCovariance (A: #Matrix<float>) =
            let rM = DenseMatrix(A.ColumnCount,A.ColumnCount)    
            for (i,coli) in A.EnumerateColumnsIndexed() do
                for (j,colj) in A.EnumerateColumnsIndexed() do 
                    let cov = covariance coli colj
                    rM.[i,j] <- cov
                    rM.[j,i] <- cov
            rM


        /// Returns the covariance matrix for the rows
        let inline rowCovariance (A: #Matrix<float>) =
            let rM = DenseMatrix(A.RowCount,A.RowCount)    
            for (i,rowi) in A.EnumerateRowsIndexed() do
                for (j,rowj) in A.EnumerateRowsIndexed() do 
                    let cov = covariance rowi rowj
                    rM.[i,j] <- cov
                    rM.[j,i] <- cov
            rM


        /// Returns the covariance matrix for the columns (Bessel's correction by N-1)
        let inline columnCovarianceUnbaised (A: #Matrix<float>) =
            let rM = DenseMatrix(A.ColumnCount,A.ColumnCount)    
            for (i,coli) in A.EnumerateColumnsIndexed() do
                for (j,colj) in A.EnumerateColumnsIndexed() do 
                    let cov = covarianceUnbaised coli colj
                    rM.[i,j] <- cov
                    rM.[j,i] <- cov
            rM


        /// Returns the covariance matrix for the rows (Bessel's correction by N-1)
        let inline rowCovarianceUnbaised (A: #Matrix<float>) =
            let rM = DenseMatrix(A.RowCount,A.RowCount)    
            for (i,rowi) in A.EnumerateRowsIndexed() do
                for (j,rowj) in A.EnumerateRowsIndexed() do 
                    let cov = covarianceUnbaised rowi rowj
                    rM.[i,j] <- cov
                    rM.[j,i] <- cov
            rM


        /// Returns mean over column
        let inline columnMean (A: #Matrix<float>) =  
            seq { for coli in A.EnumerateColumns() do
                    yield mean coli }                


        /// Returns mean over row
        let inline rowMean (A: #Matrix<float>) =
            seq { for rowi in A.EnumerateRows() do 
                    yield mean rowi }

        /// Returns range over row
        let inline rowRange (A: #Matrix<float>) =
            seq { for rowi in A.EnumerateRows() do 
                    yield range rowi }


        /// Returns range over column
        let inline columnRange (A: #Matrix<float>) =
            seq { for coli in A.EnumerateColumns() do 
                    yield range coli }


        
    //  ##### #####
    /// All descriptice stats function filters NaN and +/- inf values
    module NaN =
        
        /// Computes the population mean (Normalized by N)
        /// Removes NaN before calculation
        let mean (data:seq<float>) =
            let fdata = data |> Seq.Double.filterNanAndInfinity
            mean fdata

        /// Computes the median
        /// Removes NaN before calculation
        let median (data:seq<float>) =
            let fdata = data |> Seq.Double.filterNanAndInfinity
            median fdata            


        /// Computes the population standard deviation (Normalized by N)
        /// Removes NaN before calculation
        let stDevPopulation data =
            let fdata = data |> Seq.Double.filterNanAndInfinity
            stDevPopulation fdata


        /// Computes the baised population variance estimator (Normalized by N)
        /// Removes NaN before calculation
        let varPopulation data =
            let fdata = data |> Seq.Double.filterNanAndInfinity
            varPopulation fdata

