﻿namespace BioFSharp.Stats.Testing


module TTest =

    open BioFSharp.Stats
    open TestStatistics

    /// Equal or unequal sample sizes, assume nothing about variance.
    /// input: (mean1,variance1,N1) (mean2,variance2,N3)
    let private noAssumtion (m1,s1,n1:float) (m2,s2,n2:float) =        
        let Sd = System.Math.Sqrt(s1 / n1 + s2 / n2);
        let Statistic = (m1 - m2) / Sd
        let r1 = s1 / n1
        let r2 = s2 / n2
        let df = (((r1 + r2) * (r1 + r2)) / ((r1 * r1) / (n1 - 1.) + (r2 * r2) / (n2 - 1.)));
    
        new TTEST(Statistic,df)   

    let twoSampleFromMeanAndVar (assumeEqualVariances:bool) (mean1,variance1,n1) (mean2,variance2,n2) =

        let equalSampleSize = n1 = n2
        if (assumeEqualVariances) then
                if (equalSampleSize) then
                        // Samples have the same size and assume same variance.
                        let Sp = System.Math.Sqrt(0.5 * (variance1 + variance2))
                        let Statistic = (mean1 - mean2) / (Sp * System.Math.Sqrt(2.0 / n1))
                        let df = 2. * n1 - 2.
                        new TTEST(Statistic,df)  
                else                
                    // Samples have unequal sizes, but assume same variance.
                    let Sp = 4.0//Statistics.Tools.PooledVariance(sample1, sample2);
                    let Statistic = (mean1 - mean2) / (Sp * System.Math.Sqrt(1.0 / n1 + 1.0 / n2));

                    let df = n1 + n2 - 2.
                    new TTEST(Statistic,df)  
                

        else
            // Unequal sample sizes, assume nothing about variance.
            noAssumtion (mean1,variance1,n1) (mean2,variance2,n2)      

    let twoSample (assumeEqualVariances:bool) sample1 sample2 =
        let m1 = sample1 |> StatisticalMeasure.mean
        let m2 = sample2 |> StatisticalMeasure.mean

        let v1 = sample1 |> StatisticalMeasure.var
        let v2 = sample2 |> StatisticalMeasure.var

        let n1 = float(Seq.length(sample1))
        let n2 = float(Seq.length(sample2))

        twoSampleFromMeanAndVar assumeEqualVariances (m1,v1,n1) (m2,v2,n2)