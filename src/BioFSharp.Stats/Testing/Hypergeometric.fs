namespace BioFSharp.Stats.Testing

module Hypergeometric =

    // ###########################################################################################################
    // the hypergeometric distribution is a discrete probability distribution that describes the probability of 
    //   k successes in
    //   n draws from a finite 
    //   x population of size containing
    //   m successes without replacement (successes states)
    /// Calculates p value based on hypergeometric distribution (pValue <= k)
    let CalcHyperGeoPvalue numberOfDEsInBin numberInBin totalUnivers totalNumberOfDE (splitPvalueThreshold:int) =
        if (numberOfDEsInBin > 1) then
            let hp = MathNet.Numerics.Distributions.Hypergeometric(totalUnivers,totalNumberOfDE,numberInBin)
            if numberInBin > splitPvalueThreshold then                                
                // Calculate normal pValue
                (1. -  hp.CumulativeDistribution(float(numberOfDEsInBin + 1)) )
            else
                // Calculate split pValue
                0.5 * ((1. -  hp.CumulativeDistribution(float(numberOfDEsInBin + 1)) ) + ( (1. -  hp.CumulativeDistribution(float(numberOfDEsInBin))) ) )
        else
                nan 