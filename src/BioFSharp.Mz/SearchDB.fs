namespace BioFSharp.Mz

module SearchDB =
    
    open BioFSharp    

    type SearchModType =
        | Minus = 0
        | Plus  = 1

    type SearchModSite =
        | Any      of ModificationInfo.ModLocation
        | Specific of AminoAcids.AminoAcid * ModificationInfo.ModLocation 
    
    type SearchModification = {
        Name        : string
        Accession   : string
        Description : string
        Composition : Formula.Formula
        Site        : SearchModSite list 
        MType       : SearchModType
        }

    type MassMode = 
        | Average
        | Monoisotopic

    type SearchDbParams = {
        // name of database i.e. Creinhardtii_236_protein_full_labeled
        Name            : string
        // folder path where db files are stored
        DbPath          : string
        FastaPath       : string
        Protease        : Digestion.Protease
        MissedCleavages : int
        MaxMass         : float
        // valid symbol name of isotopic label in label table i.e. #N15
        IsotopicLabel   : string
        MassMode        : MassMode

        FixedMods       : SearchModification list            
        VariableMods    : SearchModification list
    }

    let createDbParams a = a 

    