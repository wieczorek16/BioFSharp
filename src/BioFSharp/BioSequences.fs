namespace BioFSharp

module BioSequences =

    module OptionConverter =  
        
        /// Type abbreviation for converting char to optional Nucleotide
        type NucleotideOptionConverter = char -> Nucleotides.Nucleotide option
        /// Type abbreviation for converting char to optional AminoAcid
        type AminoAcidOptionConverter = char -> AminoAcids.AminoAcid option

        /// Converters char to AminoAcid option by ignoring bad character
        let charToOptionAminoAcid (aac:char) =
            let pac = AminoAcids.charToParsedAminoAcidChar aac
            match pac with
            | AminoAcids.ParsedAminoAcidChar.StandardCodes  (aa) -> Some aa
            | AminoAcids.ParsedAminoAcidChar.AmbiguityCodes (aa) -> Some aa 
            | AminoAcids.ParsedAminoAcidChar.GapTer         (aa) -> Some aa 
            | AminoAcids.ParsedAminoAcidChar.NoAAChar (_)        -> None                

        
        /// Converters char to AminoAcid option by ignoring bad character and ambiguis code
        let charToOptionStandardAminoAcid (aac:char) =
            let pac = AminoAcids.charToParsedAminoAcidChar aac
            match pac with
            | AminoAcids.ParsedAminoAcidChar.StandardCodes  (aa) -> Some aa
            | AminoAcids.ParsedAminoAcidChar.GapTer         (aa) -> Some aa 
            | AminoAcids.ParsedAminoAcidChar.AmbiguityCodes (_)  -> None
            | AminoAcids.ParsedAminoAcidChar.NoAAChar (_)        -> None

        /// Converters char to AminoAcid option by ignoring bad character
        let charToOptionNucleotid (nuc:char) =
            let pnc = Nucleotides.charToParsedNucleotideChar nuc
            match pnc with
            | Nucleotides.ParsedNucleotideChar.StandardCodes    (n) -> Some n
            | Nucleotides.ParsedNucleotideChar.Standard_DNAonly (n) -> Some n
            | Nucleotides.ParsedNucleotideChar.Standard_RNAonly (n) -> Some n
            | Nucleotides.ParsedNucleotideChar.AmbiguityCodes   (n) -> Some n
            | Nucleotides.ParsedNucleotideChar.GapTer           (n) -> Some n 
            | Nucleotides.ParsedNucleotideChar.NoNucChar (_)        -> None              


        /// Converters char to AminoAcid option by ignoring bad character
        let charToOptionStandardNucleotid (nuc:char) =
            let pnc = Nucleotides.charToParsedNucleotideChar nuc
            match pnc with
            | Nucleotides.ParsedNucleotideChar.StandardCodes    (n) -> Some n
            | Nucleotides.ParsedNucleotideChar.Standard_DNAonly (n) -> Some n
            | Nucleotides.ParsedNucleotideChar.Standard_RNAonly (n) -> Some n
            | Nucleotides.ParsedNucleotideChar.AmbiguityCodes   (_) -> None
            | Nucleotides.ParsedNucleotideChar.GapTer           (n) -> Some n
            | Nucleotides.ParsedNucleotideChar.NoNucChar (_)        -> None  



    // BioSequence

    open System.Collections
    open System.Collections.Generic

    ///Marker interface for BioItem base.
    //[<StructuralEquality;StructuralComparison>]
    type IBioSequence<[<EqualityConditionalOn; ComparisonConditionalOn >]'a when 'a :> IBioItem> =  

        abstract member Item     : int -> 'a 
        abstract member GetSlice : int option*int option -> IBioSequence<'a>  



    type BSubSequence<[<EqualityConditionalOn; ComparisonConditionalOn >]'a when 'a :> IBioItem> =
        BSubSequence of System.ArraySegment<'a>
         member private this.Sequence = match this with BSubSequence this' -> this'
         interface IBioSequence<'a> with
            member this.Item(x) =
                if x < 0 || x >= this.Sequence.Count then
                    raise (System.IndexOutOfRangeException("Index was outside the bounds of the array segment."))
                this.Sequence.Array.[x + this.Sequence.Offset]

            member this.GetSlice(start: int option, finish : int option) =
                let start = defaultArg start 0
                let finish = defaultArg finish (this.Sequence.Count - 1)
                if start < 0 || finish >= this.Sequence.Count then
                    raise (System.IndexOutOfRangeException("Index was outside the bounds of the array segment."))
                let tmp = new System.ArraySegment<'a>(this.Sequence.Array, this.Sequence.Offset + start, finish - start + 1)
                BSubSequence tmp :> IBioSequence<'a>
            
         interface IEnumerable<'a> with
                    member this.GetEnumerator() = (this.Sequence :> seq<_>).GetEnumerator()

         interface IEnumerable with
                    member this.GetEnumerator() = (this.Sequence :> seq<_>).GetEnumerator() :> IEnumerator 


    type BSequence<[<EqualityConditionalOn; ComparisonConditionalOn >]'a when 'a :> IBioItem> =
        BSequence of array<'a>
         member private this.Sequence = match this with BSequence this' -> this'
         member this.Item(i) = (this :>  IBioSequence<'a>).Item(i)
         member this.GetSlice(start, finish) = (this :>  IBioSequence<'a>).GetSlice(start, finish) :?> BSubSequence<'a>
         interface IBioSequence<'a> with
            member  this.Item(i) = this.Sequence.[i]

            member this.GetSlice(start: int option, finish : int option) =
                let start = defaultArg start 0
                let finish = defaultArg finish (this.Sequence.Length - 1)
                //this.Sequence.[start..finish] |> BSequence :> IBioSequence<'a>
                if start < 0 || finish >= this.Sequence.Length then
                    raise (System.IndexOutOfRangeException("Index was outside the bounds of the array segment."))
                let tmp = new System.ArraySegment<'a>(this.Sequence, start, finish - start + 1)
                BSubSequence tmp :> IBioSequence<'a>

         interface IEnumerable<'a> with
                    member this.GetEnumerator() = (this.Sequence :> seq<_>).GetEnumerator()

         interface IEnumerable with
                    member this.GetEnumerator() =  this.Sequence.GetEnumerator() 



//    let isAminoAcidSequence (bseq:#IBioSequence<'a>) =
        


    /// Generates amino acid sequence of one-letter-code string using given OptionConverter
    let ofAminoAcidStringWithOptionConverter (converter:OptionConverter.AminoAcidOptionConverter) (s:string) =          
        s
        |> Seq.choose converter
        |> Seq.toArray |> BSequence

    /// Generates amino acid sequence of one-letter-code raw string
    let ofAminoAcidString (s:string) =          
        s
        |> Seq.choose OptionConverter.charToOptionAminoAcid
        |> Seq.toArray |> BSequence


    /// Generates nucleotide sequence of one-letter-code string using given OptionConverter
    let ofNucleotideStringWithOptionConverter (converter:OptionConverter.NucleotideOptionConverter) (s:string) =             
        s
        |> Seq.choose converter
        |> Seq.toArray |> BSequence
        
    /// Generates nucleotide sequence of one-letter-code raw string
    let ofNucleotideString (s:string) =             
        s
        |> Seq.choose OptionConverter.charToOptionNucleotid           
        |> Seq.toArray |> BSequence
        

    
    ///Active pattern which returns a base triplet
    let private (|Triplet|_|) (en:System.Collections.Generic.IEnumerator<_>) = 
        if en.MoveNext () then                
                    let n1 = en.Current
                    if en.MoveNext () then
                        let n2 = en.Current
                        if en.MoveNext () then
                            Some((n1,n2,en.Current))
                        else
                            None
                    else
                        None
                    
        else
            None

    /// Builts a new collection whose elements are the result of applying
    /// the given function to each triplet of the collection. 
    let mapInTriplets f (input:seq<'a>) =
        let sourceIsEmpty = ref false    
        seq {   use en = input.GetEnumerator()
                while not(!sourceIsEmpty) do                
                match en with
                | Triplet t -> yield (f t)                                                              
                | _         -> sourceIsEmpty := true                               
        }

    //  Replace T by U
    /// Transcribe a given DNA coding strand (5'-----3')
    let transcribeCodeingStrand (nucs:seq<Nucleotides.Nucleotide>) = 
        nucs |> Seq.map (fun nuc -> Nucleotides.replaceTbyU nuc)
        


    //  
    /// Transcribe a given DNA template strand (3'-----5')
    let transcribeTemplateStrand (nucs:seq<Nucleotides.Nucleotide>) = 
        nucs |> Seq.map (fun nuc -> Nucleotides.replaceTbyU (Nucleotides.complement nuc))


    /// translates nucleotide sequence to aminoacid sequence    
    let translate (nucleotideOffset:int) (rnaSeq:seq<Nucleotides.Nucleotide>) =         
        if (nucleotideOffset < 0) then
                raise (System.ArgumentException(sprintf "Input error: nucleotide offset of %i is invalid" nucleotideOffset))                
        rnaSeq
        |> Seq.skip nucleotideOffset
        |> mapInTriplets Nucleotides.lookupBytes

    
    /// Compares the elemens of two biosequence
    let isEqual a b =
        let tmp = Seq.compareWith (fun elem1 elem2 ->
                            if elem1 = elem2 then 0    
                            else 1)  a b 
        tmp = 0



    /// Returns string of one-letter-code
    let toString (bs:seq<#IBioItem>) =
        new string [|for c in bs  -> BioItem.symbol c|]         


       
    /// Returns formula
    let toFormula (bs:seq<#IBioItem>) =
        bs |> Seq.fold (fun acc item -> Formula.add acc  (BioItem.formula item)) Formula.emptyFormula
        
    /// Returns monoisotopic peptide mass (+H20)
    let toPeptideMonoisotopicMass =
        let water = Formula.Table.H2O |> Formula.monoisoMass
        (fun (aa:seq<AminoAcids.AminoAcid>) -> 
            aa 
            |> Seq.sumBy AminoAcids.monoisoMass
            |> (+) water )


    /// Returns average peptide mass (+H20)
    let toPeptideAverageMass =
        let water = Formula.Table.H2O |> Formula.averageMass
        (fun (aa:seq<AminoAcids.AminoAcid>) -> 
            aa 
            |> Seq.sumBy AminoAcids.averageMass
            |> (+) water )



//    /// Returns string of one-letter-code with modifications and label
//    /// representation equivalent to ABSciex protein pilot software
//    let toStringProteinPilot (bs:BioSeq<_>) =
//        let getString (b:#IBioItem) =            
//            match box b with
//            | :? AminoAcids.AminoAcid     as a -> let massDiffInt = AminoAcids.monoisoMassDiff a |> round |> int                                                   
//                                                  sprintf "%c[%0+3i]" (AminoAcids.toChar a) massDiffInt
//            | :? Nucleotides.INucleotides as n -> sprintf "%c" (BioItem.symbol n)
//            | _                                -> failwithf "Type is unknown: %A" b
//
//        new string [|for c in bs  do yield! (getString c)|]  

//    /// Returns average mass of AminoAcidSequence including H20
//    let averageMass (aas:AminoAcidSequence) =
//        Formula.averageMass (toFormula aas)
//                
//
//    /// Returns monoisotopic mass of AminoAcidSequence including H20
//    let monoisoMass (aas:AminoAcidSequence) =
//        Formula.monoisoMass (toFormula aas)

    
//    /// Filters gaps 
//    let filterGAP (aas:AminoAcidSequence) =
//        aas |> Seq.filter (fun a -> a <> AminoAcids.Table.Gap) 






