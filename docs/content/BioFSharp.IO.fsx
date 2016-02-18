(*** hide ***)
#I "../../bin"

(**
BioFSharp: Documentation
========================

BioFSharp aims to be a user-friendly library for Bioinformatics written in F#. It contains the basic data 
structures for common biological objects like amino acids and nucleotides based on chemical formulas and chemical elements. 
It facilitates some basic machine learning task as well as statistical analysis of biological data set.


This example demonstrates using a function defined in BioFSharp library.

*)
#r "BioFSharp.dll"
#r "BioFSharp.IO.dll"

open BioFSharp
open BioFSharp.IO

(**


Read a fastA file
----------------

Converter reads character and returns it as either amino acid or nucleotide
depending on the `OptionConverter` respectivly.
For a protein fastA use: `OptionConverter.charToOptionStandardAminoAcid` and 
for gene fastA use: `OptionConverter.charToOptionStandardNucleotid`.

*)


let converter = BioSequences.OptionConverter.charToOptionStandardAminoAcid

// Path to .fastA file
let fastaPath = __SOURCE_DIRECTORY__ + "/data/chlamy3proteins.fasta"
let chlamy3proteins =
    // Read .fastA
    FastA.fromFile converter fastaPath
    |> Seq.toArray


(**
Digest proteins
---------------
Digests the proteins from .fastA file to peptides. 
Trypsin is used as the protease 
*)


let trypsinPeptides =
    chlamy3proteins
    |> Seq.map (fun fastaItem -> fastaItem.Sequence)    
    |> Seq.collect (fun aas -> Digestion.digest Digestion.trypsin aas)


(**
Mass calculation
----------------
Calculates peptide masses (monoisotopic) of previous digestion.

*)

let peptideMasses =
    trypsinPeptides
    |> Seq.map (fun peptide -> let fPEptide = BioSequences.toFormula peptide
                               Formula.add fPEptide Formula.Table.H2O
                               |> Formula.monoisoMass
                        )

(**
Mass histogram
----------------
Shows distribution of monoisotopic peptide masses

*)
(*** define-output:histogram ***)
#r "FSharp.Charting.dll"
#r "BioFSharp.Stats.dll"

open BioFSharp.Stats
open FSharp.Charting

let bw = 0.7
let histo = Descriptive.Histogram.create bw peptideMasses

Chart.Column (histo |> Descriptive.Histogram.getZip)
|> Chart.WithXAxis(Max=8000.,Min=300.)
(*** include-it:histogram ***)


//// Write
//let _ = chlamy3proteins |> FastA.write AminoAcids.symbol fastaPath













// ##################################################################
// Examples for Fsharp.FsIO Project
//
// --- Csv-reader
// --- FatsA reader/ writer
(*** hide ***)
#I "../../bin"
#r "FSharpAux.dll"
#r "FSharpAux.IO.dll"

//#r "BioFSharp.dll"
//#r "BioFSharp.IO.dll"

open FSharpAux.IO
open FSharpAux.IO.SchemaReader
open FSharpAux.IO.SchemaReader.Csv
open FSharpAux.IO.SchemaReader.Attribute

// ##################################################################
// Examples: Csv-reader reads iris data set
type irisItem = 
    { [<FieldAttribute("Sepal length")>] SepalLength : float
      [<FieldAttribute("Sepal width")>] SepalWidth : float
      [<FieldAttribute("Petal length")>] PetalLength : float
      [<FieldAttribute("Petal width")>] PetalWidth : float
      [<FieldAttribute("Species")>] Species : string
      [<FieldAttribute("Species2")>] Species2 : string }

//0 based index mapping 
type irisItemWithIndex = 
    { [<FieldAttribute(0)>] SepalLength : float
      [<FieldAttribute(1)>] SepalWidth : float
      [<FieldAttribute(2)>] PetalLength : float
      [<FieldAttribute(3)>] PetalWidth : float
      [<FieldAttribute(4)>] Species : string }

type DoubleArrayConverter() = 
    inherit ConverterAttribute()
    override this.convertToObj = 
        Converter.Collection(fun (strs : seq<string>) -> 
            (strs
             |> Seq.map 
                    (fun s -> FSharpAux.String.tryParseFloatDefault nan s)
             |> Seq.toArray)
            |> box)

type irisItemWithMulti = 
    { [<FieldAttribute([| 0; 1; 2; 3 |])>][<DoubleArrayConverter>] Features : float []
      [<FieldAttribute(4)>] Species : string }



type Test = {My :string}

//let _         = IO.setWorkingDirectory __SOURCE_DIRECTORY__
let path = __SOURCE_DIRECTORY__ + "/data/irisData.csv" //....
let reader = new FSharpAux.IO.SchemaReader.Csv.CsvReader<Test>(schemaMode = FSharpAux.IO.SchemaReader.Csv.SchemaMode.Fill)
let hasHeader = true
let separator = ','
let data = reader.ReadFile(path, separator, hasHeader)

new FSharpAux.IO.SchemaReader.Csv.CsvReader<Test>()