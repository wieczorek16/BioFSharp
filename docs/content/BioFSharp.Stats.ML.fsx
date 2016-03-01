(*** hide ***)
#I "../../bin"


#r "BioFSharp.dll"
#r "FSharpAux.IO.dll"
#r "BioFSHarp.Stats"
#r "MathNet.Numerics.dll"
#r "MathNet.Numerics.Fsharp.dll"
#r "FSharp.Charting.dll"


open BioFSharp
open FSharpAux.IO
open FSharpAux.IO. SchemaReader.Csv
open FSharpAux.IO.SchemaReader.Attribute
open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.LinearAlgebra.Double

(**
BioFSharp
=========

This example demonstrates using csv reader to read a data set to an item list followed by the 
unsupervised analysis of this data set with different approaches implemented in the BioFSharp ML module.

*)


(**
Read iris data set via Csv-reader
----------------------------------
*)
// Record type representing one irisItem. 
type irisItem = 
    { [<FieldAttribute("Sepal length")>] SepalLength : float
      [<FieldAttribute("Sepal width")>] SepalWidth : float
      [<FieldAttribute("Petal length")>] PetalLength : float
      [<FieldAttribute("Petal width")>] PetalWidth : float
      [<FieldAttribute("Species")>] Species : string }

let _ = FileIO.setWorkingDirectory __SOURCE_DIRECTORY__
let reader = new CsvReader<irisItem>(schemaMode = SchemaMode.Exact)

//Read iris data to list of irisItem.
let irisData = reader.ReadFile("./data/irisData.csv", ',', true) |> Seq.toList
let firstItem = 
    irisData |> Seq.averageBy (fun (item : irisItem) -> item.PetalLength)
let irisFeatures = 
    irisData 
    |> List.map 
           (fun ii -> 
           [ ii.SepalLength; ii.SepalWidth; ii.PetalLength; ii.PetalWidth ])
let irisLables = irisData |> List.map (fun ii -> ii.Species)
let irisFeaturesMatrix = DenseMatrix.ofColumnList irisFeatures


(**
BioFSharp ML module 
--------------------
Principal component analysis of the iris data set and visual inspection of the result.
*)
open BioFSharp.Stats.ML.Unsupervised


let adjCenter = PCA.toAdjustCenter irisFeaturesMatrix
let irisPCA = PCA.compute adjCenter irisFeaturesMatrix
let irisDataPCA = PCA.transform adjCenter irisPCA irisFeaturesMatrix
let irisrev = PCA.revert adjCenter irisPCA irisDataPCA

// Score Plot
open FSharp.Charting

// Plot loadings colored grouped by grouping function
let plotLoadingsColoredByGrouping (pcaComponents : PCA.Component []) 
    (labels : seq<string>) (grouping : string -> string) pcIndex1 pcIndex2 = 
    let pComponent1 = pcaComponents.[pcIndex1 - 1]
    let pComponent2 = pcaComponents.[pcIndex2 - 1]
    (Seq.zip3 (pComponent1.EigenVector) (pComponent2.EigenVector) labels)
    |> Seq.groupBy (fun (x, y, label) -> grouping label)
    |> Seq.map 
           (fun (key, values) -> 
           let nVal = values |> Seq.map (fun (x, y, l) -> (x, y))
           let nLab = values |> Seq.map (fun (x, y, l) -> l)
           Chart.Point(nVal, Name = key, Labels = nLab) 
           |> Chart.WithMarkers(Size = 15))
    |> Chart.Combine
    |> Chart.WithTitle
           (sprintf "PC %i (%.2f) versus PC %i (%.2f)" pComponent1.Index 
                (pComponent1.Proportion * 100.) pComponent2.Index 
                (pComponent2.Proportion * 100.))
    |> Chart.ShowChart

// Plot loadings colored grouped by grouping function
let plotScoresColoredByGrouping (transformedData : Matrix<float>) 
    (labels : seq<string>) (grouping : string -> string) pcIndex1 pcIndex2 = 
    (Seq.zip3 (transformedData.Column(pcIndex1 - 1)) 
         (transformedData.Column(pcIndex2 - 1)) labels)
    |> Seq.groupBy (fun (x, y, label) -> grouping label)
    |> Seq.map 
           (fun (key, values) -> 
           let nVal = values |> Seq.map (fun (x, y, l) -> (x, y))
           let nLab = values |> Seq.map (fun (x, y, l) -> l)
           Chart.Point(nVal, Name = key, Labels = nLab) 
           |> Chart.WithMarkers(Size = 15))
    |> Chart.Combine    
    |> Chart.ShowChart

plotLoadingsColoredByGrouping irisPCA 
    [ "Sepal length"; "Sepal width"; "Petal length"; "Petal width" ] 
    (fun x -> x) 1 2
plotScoresColoredByGrouping irisDataPCA irisLables (fun x -> x) 1 2

(**

Scatter matrix plots of iris feature data.


*)
let plotScatternMatrixColoredByGrouping (inputMatrix : Matrix<float>) 
    (labels : seq<string>) (grouping : string -> string) = 
    inputMatrix.EnumerateColumnsIndexed() //ColumnEnumerator()
    |> Seq.collect (fun (i, outerColumn) -> 
           inputMatrix.EnumerateColumnsIndexed()     //ColumnEnumerator()
           |> Seq.map (fun (ii, innerColumn) -> 
                  (Seq.zip3 outerColumn innerColumn labels)
                  |> Seq.groupBy (fun (x, y, label) -> grouping label)
                  |> Seq.map 
                         (fun (key, values) -> 
                         let nVal = values |> Seq.map (fun (x, y, l) -> (x, y))
                         Chart.Point
                             (nVal, 
                              Name = (sprintf "%s%i:%i" key (i + 1) (ii + 1))))
                  |> Chart.Combine
                  |> Chart.WithTitle((sprintf "%i : %i" (i + 1) (ii + 1)))
                  |> Chart.WithMarkers(Style = ChartTypes.MarkerStyle.Circle))
           |> Seq.toList
           |> List.rev)
    |> Chart.RowsWithBreak inputMatrix.ColumnCount
    |> Chart.ShowChart

let plotScatternMatrix (inputMatrix : Matrix<float>) = 
    inputMatrix.EnumerateColumnsIndexed()        //ColumnEnumerator()
    |> Seq.collect (fun (i, outerColumn) -> 
           inputMatrix.EnumerateColumnsIndexed()    //ColumnEnumerator()
           |> Seq.map 
                  (fun (ii, innerColumn) -> 
                  Chart.Point(Seq.zip outerColumn innerColumn)
                  |> Chart.WithTitle((sprintf "%i : %i" (i + 1) (ii + 1)))
                  |> Chart.WithMarkers
                         (Color = System.Drawing.Color.DarkGray, 
                          Style = ChartTypes.MarkerStyle.Circle))
           |> Seq.toList
           |> List.rev)
    |> Chart.RowsWithBreak inputMatrix.ColumnCount
    |> Chart.ShowChart

plotScatternMatrix irisFeaturesMatrix
plotScatternMatrixColoredByGrouping irisFeaturesMatrix irisLables (fun x -> x)

(**
Iterative clustering 
--------------------
K means clustering of iris feature data using euclidean distance metric 
and initiate centroids based on cvMax algorithm.
*)
// For random cluster inititalization use randomInitFactory:
let rng = new System.Random()

// Centroid factory, given a dataset, generates n random cluster.
let randomInitFactory : IterativeClustering.CentroidsFactory<float []> = 
    IterativeClustering.randomCentroids<float []> rng

// Centroid factory, given a dataset, generates cluster based on cvmax algorithm.
let cvmaxFactory : IterativeClustering.CentroidsFactory<float []> = 
    IterativeClustering.intitCVMAX

// Returns result of kMeans clustering of the data. 
let kmeansResult = 
    IterativeClustering.kmeans <| DistanceMetrics.euclidean <| cvmaxFactory 
    <| (Matrix.toRowArrays irisFeaturesMatrix) <| 3

// Visualization of kMeans clustering result.
let chartsOfClassifiedData = 
    Matrix.toRowArrays irisFeaturesMatrix
    |> Seq.groupBy (fun dataPoint -> fst (kmeansResult.Classifier dataPoint))
    |> Seq.sortBy fst
    |> Seq.map (fun (key, values) -> 
           values
           |> Seq.map 
                  (fun v -> 
                  Chart.Line v 
                  |> Chart.WithStyling
                         (Color = System.Drawing.Color.Silver, BorderWidth = 1))
           |> Chart.Combine
           |> Chart.WithTitle(key.ToString()))
    |> Chart.Rows
    |> Chart.ShowChart


(**
Gap statistics 
---------------
Calculation of the gap statistics to estimate the optimal number of clusters.
*)
// Given data<'a>, random point generator returns data' in the range of data 
// and takes the original datashape into account.
let pointGenerator : GapStatistics.GenericPointGenerator<float[]> = 
    GapStatistics.PointGenerators.generate_uniform_points_PCA rng 

// input data<'a>.
let irisFeatureData = irisFeatures |> Seq.map (fun ilist -> List.toArray ilist)

// Returns the cluster dispersion given a dataset (data<'a>) and clusternumber k. 
let calcClusterDispersion : GapStatistics.GenericClusterDispersion<float[]> =
    GapStatistics.ClusterDispersionMetric.logDispersionKMeans_initCvMax

// Computes gap statistics based on kMeans clustering initiated by cvMax algorithm.
let GapStatisticResult =
    GapStatistics.calculate pointGenerator 10 calcClusterDispersion 4 irisFeatureData

// Visualization of gap statistics result.
let chartGaps =
    GapStatisticResult
    |> Seq.map (fun gapSt -> gapSt.Gaps)
    |> Chart.Line
    |> Chart.WithStyling
            (Color = System.Drawing.Color.Gray, BorderWidth = 1)
    |> Chart.WithXAxis(Title = "# clusters")
    |> Chart.WithYAxis(Title = "gaps")
    |> Chart.ShowChart


(**
Agglomerative clustering 
-------------------------
Hierarchical clustering of iris feature data to build a cluster tree based on 
euclidean distance metric and weighted group avg linkage criterion.
*)

let clusterTree = 
    HierarchicalClustering.generate<float list> DistanceMetrics.euclidean 
        HierarchicalClustering.Linker.weightedGroupAverageLwLinker irisFeatures
let get = HierarchicalClustering.getClusterMemberLabels clusterTree

let tmp = 
    get
    |> List.map (fun l -> 
           ((List.nth l 2), 
            (l
             |> List.rev
             |> List.head)))
    |> List.sortBy snd

let test = Seq.zip (tmp |> Seq.map fst) irisLables
test |> Seq.countBy (fun x -> x)

// Convert cluster tree into string sequence and save it to file path.
HierarchicalClustering.printHClust clusterTree 
|> Seq.write "D:/mySite/test2.json"







