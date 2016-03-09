(*** hide ***)
#I "../../bin"


#r "BioFSharp.dll"
#r "FSharpAux.IO.dll"
#r "BioFSHarp.Stats"
#r "MathNet.Numerics.dll"
#r "MathNet.Numerics.Fsharp.dll"
#r "FSharp.Charting.dll"


open BioFSharp

open BioFSharp.Stats.Fitting
open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.LinearAlgebra.Double

(**
BioFSharp Stats
===============

This example demonstrates using modules from stats package.

*)


(**
Linear Regression
-----------------
*)

//let xVector = vector [0.25; 0.5; 1. ; 2.;]
//let yVector = vector [-9.48332; -8.61472;  -10.4905;  -11.0471;]

//let xVector = vector [0.25; 0.5; 1. ; 2.; 0.25; 0.5; 1. ; 2.; 0.25; 0.5; 1. ; 2.;]
//let yVector = vector [-9.48332; -8.61472;  -10.4905;  -11.0471; -8.27642;  -7.63456;  -11.4213;  -12.26; -6.01979;  -7.26284;  -8.65068;  -8.00442;]

let xVector = vector [2000.;   2001.;  2002.;  2003.;   2004.;]
let yVector = vector [9.34;   8.50;  7.62;  6.93;  6.60;]

let coeff   = Regression.Linear.coefficient xVector yVector
let fit     = Regression.Linear.fit coeff
let regLine = xVector |> Vector.map fit

let rsqured = Regression.calulcateDetermination yVector regLine

let anova = Regression.Linear.calculateANOVA coeff xVector yVector

Regression.getResiduals fit xVector yVector
Regression.calculateSSE fit xVector yVector

open FSharp.Charting

[Chart.Point(Seq.zip xVector yVector);
Chart.Line(Seq.zip xVector regLine)]
|> Chart.Combine
|> Chart.ShowChart

let d1,d2 = 10.,1.
let FStat  = MathNet.Numerics.Distributions.FisherSnedecor(d1,d2)
let pvalue = 1.0 - (3. |> FStat.CumulativeDistribution)


