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

// Test versus http://www.cyclismo.org/tutorial/R/linearLeastSquares.html
let xVector = vector [2000.;   2001.;  2002.;  2003.;   2004.;]
let yVector = vector [9.34;   8.50;  7.62;  6.93;  6.60;]

let coeff   = Regression.Linear.coefficient xVector yVector
let fit     = Regression.Linear.fit coeff
let regLine = xVector |> Vector.map fit



let summary = Regression.calulcateSumOfSquares fit xVector yVector

let rsquared = Regression.calulcateDetermination summary

let sigIntercept = Regression.ttestIntercept coeff.[0] summary
let sigSlope     = Regression.ttestSlope coeff.[1] summary


let anova = Regression.Linear.calculateANOVA coeff xVector yVector


let aic = Regression.calcAIC 2. summary.Count summary.Error
let bic = Regression.calcBIC 2. summary.Count summary.Error

Regression.getResiduals fit xVector yVector
Regression.calculateSSE fit xVector yVector

open FSharp.Charting

[Chart.Point(Seq.zip xVector yVector);
Chart.Line(Seq.zip xVector regLine)]
|> Chart.Combine
|> Chart.ShowChart







