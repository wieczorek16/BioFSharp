﻿namespace BioFSharp


//open MathNet.Numerics
open FSharpAux
open FSharpAux.Math
open Isotopes
    

module Elements =
    
    //  #####
    /// Mono-isotopic elements consist of one isotope
    type MonoIsotopic  = { Symbol : string;
                           X      : Isotopes.Isotope;
                           Xcomp  : float;
                           Root   : float;
                         }
    
    /// Create a mono-isotopic element
    let createMono symbol (x,xcomp) =
        { Symbol = symbol; X = x; Xcomp = xcomp; Root = xcomp }   
    

    //  #####
    /// Di-isotopic elements consist of two isotopes
    type DiIsotopic    = { Symbol : string;
                           X      : Isotopes.Isotope;
                           Xcomp  : float;
                           X1     : Isotopes.Isotope;
                           X1comp : float;
                           Root   : float;
                         }
    
    /// Create a di-isotopic element
    let createDi symbol (x,xcomp) (x1,x1comp) =
        { Symbol = symbol; X = x; Xcomp = xcomp; X1 = x1; X1comp = x1comp; Root = (-1. * xcomp / x1comp)  }


    //  #####
    /// Tri-isotopic elements consist of three isotopes
    [<CustomEquality; CustomComparison>]
    type TriIsotopic   = { Symbol : string;
                           X      : Isotopes.Isotope;
                           Xcomp  : float;
                           X1     : Isotopes.Isotope;
                           X1comp : float;
                           X2     : Isotopes.Isotope;
                           X2comp : float;                           
                           Root   : System.Numerics.Complex*System.Numerics.Complex
                         }
                        
                            override x.Equals(yobj) =
                                match yobj with
                                | :? TriIsotopic as y -> (x.Symbol = y.Symbol)
                                | _ -> false
 
                            override x.GetHashCode() = hash x.Symbol
                        
                            interface System.IComparable with
                              member x.CompareTo yobj =
                                  match yobj with
                                  | :? TriIsotopic as y -> compare x.Symbol y.Symbol
                                  | _ -> invalidArg "yobj" "cannot compare values of different types"

    
    /// Calculates roots of tri-isotopic elements
    let private calcRootsTri (x1Abundance:float) (x2Abundance:float) (x3Abundance:float) =
        let d  =  sqrt (Complex.toComplex (x2Abundance**2. - 4. * x1Abundance * x3Abundance) 0.)//sqrt( x2Abundance**2. - 4. * x1Abundance * x3Abundance)
        let r0 = ((Complex.toComplex (- x2Abundance) 0.) + d) / (Complex.toComplex (2. * x3Abundance) 0.)
        let r1 = ((Complex.toComplex (- x2Abundance) 0.) - d) / (Complex.toComplex (2. * x3Abundance) 0.)
        (r0,r1)

    /// Create a three-isotopic element
    let createTri symbol (x,xcomp) (x1,x1comp) (x2,x2comp) =
        { Symbol = symbol; X = x; Xcomp = xcomp; X1 = x1; X1comp = x1comp; X2 = x2; X2comp = x2comp; Root = calcRootsTri xcomp x1comp x2comp; }


    //  #####
    /// Multi-isotopic elements consist of more than three isotopes
    type MultiIsotopic = { Symbol : string;
                           X      : Isotopes.Isotope;
                           Xcomp  : float;
                           X1     : Isotopes.Isotope;
                           X1comp  : float;
                           X2     : Isotopes.Isotope;
                           X2comp  : float;
                           XL     : Isotopes.Isotope[];
                          } 

    /// Create a multi-isotopic element
    let createMulti symbol (x,xcomp) (x1,x1comp) (x2,x2comp) xl =
        { Symbol = symbol; X = x; Xcomp = xcomp; X1 = x1; X1comp = x1comp; X2 = x2; X2comp = x2comp; XL = xl }


    //  #####################################################################
    /// Discriminant union type of Mono-,Di-,Tri- and Multi-isotopic elements   
    type Element =
        | Mono  of MonoIsotopic
        | Di    of DiIsotopic
        | Tri   of TriIsotopic
        | Multi of MultiIsotopic

    
    /// Returns the main isotop of an Element
    let getMainIsotope (elem:Element) =
        match elem with
        | Mono  {X = x} -> x
        | Di    {X = x} -> x
        | Tri   {X = x} -> x
        | Multi {X = x} -> x

    // Single term of equation. (10)
    let getSinglePhiL (elem:Element) (v:float) (l:float) =
        match elem with
        | Mono  {Root = r}     -> v * r**(- l)
        | Di    {Root = r}     -> v * r**(- l)
        | Tri   {Root = r0,r1} -> let cv  = Complex.toComplex v 0.       
                                  let r0c = Complex.toComplex r0.Real r0.Imaginary
                                  let r1c = Complex.toComplex r1.Real r1.Imaginary
                                  ((cv * r0c**(- l)) + (cv * r1c**(- l))).Real
        | Multi {X = x} -> nan

    
    let getSinglePhiM (elem:Element) (v:float) (l:float) =
        match elem with
        | Mono  {Root = r; X = x}           -> v * (x.NatAbundance*x.Mass)**(- l)
        | Di    {Root = r; X = x; X1 = x1}  -> //let massCoef = x.Mass / x1.Mass //* - 1.        
                                               let root = -1. * (x.Mass * x.NatAbundance) / (x1.Mass * x1.NatAbundance)
                                               v  * (root)**(- l)// * massCoef**(-l)
                                               
        | Tri   {Root = r0,r1} -> let cv  = Complex.toComplex v 0.       
                                  let r0c = Complex.toComplex r0.Real r0.Imaginary
                                  let r1c = Complex.toComplex r1.Real r1.Imaginary
                                  ((cv * r0c**(- l)) + (cv * r1c**(- l))).Real
        | Multi {X = x} -> nan



//    let getIsotopePattern (elem:Element) (count:int) =
//        match elem with
//        | Mono  { Symbol = symbol; X = x; Xcomp = xcomp } -> x.Mass  
//        | Di    {X = x} -> x  
//        | Tri   {X = x} -> x
//        | Multi {X = x} -> x


    module Table = 

        let P = Mono (createMono "P" (Isotopes.Table.P31,Isotopes.Table.P31.NatAbundance) )
    
        let H = Di   (createDi "H" (Isotopes.Table.H1,Isotopes.Table.H1.NatAbundance) (Isotopes.Table.H2,Isotopes.Table.H2.NatAbundance) )
        let C = Di   (createDi "C" (Isotopes.Table.C12,Isotopes.Table.C12.NatAbundance) (Isotopes.Table.C13,Isotopes.Table.C13.NatAbundance) )
        let N = Di   (createDi "N" (Isotopes.Table.N14,Isotopes.Table.N14.NatAbundance) (Isotopes.Table.N15,Isotopes.Table.N15.NatAbundance) )

        let O = Tri  (createTri "O" (Isotopes.Table.O16,Isotopes.Table.O16.NatAbundance) (Isotopes.Table.O17,Isotopes.Table.O17.NatAbundance) (Isotopes.Table.O18,Isotopes.Table.O18.NatAbundance) )
        let S = Tri  (createTri "S" (Isotopes.Table.S32,Isotopes.Table.S32.NatAbundance) (Isotopes.Table.S33,Isotopes.Table.S33.NatAbundance) (Isotopes.Table.S34,Isotopes.Table.S34.NatAbundance) ) // Not tri but quad

        // Attention! Se is Multi
        let Se = Mono (createMono "Se" (Isotopes.Table.Se74,Isotopes.Table.Se74.NatAbundance) )
    
        /// Returns element object according to element symbol string
        let ElementAsObject (symbol:string) =
            match symbol with
            | "H"       -> H        
            | "O"       -> O        
            | "N"       -> N        
            | "C"       -> C        
            | "S"       -> S
            | "P"       -> P
            | "Se"      -> Se
            | _ -> raise (System.ArgumentException("Element unknown"))


        module Heavy =

            let N15 = Di  (createDi "N15" (Isotopes.Table.N15,Isotopes.Table.N14.NatAbundance) (Isotopes.Table.N14,Isotopes.Table.N15.NatAbundance) )



