(*** hide ***)
#I "../../bin"

(**


Connect to BioDB Project
------------------------

BioDB is a multipurpose database for storing polymorphic biological data.
This client realise an easy connection using the BioDB API.

*)


#r "BioFSharp.dll"
#r "BioFSharp.IO.dll"

open BioFSharp
open BioFSharp.IO


// Set 
let baseURL = "http://iomiqsweb1.bio.uni-kl.de:65000/"

let login = BioDB.login baseURL "*****" "*****"


BioDB.getItemTypes baseURL login
