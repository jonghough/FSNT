namespace FSNT


module Sigma = 
    open System
    open Pollard

    let Totient (N : UInt64) =
        let flist = Pollard.Factor N
        let dict = System.Collections.Generic.Dictionary<UInt64,int>()
        let mutable res = 1UL
        for n in flist do
            if n = 1UL then
                ()
            else if dict.ContainsKey n then 
                dict.[n] <- dict.[n] + 1
            else    
                dict.[n] <- 1
          
        for kvp in dict do
            res <- res * uint64( Math.Pow(float(kvp.Key), float(kvp.Value)) - Math.Pow(float(kvp.Key), float(kvp.Value-1)))
        res
