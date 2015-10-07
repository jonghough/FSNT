namespace FSNT


module Sigma = 
    open System
    open FSNT.Pollard
    open Microsoft.FSharp.Collections

    let Totient (N : UInt64) =
        let flist = FSNT.Pollard.Factor N
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

    let ToMap list =
        list |> Seq.groupBy (fun i -> i) |> Map.ofSeq |> Map.map(fun k v -> Seq.length v)



    let Sig (i : uint32) (N : UInt64) =
        let flist = FSNT.Pollard.Factor N
        let dict = System.Collections.Generic.Dictionary<UInt64,int>()
        let pMap = ToMap flist
        match i with
        | 0u ->     pMap    |> Map.filter(fun k _ -> k > 1UL)
                            |> Map.map(fun k v -> v + 1)
                            |> Map.fold(fun s k v -> s * v) 1
        | _ ->    
                    pMap    |> Map.filter(fun k _ -> k > 1UL)
                            |> Map.map(fun k v -> 
                                int((Math.Pow(float(k),float((v+1)*int(i)))-1.0) / (Math.Pow(float(k), float(i)) - 1.0)))
                            |> Map.fold(fun s k v -> s * v) 1


