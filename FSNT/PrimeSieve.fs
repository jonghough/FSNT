namespace FSNT

open System
open System.Collections
open System.Collections.Generic

module PrimeSieve = 

//type SList<'T when 'T : uint32 || 'T : uint64> =

//    let toSeq (d : Dictionary<uint32,bool> ) =
//        for v in d.Values do
//
    let toMap dictionary = 
        (dictionary :> seq<_>)
        |> Seq.map (|KeyValue|)
        |> Map.ofSeq

    let Test v =
        v = false

    let AtkinSieve (n : uint32) =
        let sqrt = uint32(Math.Sqrt(float(n)) ) + 1u
        //let mutable items : List<uint32> = [0 .. ]
        let mutable primes =System.Collections.BitArray(int(n+1u),false)//new Dictionary<uint32,bool>()//[0 .. int(sqrt)]

        Console.WriteLine("srt is "+string(sqrt));
        let mutable i = 0u
        while i < sqrt do
            Console.WriteLine("i is "+string(i));
            let mutable j = 0u
            while j < sqrt do
                Console.WriteLine("j is "+string(j));
                let k = (4u * i * i) + (j * j)
                if k < n && (k % 12u = 1u || k % 12u = 5u) then

                    Console.WriteLine("k is "+string(k));
                    primes.Set(int(k), Test(primes.Get(int(k))))

                let k2 = (3u * i * i) + (j * j)
                if k2 < n && k2 % 12u = 7u then
                    Console.WriteLine("k2 is "+string(k2));
                    primes.Set(int(k2), Test(primes.Get(int(k2))))
                let k3 = (3u * i * i) - (j * j)
                if k3 < n && i > j && k3 % 12u = 11u then
                    Console.WriteLine("k3 is "+string(k3));
                    primes.Set(int(k3), primes.Get(int(k3)) = false)
                j <- j + 1u
            i <- i + 1u
        
        let pr = new List<uint32>()
    
        i <- 0u

        while i < sqrt do

            let index = int(i)
            i <- i+1u
            if primes.Get(index) = true then
                Console.WriteLine("index is "+string(index));
                let mutable D = 0u in
                let F = uint64(index) in
                while F * F + uint64(D) * F < uint64(n) do
                    primes.Set(int(F * F + uint64(D) * F),false)
                    D <- D+1u
                pr.Add(uint32(index))

        pr
 


