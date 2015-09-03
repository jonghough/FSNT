namespace FSNT

open System

module PrimeSieve = 

//type SList<'T when 'T : uint32 || 'T : uint64> =


    let AtkinSieve n : uint32 =
        let sqrt = uint32(Math.Sqrt(n) ) + 1u
        //let mutable items : List<uint32> = [0 .. ]
        let mutable primes = [0 .. int(sqrt)]

        let mutable i = 0u
        let mutable j = 0u

        while i < sqrt do
            while j < sqrt do
                let k = 4u * i * i + j * j
                if k < n && (k % 12u = 1u || k % 12u = 5u) then
                    primes.[k] <-    
                        if primes[k] = () then true
                        else !primes[k]
                let k2 = 3u * i * i + j * j
                if k2 < n && k2 % 12u = 7u then
                    primes.[k2] <-    
                        if primes[k2] = () then true
                        else !primes[k2]
                let k3 = 3u * i * i - j * j
                if k3 < n &&  k3 % 12u = 11u then
                    primes.[k3] <-    
                        if primes[k3] = () then true
                        else !primes[k3]
                j <- j + 1u
            i <- i + 1u

        primes |> Seq.filter(
            fun x -> 
                    let mutable D = 0u
                    let mutable E = uint64(D)
                    if primes.[x] then
                        E <- uint64(D)
                        let F = uint64(x)
                        while x * x + D * x < n do
                            primes.[x * x + x * D] <- false
                            D <- D + 1u
                    primes.[x]
        )

    let update lst =
        retLst = 
