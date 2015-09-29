namespace FSNT

open System
open System.Collections
open System.Collections.Generic

module PrimeSieve = 

    ///<summary>
    /// Returns a list of the prime sless than n.
    ///</summary>
    let AtkinSieve (n : uint32) =
        let sqrt = uint32(Math.Sqrt(float(n)) ) + 1u

        let mutable primes =System.Collections.BitArray(int(n+1u),false)

        let mutable i = 0u
        while i < sqrt do
            let mutable j = 0u
            while j < sqrt do

                let k = (4u * i * i) + (j * j)
                if k < n && (k % 12u = 1u || k % 12u = 5u) then
                    primes.Set(int(k), primes.Get(int(k)) = false)

                let k2 = (3u * i * i) + (j * j)
                if k2 < n && k2 % 12u = 7u then
                    primes.Set(int(k2), primes.Get(int(k2)) = false)

                let k3 = (3u * i * i) - (j * j)
                if k3 < n && i > j && k3 % 12u = 11u then
                    primes.Set(int(k3), primes.Get(int(k3)) = false)

                j <- j + 1u
            i <- i + 1u
        
        let pr = new List<uint32>()
        pr.Add(2u)
        pr.Add(3u)
        i <- 2u

        while int(i) < primes.Length do
            let index = int(i)
            i <- i+1u
            if primes.Get(index) = true then

                let mutable D = uint64(index) 
                let F = uint64(index) 
                while F * F + D * F < uint64(n) do

                    primes.Set(int(F * F + uint64(D) * F),false)
                    D <- D+1UL
                pr.Add(uint32(index))
        pr
 


