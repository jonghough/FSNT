namespace FSNT

open System
open System.Numerics
///
///
///
module Pollard =
    ///
    let rec GCD(a : UInt64, b : UInt64) : UInt64 =
            match b with
                | 0UL -> a
                | _ -> GCD ( b , ((b + a) % b))
        
    let Rho(n : UInt64) = 
        let divs = List.filter<UInt64> (fun x -> 0UL = n%x) [2UL;3UL;5UL;7UL]
        if (divs.Length > 0) then
            divs.[0]
        else
            let rand = new Random()
            let buffer : byte[] = BitConverter.GetBytes(n)
            rand.NextBytes(buffer)
            let summand  = BitConverter.ToUInt64(buffer, 0)
            rand.NextBytes(buffer)
            let mutable a = BitConverter.ToUInt64(buffer, 0)
            let mutable b = a
     
            a <- (n + a * a + summand) % n
            b <- (((n + b * b + summand) % n) * (n + (n + b * b + summand) % n) + summand) % n
            let mutable divisor : UInt64 = GCD(a - b, n)

            while divisor = 1UL do
                a <- (a * a + summand) % n
                b <- (((n + b * b + summand) % n) * (n + (n + b * b + summand) % n) + summand) % n
                divisor <- GCD(a - b, n)
        
            divisor;


    let PMO (n : UInt64) : UInt64 =
        let mutable cnt = 1000
        let bound = 720
        let mutable search = true
        let mutable res = n
        let mutable f = 1UL
        while (cnt > 0 && search) do
            cnt <- cnt + 1
            let rand = new Random()
            let buffer : byte[] = BitConverter.GetBytes(n)
            rand.NextBytes(buffer)
            let mutable a = BitConverter.ToUInt64(buffer, 0)
            let mutable g = uint64(Math.Pow(float(a), float(bound))) % n
            g <- (g - 1UL) % n
            f <- GCD(g,n)
            if f > 1UL && n > f then
                search <- false
            else () |> ignore
        f

    ///
    ///
    ///
    let rec Factor (n : UInt64) : List<UInt64> =
        match n with
            | 1UL -> [1UL]
            | _ -> let d = Rho n
                   List.concat [[d] ; Factor (n / d)]
