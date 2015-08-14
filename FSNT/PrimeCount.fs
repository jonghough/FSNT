namespace FSNT

open System
module PrimeCount = 

    ///<summary>
    /// Primality test by trial division for unsigned int32s.
    ///</summary>
    let IsPrime ( i : UInt32) = 
        match i with
        | 0u -> false
        | 1u -> false
        | 2u -> true
        | 3u -> true
        | _ ->
            if i % 2u = 0u then false
            else if i % 3u = 0u then false
            else
                let max : UInt32 = uint32(Math.Round( Math.Sqrt(double(i)+1.0)))
                seq { 5u .. max }|> Seq.tryFind (fun x ->  i % x = 0u) = None

    
    ///<summary>
    /// Gets the ith prime, beginning with the rule that 2 is the first prime
    /// (not the zeroth prime)
    ///</summary>
    let GetPrime (i : UInt32) = 
        match i with
        | 1u -> 2u
        | _ -> 
            let mutable x : UInt32 = 3u
            let mutable count : UInt32 = 1u
            let mutable p : UInt32 = 2u
            while count < i do
                if IsPrime x then
                    count <- count + 1u
                    p <- x
                else
                    count |> ignore
                x <- x + 1u
            p


    ///<summary>
    /// Gets the next prime after the given integer
    ///</summary>
    let GetNextPrime (i : UInt32) =
        let max : UInt32 = 2u*(i+1u)
        let x = seq{(i+1u) .. max} |> Seq.tryFind (fun x -> IsPrime x)
        match x with
        | Some x -> x
        | _ -> 3u


    ///<summary>
    /// Returns Legendre Phi(m,n)
    ///</summary>
    let rec Phi (m : float) (n : uint32) =
        if m < 0.0 then raise (Exception("EX"))
        else if n = 0u then uint32(m)
        else
            let k = GetPrime n
            (Phi m (n - 1u)) - Phi ((m / float(k))) (n - 1u)


    ///<summary>
    /// Returns the numbe rof primes less than (or equal to) the given float.
    ///</summary>
    let rec Pi( i : float) : uint32 =
        if i < 2.0 then 0u
        else if i < 3.0 then 1u
        else if i < 5.0 then 2u
        else if i < 7.0 then 3u
        else if i < 11.0 then 4u
        else
            let cbrt = Pi(Math.Pow(i, 1.0/3.0))
            let sqrt = Math.Sqrt(i)
            let h = Pi(sqrt)
            let mu = h - cbrt
            let muf = float(mu)
            let cbrtf = float(cbrt)
            let summand1 =(Phi i cbrt)+ cbrt * (mu + 1u) + uint32(float(mu * mu - mu) * 0.5) - 1u
            let SumPi( m : float) (n : uint32) (r : uint32) =
                let mutable l = 1u
                let mutable total = 0u
                let mutable p = GetPrime n
                while l <= r do
                    l <- l + 1u
                    p <- GetNextPrime p
                    total <- total + Pi(m / float(p))
                total
            summand1 - SumPi i cbrt mu


       
   


    


