namespace FSNT


module EllipticCurve =

    open System
    open Sigma

    type Point(x, y,isInf : bool) =
        member public this.x : int32 = x
        member public this.y : int32 = y
        member private this.isInf = isInf

        member public this.IsInfinite() =
            this.isInf

       

    type EC (A : int32, B : int32, N : int32) =
        member private this.A = A
        member private this.B = B
        member private this.N = N


        member public this.add(a : Point, b : Point) =
            match a.IsInfinite() with
            | true -> b
            | _ -> 
                match b.IsInfinite() with
                | true -> a
                | _ -> this.sum(a, b)



        member private this.sum (a: Point, b : Point) : Point =
            if a.x = b.x && a.y = -1 * b.y then
                Point(0,0,false)
            else
                let phi : int32 = int32(Sigma.Totient(uint64(this.N)))
                let mutable lambda = 1
                if a.x = b.x && a.y = b.y then
                    let denom = (2 * a.y) % this.N
                    let denomInv = this.powerMod (denom % this.N, phi - 1, this.N)
                    lambda <- (3 * a.x * a.x + this.A) % this.N
                else
                    lambda <- (b.y - a.y) % this.N
                    let inv = this.powerMod ((b.x - a.x) % this.N, phi - 1, N) % this.N
                    lambda <- lambda * inv
                lambda <- lambda % this.N
                let nx : int32 = (this.N+lambda * lambda - a.x - b.x)%this.N
                let ny : int32 = (this.N - lambda*nx - (a.x - lambda * a.x)) % this.N
                Point( (nx + this.N)%this.N , (ny + this.N)%this.N, false)


        member private this.powerMod(a,r,M) : int32 =
            let mutable c = r
            let mutable total = 1
            while c > 0 do
                c <- c - 1
                total <- ( total * a ) % M
            total

    