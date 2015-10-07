namespace FSNT


module EllipticCurve =

    open System
    open FSNT.Sigma
    open FSNT.Jacobi

    type Point = 
        | XY of x : int32 * y : int32
        | Infinite
           
   
    type EC (A : int32, B : int32, N : int32) =
        member private this.A = A
        member private this.B = B
        member private this.N = N

        member public this.showPoint (p : Point) =
            match p with
            | Infinite -> Console.WriteLine("Infinite")
            | XY (x,y) -> Console.WriteLine("("+string(x)+", "+string(y)+")")

        member public this.add(a : Point, b : Point) =
            match (a,b) with
            | (XY(x,y), XY(u,v)) -> this.sum(a,b)
            | (Infinite, XY(x,y)) -> b
            | (XY(x,y), Infinite) -> a
            | (_,_) -> Infinite



        member private this.sum (a: Point, b : Point) : Point =
            match (a,b) with
            | (XY(x,y), XY(u,v)) ->
                if x = u && y = -1 * v then
                    XY (0,0)
                else
                    let phi : int32 = int32(FSNT.Sigma.Totient(uint64(this.N)))
                    let mutable lambda = 1
                    if x = u && y = v then
                        let denom = (2 * y) % this.N
                        let denomInv = this.powerMod (denom % this.N, phi - 1, this.N)
                        lambda <- ((3 * x * x + this.A) * denomInv) % this.N
                    else
                        lambda <- (v - y) % this.N
                        let inv = this.powerMod ((u - x) % this.N, phi - 1, N) % this.N
                        lambda <- lambda * inv
                    lambda <- lambda % this.N
                    let nx : int32 = (this.N+lambda * lambda - x - u)%this.N
                    let ny : int32 = (this.N - lambda*nx - (y - lambda * x)) % this.N
                    XY ( (nx + this.N)%this.N , (ny + this.N)%this.N)
            | (_,_) -> Infinite


        member private this.powerMod(a,r,M) : int32 =
            let mutable c = r
            let mutable total = 1
            while c > 0 do
                c <- c - 1
                total <- ( total * a ) % M
            total

        member public this.findPoint (x) :  Point Option =
            let possible = (x*x*x + this.A*x + this.B + this.N) % this.N
            let p = uint64(possible)
            let n = uint64(this.N)
            let r = FSNT.Jacobi.JS p n
            if  r = 1 then
                let se = seq{ 0 .. (this.N)}
                let l : seq<int32> = Seq.filter (fun i -> (i*i+this.N)%this.N = possible) se

                if Seq.isEmpty l then 
                    None
                else Some (XY (x ,(Seq.head l )))

            else
                None
    
        member public this.enumerateGroup (x : int32) =
            let seq = {0 .. (this.N)}
            let pts = Seq.map (fun n -> this.findPoint n) seq
            let removeNone = fun n ->
                                    match n with
                                    | None -> false
                                    | Some(n) -> true

            let fin = pts |> Seq.filter removeNone |> Seq.map(fun n -> n.Value )
            let others = Seq.map(fun n  -> this.add(n, n)) fin
            let pai = Infinite //point at infinity
            let elements = Seq.concat [fin ; others]
           // elements |> Seq.iter(fun n -> this.showPoint(n)) 
            elements|> Seq.append ([pai]) |> Seq.distinctBy (fun n -> 
                                        match n with
                                        | Infinite -> 1
                                        | XY (x,y) -> x*7927 + y*17393 - (x*y*11))



    