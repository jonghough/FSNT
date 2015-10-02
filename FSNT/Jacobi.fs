namespace FSNT

open System
open System.Numerics

module Jacobi = 

    let JS n d = 
        match n, d with
            | n, d when n = 0UL -> 0
            | n, d when d % 2UL = 0UL -> 0
            | n, d ->
                let mutable N = n
                let mutable D = d
                let mutable j = 1
                let mutable k = 0
                while N <> 0UL && N<> 1UL do
                    k <- k + 1
                    j <-
                        
                        match N with
                            | 1UL -> 
                                N <- 1UL
                                j
                            | _ ->
                                while N % 2UL = 0UL do
                                    N <- N / 2UL
                                    j <- if D % 8UL = 3UL || D % 8UL = 5UL then
                                            -1 * j
                                         else j
                                let temp = (D + N) % N
                                D <- N
                                N <- temp

                                if N % 4UL = 3UL || D % 4UL = 3UL then
                                    N <- N % D 
                                    -1 * j
                                else j
                    
                j