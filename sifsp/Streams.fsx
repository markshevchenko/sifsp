// Chapter 3

open System

let print n title = Seq.take n >> Seq.map string >> String.concat ", " >> printfn "%s: %s" title    
let print20 title = print 20 title

let integers1 = seq {
    let mutable value = 1I
    while true do
        yield value
        value <- value + 1I
}

let integers2 =
    let rec gen i = seq {
        yield i
        yield! gen (i + 1I)
    }

    gen 1I

let integers = Seq.unfold (fun i -> Some (i + 1I, i + 1I)) 0I

print20 "integers" integers
print20 "integers" integers1
print20 "integers" integers2

let random seed = Seq.unfold (fun previous -> let next = 1664525u * previous + 1013904223u
                                              Some (float next/float UInt32.MaxValue, next)) seed 

print20 "random 11786u" (random 11786u)

let fibs1 =
    let rec gen a b = seq {
        yield a
        yield! gen b (a + b)
    }
        
    gen 0I 1I
    
print20 "fibs" fibs1

let fibs = Seq.unfold (fun (a, b) -> Some (a, (b, a + b))) (0I, 1I)

print20 "fibs" fibs
    
let rec bigint_sqrt n =
    if n < 0I then failwith "n can't be negative"
    else if n < 2I then n
    else
        let small = (bigint_sqrt (n >>> 2)) <<< 1
        let large = small + 1I
        if large * large > n then small
        else large

bigint_sqrt 1000000I

let is_prime n =
    if n <= 1I then false
    else if n = 2I then true
    else if n % 2I = 0I then false
    else let sqrt_n = bigint_sqrt n
         { 3I..sqrt_n } |> Seq.forall (fun c -> n % c <> 0I)

is_prime 7I
is_prime 1001I

let primes1 = integers |> Seq.filter is_prime

print20 "primes" primes1

let primes2 =
    let rec gen stream = seq {
        let next_prime = Seq.head stream
        yield next_prime
//        let filtered_tail = Seq.filter (fun i -> i % next_prime <> 0I) (Seq.tail stream)
//        let filtered_tail = stream |> Seq.tail |> Seq.filter (fun i -> i % next_prime <> 0I)
        yield! stream |> Seq.tail |> Seq.filter (fun i -> i % next_prime <> 0I) |> gen
    }
        
    gen (Seq.skip 1 integers)
    
print20 "primes" primes2

let primes = Seq.unfold (fun (n, primes) -> if List.exists (fun prime -> n % prime = 0I) primes
                                            then Some ((n, false), (n + 1I, primes))
                                            else Some ((n, true), (n + 1I, n::primes))) (2I, [])
          |> Seq.filter snd
          |> Seq.map fst
//          |> Seq.filter (fun (_, is_prime) -> is_prime)
//          |> Seq.map (fun (n, _) -> n)
    
print20 "primes" primes

let ones1 =
    let rec gen () = seq {
        yield 1I
        yield! gen ()
    }
        
    gen ()
    
print20 "ones" ones1

module Seq =
    let cycle value = seq {
        while true do
            yield value
    }

    let triplewise (source: seq<_>) = seq {
        use enumerator = source.GetEnumerator()
        if enumerator.MoveNext() then
            let mutable first = enumerator.Current
            if enumerator.MoveNext() then
                let mutable second = enumerator.Current
                while enumerator.MoveNext() do
                    let third = enumerator.Current
                    yield (first, second, third)
                    first <- second
                    second <- third
    }
    
// module Seq = let cycle value = seq { while true do yield value }

let ones = Seq.cycle 1I

print20 "ones" ones
    
let integers3 =
    let rec gen () = seq {
        yield 1I
        yield! Seq.zip ones (gen ()) |> Seq.map (fun (a, b) -> a + b)
    }

    gen ()
    
print20 "integers" integers3 

let fibs2 =
    let rec gen () = seq {
        yield 0I
        yield 1I
        yield! Seq.zip (gen ()) (gen () |> Seq.skip 1) |> Seq.map (fun (a, b) -> a + b)
    }
    
    gen ()
    
let fibs3 =
    let rec gen () = seq {
        yield 0I
        yield 1I
        let fibs = Seq.cache (gen ())
        yield! Seq.zip fibs (Seq.skip 1 fibs) |> Seq.map (fun (a, b) -> a + b)
    }
    
    gen ()
    
print20 "fibs" fibs2
print 40 "fibs 40" fibs3    

let sqrt_stream x =
    let improve guess x = (guess + x/guess)/2.0
    let rec gen () = seq {
        yield 1.0
        yield! gen () |> Seq.map (fun guess -> improve guess x)
    }
    
    gen ()
    
print 40 "sqrt stream 40" (sqrt_stream 2.0)

let inline partial_sums2< ^a when ^a: (static member (+): ^a * ^a -> ^a)> (stream: ^a seq) =
    let rec gen stream = seq {
        yield Seq.head stream
        yield! Seq.zip (Seq.tail stream) (gen stream) |> Seq.map (fun (a, b) -> a + b)
    }
    
    gen stream
    
print20 "partials_sums integers" (partial_sums2 integers)

let inline partial_sums1 source = Seq.scan (+) (Seq.head source) (Seq.tail source)

print20 "partials_sums integers" (partial_sums1 integers)

let inline partial_sums source = Seq.scan (+) LanguagePrimitives.GenericZero source |> Seq.skip 1

print20 "partials_sums integers" (partial_sums integers)
    
let rec pi_terms1 n =
    seq {
        yield 1.0/n
//        yield! pi_summands (n + 2.0) |> Seq.map (fun x -> -x)
        yield! pi_terms1 (n + 2.0) |> Seq.map (~-)
    }
    
print20 "pi terms" (pi_terms1 1.0)

let pi_terms = Seq.unfold (fun n -> Some (1.0 / n, if n < 0.0 then -n + 2.0 else -n - 2.0)) 1.0

print20 "pi_terms" pi_terms


//let pi_stream = pi_terms1 1.0 |> partial_sums2 |> Seq.map (fun x -> 4.0 * x)
let pi_stream = pi_terms |> partial_sums |> Seq.map ((*)4.0)

print20 "pi stream" pi_stream

let rec euler_transform2 (stream: float seq) = seq {
    let cache = Seq.cache stream
    let s0 = Seq.item 0 cache
    let s1 = Seq.item 1 cache
    let s2 = Seq.item 2 cache
    yield s2 - (s2 - s1) * (s2 - s1)/(s0 - 2.0 * s1 + s2)
    yield! euler_transform2 (Seq.tail cache)
}

print20 "euler transform of pi stream" (euler_transform2 pi_stream)

seq { 1 .. 30 } |> Seq.triplewise |> print20 "triples"

let euler_transform source =
    source
 |> Seq.triplewise
 |> Seq.map (fun (s0, s1, s2) -> s2 - (s2 - s1) * (s2 - s1)/(s0 - 2.0 * s1 + s2))

print20 "euler transform of pi stream" (euler_transform pi_stream)

let rec make_tableau (transform: float seq -> float seq) (source: float seq) = seq {
    yield source
    yield! make_tableau transform (transform source)
}

let accelerated_sequence transform source =
    source |> make_tableau transform |> Seq.map Seq.head
    
print 10 "accelerated sequence of pi stream" (accelerated_sequence euler_transform pi_stream)
