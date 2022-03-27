module Streams

open System

// Chapter 3

let integers =
    let rec gen i =
        seq {
            yield i
            yield! gen (i + 1I)
        }

    gen 1I


let fibs =
    let rec gen a b =
        seq {
            yield a
            yield! gen b (a + b)
        }
        
    gen 0I 1I
    
    
let rec bigint_sqrt n =
    if n < 0I then failwith "n can't be negative"
    else if n < 2I then n
    else
        let small = (bigint_sqrt (n >>> 2)) <<< 1
        let large = small + 1I
        if large * large > n then small
        else large


let is_prime n =
    if n <= 1I
    then false
    else if n = 2I
    then true
    else if n % 2I = 0I
    then false
    else
        let sqrt_n = bigint_sqrt n
        { 3I..sqrt_n } |> Seq.forall (fun c -> n % c <> 0I)


let primes = integers |> Seq.filter is_prime


let primes1 =
    let rec gen stream =
        seq {
            let next_prime = Seq.head stream
            yield next_prime
//            let filtered_tail = Seq.filter (fun i -> i % next_prime <> 0I) (Seq.tail stream)
            let filtered_tail = stream |> Seq.tail |> Seq.filter (fun i -> i % next_prime <> 0I)
            yield! gen filtered_tail
        }
        
    gen (Seq.skip 1 integers)
    
    
let ones =
    let rec gen () =
        seq {
            yield 1I
            yield! gen ()
        }
        
    gen ()
    
    
let integers1 =
    let rec gen () = seq {
        yield 1I
        yield! Seq.zip ones (gen ()) |> Seq.map (fun (a, b) -> a + b)
    }

    gen ()
    
    
let fibs1 =
    let rec gen () = seq {
        yield 0I
        yield 1I
        yield! Seq.zip (gen ()) (gen () |> Seq.skip 1) |> Seq.map (fun (a, b) -> a + b)
    }
    
    gen ()
    
    
let two_powers =
    let rec gen () = seq {
        yield 1I
        yield! gen () |> Seq.map (fun i -> 2I * i)
    }
    
    gen ()
    
    
let sqrt_stream x =
    let improve guess x = (guess + x/guess)/2.0
    let rec gen () = seq {
        yield 1.0
        yield! gen () |> Seq.map (fun guess -> improve guess x)
    }
    
    gen()
    
    
let inline partial_sums< ^a when ^a: (static member (+): ^a * ^a -> ^a)> (stream: ^a seq) =
    let rec gen stream = seq {
        yield Seq.head stream
        yield! Seq.zip (Seq.tail stream) (gen stream) |> Seq.map (fun (a, b) -> a + b)
    }
    
    gen stream
    
    
let rec pi_summands n =
    seq {
        yield 1.0/n
//        yield! pi_summands (n + 2.0) |> Seq.map (fun x -> -x)
        yield! pi_summands (n + 2.0) |> Seq.map (~-)
    }


let pi_stream = pi_summands 1.0 |> partial_sums |> Seq.map (fun x -> 4.0 * x)


let rec euler_transform (stream: float seq) = seq {
    let cache = Seq.cache stream
    let s0 = Seq.item 0 cache
    let s1 = Seq.item 1 cache
    let s2 = Seq.item 2 cache
    yield s2 - (s2 - s1) * (s2 - s1)/(s0 - 2.0 * s1 + s2)
    yield! euler_transform (Seq.tail cache)
}


let rec make_tableau (transform: float seq -> float seq) (stream: float seq) = seq {
    yield stream
    yield! make_tableau transform (transform stream)
}

let accelerated_sequence transform stream =
    stream |> make_tableau transform |> Seq.map Seq.head


let rec interleave (s1: 'a seq) (s2: 'a seq) = seq {
    if Seq.isEmpty s1
    then yield! s2
    else
        yield Seq.head s1
        yield! interleave s2 (Seq.tail s1)
}


let rec pairs (s1: 'a seq) (s2: 'a seq) = seq {
    let cache2 = Seq.cache s2
    yield (Seq.head s1, Seq.head cache2)
    yield! interleave (cache2 |> Seq.tail |> Seq.map (fun e -> (Seq.head s1, e))) (pairs (Seq.tail s1) (Seq.tail cache2))
}