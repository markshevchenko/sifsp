open System
open Deriv
open Streams
open Parser

// Chapter 1

let inline square x = x * x
let inline cube x = x * x * x
let inline average a b = LanguagePrimitives.DivideByInt (a + b) 2


let sqrt1 x =
    let is_good_enough guess x =
        (abs (square guess - x)) < 0.001

    let improve guess x =
        average guess (x / guess)

    let rec sqrt_iter guess x =
        if is_good_enough guess x
        then guess
        else sqrt_iter (improve guess x) x

    sqrt_iter 1.0 x

    //printfn $"sqrt 2 = %f{sqrt 2.0}"
    //printfn $"sqrt1 2 = %f{sqrt1 2.0}"


let rec fact_iter product (counter: int) max_count =
    if counter > max_count
    then product
    else fact_iter (product * bigint counter) (counter + 1) max_count

let fact n = fact_iter 1I 1 n

    //printfn $"20! = %A{fact 20}"

let is_even n = n % 2 = 0

let rec fast_power b n : bigint =
    if n = 1
    then 1I
    else if is_even n
         then square (fast_power b (n / 2))
         else b * fast_power b (n - 1)

    //printfn $"3 ^ 100 = %A{fast_power 3I 100}"

let integral f a b dx =
    seq { a .. dx .. b }
    |> Seq.map (fun x -> (f (x + dx / 2.0)) * dx)
    |> Seq.sum

    //printfn $"integral cube 0 1 0.01 = %f{integral cube 0.0 1.0 0.01}"

let rec search_zero f (neg: float) (pos: float) =
    let mid = average neg pos
    if (abs (neg - pos)) < 0.001
    then mid
    else
        let test_value = (f mid)
        if test_value > LanguagePrimitives.GenericZero then search_zero f neg mid
        else if test_value < LanguagePrimitives.GenericZero then search_zero f mid pos
        else mid

    //printfn $"pi = search sin 2 4 = %f{half_interval_method sin 2.0 4.0}"

let half_interval_method f a b =
    let a_value = f a
    let b_value = f b
    if a_value < LanguagePrimitives.GenericZero && b_value > LanguagePrimitives.GenericZero
    then search_zero f a b
    else if b_value < LanguagePrimitives.GenericZero && a_value > LanguagePrimitives.GenericZero
    then search_zero f b a
    else failwith "Parameters have different signs"


let fixed_point f first_guess =
    let rec try_guess guess =
        let next = f guess
        if (abs (guess - next)) < 0.00001
        then next
        else try_guess next

    try_guess first_guess

    //printfn $"fixed_point cos 1 = %f{fixed_point cos 1.0}"
    //printfn $"fixed_point (sin y + cos y) 1 = %f{fixed_point (fun y -> sin y + cos y) 1.0}"
    
// let sqrt2 x = fixed_point (fun y -> x / y) 1.0
let sqrt2 x = fixed_point (fun y -> average y (x / y)) 1.0

    //printfn $"sqrt'' 2 = %f{sqrt2 2.0}"
    //printfn $"phi = fixed_point (fun x -> 1 + 1/x) 1 = %f{fixed_point (fun x -> 1.0 + 1.0/x) 1.0}"

let cont_frac (n: int -> float) (d: int -> float) k =
    let rec iter i =
        if i = k
        then (n k)/(d k)
        else (n i)/((d i) + iter (i + 1))
        
    iter 1
    
    //printfn $"1/phi cont_frac (fun i -> 1.0) (fun i -> 1.0) 10 = %f{cont_frac (fun _ -> 1.0) (fun _ -> 1.0) 10}"
    
let euler_d n =
    if (n + 1) % 3 = 0
    then 2.0 * (float n + 1.0) / 3.0
    else 1.0
    
    //printfn $"e - 2 = cont_frac (fun i -> 1.0) euler_d 10 = %f{cont_frac (fun _ -> 1.0) euler_d 10}"
    
let average_damp (f: float -> float) =
    fun x -> average x (f x)

    //printfn $"(average_dump square) 10 = %f{(average_damp square) 10.0}"

let sqrt3 x = fixed_point (average_damp (fun y -> x / y)) 1.0

    //printfn $"sqrt''' 2 = %f{sqrt3 2.0}"

let cube_root x = fixed_point (average_damp (fun y -> x / (y * y))) 1.0

    //printfn $"cube_root 8.0 = %f{cube_root 8.0}"

let derive g =
    let dx = 0.00001
    
    fun x -> ((g (x + dx)) - (g x))/dx
    
    //printfn $"(derive cube) 5 = %f{(derive cube) 5.0}"
    
let newton_transform f =
    fun x -> x - (f x)/((derive f) x)
    
    
let newton_method f guess =
    fixed_point (newton_transform f) guess


let sqrt4 x =
    newton_method (fun y -> (y * y - x)) 1.0
    
    //printfn $"sqrt'''' 2 = %f{sqrt4 2.0}"
    
let fixed_point_of_transform g transform guess =
    fixed_point (transform g) guess
    
    
let sqrt5 x = fixed_point_of_transform (fun y -> x / y) average_damp 1.0


let sqrt6 x = fixed_point_of_transform (fun y -> y * y - x) newton_transform 1.0
    
    
let inline double f =
    fun x -> (f (f x))
    
    //printfn $"double ((+)1) 5 = %d{double ((+)1) 5}"
    
let inline compose f g =
    fun x -> (f (g x))
    
    //printfn $"compose square ((+)1) 6=%d{compose square ((+)1) 6}"
    //printfn $"(square << ((+)1)) 6 = %d{(square << ((+)1)) 6}"
    
let rec repeated f n =
    if n = 1
    then f
    else f >> repeated f (n - 1)
    
    //printfn $"(repeated square 2) 5 = %d{(repeated square 2) 5}"
    
let smooth f =
    let dx = 0.00001
    fun x -> ((f (x - dx)) + (f x) + (f (x + dx)))/3.0
    

// let fourth_root x = fixed_point_of_transform (fun y -> x / (y * y * y)) average_damp 1.0
let fourth_root x = fixed_point_of_transform (fun y -> x / (y * y * y)) (repeated average_damp 2) 1.0

    //printfn $"fourth_root 625.0 = %f{fourth_root 625.0}"

// Chapter 2

//let make_rat (n: int) d =
//    fun case -> if case then n else d

let make_rat (n: int) d =
    //let dispatch case =
    //    if case then n else d
        
    //dispatch
    fun case -> if case then n else d
    
let numerator r = (r true)
let denominator r = (r false)

let add_rat a b =
    let a_numerator = numerator a
    let a_denominator = denominator a
    let b_numerator = numerator b
    let b_denominator = denominator b

    make_rat (a_numerator * b_denominator + b_numerator * a_denominator) (a_denominator * b_denominator)

let print_rat r =
    let r_numerator = numerator r
    let r_denominator = denominator r
    printfn "%d/%d" r_numerator r_denominator

    //printfn $"numerator (make-rat 3 4) = %d{numerator (make_rat 3 4)}"
    //printfn $"denominator (make-rat 3 4) = %d{denominator (make_rat 3 4)}"

//let zero = fun _ -> id
//let add_1 n = fun f -> fun x -> (f ((n f) x))

type List<'a> =
    | Nil
    | Node of 'a * List<'a>
    
let cons element list =
    List.Node (element, list)
    
let rec length list =
    match list with
    | Nil -> 0
    | Node (_, tail) -> 1 + (length tail)

let rec contains (value: 'a when 'a : equality) list =
    match list with
    | Nil -> false
    | Node (head, tail) -> if head = value
                           then true
                           else contains value tail                           
                           
let rec filter predicate list =
    match list with
    | Nil -> Nil
    | Node (head, tail) -> if predicate head
                           then cons head (filter predicate tail)
                           else filter predicate tail
                           
let rec for_all handler list =
    match list with
    | Nil -> ()
    | Node (head, tail) -> handler head
                           for_all handler tail
                           
let print list =
    match list with
    | Nil -> ()
    | Node (head, tail) -> printf $"%d{head}"
                           for_all (printf ", %d") tail
                           
    printfn ""
    

[<EntryPoint>]
let main _ =
    printfn $"sqrt 2 = %f{sqrt 2.0}"
    printfn $"sqrt1 2 = %f{sqrt1 2.0}"
    printfn $"20! = %A{fact 20}"
    printfn $"3 ^ 100 = %A{fast_power 3I 100}"
    printfn $"sum 1..100 = %d{seq { 1..100 } |> Seq.sum}"
    printfn $"sum_cubes 1..10 = %d{seq { 1..10 } |> Seq.map cube |> Seq.sum}"
    printfn $"pi_sum = %f{8.0 * (seq { 1.0 .. 4.0 .. 1001.0 }
                              |> Seq.map (fun x -> 1.0 / (x * (x + 2.0)))
                              |> Seq.sum)}"
    printfn $"integral cube 0 1 0.01 = %f{integral cube 0.0 1.0 0.01}"
    printfn $"pi = search sin 2 4 = %f{half_interval_method sin 2.0 4.0}"
    printfn $"fixed_point cos 1 = %f{fixed_point cos 1.0}"
    printfn $"fixed_point (sin y + cos y) 1 = %f{fixed_point (fun y -> sin y + cos y) 1.0}"
    printfn $"sqrt'' 2 = %f{sqrt2 2.0}"
    printfn $"phi = fixed_point (fun x -> 1 + 1/x) 1 = %f{fixed_point (fun x -> 1.0 + 1.0/x) 1.0}"
    printfn $"1/phi cont_frac (fun i -> 1.0) (fun i -> 1.0) 10 = %f{cont_frac (fun _ -> 1.0) (fun _ -> 1.0) 10}"
    printfn $"e - 2 = cont_frac (fun i -> 1.0) euler_d 10 = %f{cont_frac (fun _ -> 1.0) euler_d 10}"
    printfn $"(average_dump square) 10 = %f{(average_damp square) 10.0}"
    printfn $"sqrt''' 2 = %f{sqrt3 2.0}"
    printfn $"cube_root 8.0 = %f{cube_root 8.0}"
    printfn $"(derive cube) 5 = %f{(derive cube) 5.0}"
    printfn $"sqrt'''' 2 = %f{sqrt4 2.0}"
    printfn $"double ((+)1) 5 = %d{double ((+)1) 5}"
    printfn $"compose square ((+)1) 6=%d{compose square ((+)1) 6}"
    printfn $"(square << ((+)1)) 6 = %d{(square << ((+)1)) 6}"
    printfn $"(repeated square 2) 5 = %d{(repeated square 2) 5}"
    printfn $"fourth_root 625.0 = %f{fourth_root 625.0}"
    printfn $"numerator (make-rat 3 4) = %d{numerator (make_rat 3 4)}"
    printfn $"denominator (make-rat 3 4) = %d{denominator (make_rat 3 4)}"

    let list = (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 (cons 6 Nil)))))) 
    print list
    list |> filter (fun n -> n % 2 = 0) |> print
    printfn $"%A{<@ fun (x: float) -> x * x @>}"
    printfn $"%A{deriv <@ fun (x: float) -> x * x @>}"

    printfn $"bigint_sqrt 1000000I = %A{bigint_sqrt 1000000I}"
    integers |> Seq.take 10 |> Seq.map string |> String.concat ", " |> printfn "integers: %s" 
    fibs |> Seq.take 10 |> Seq.map string |> String.concat ", " |> printfn "fibs: %s"
    primes |> Seq.take 10 |> Seq.map string |> String.concat ", " |> printfn "primes: %s"
    primes1 |> Seq.take 10 |> Seq.map string |> String.concat ", " |> printfn "primes': %s"
    ones |> Seq.take 10 |> Seq.map string |> String.concat ", " |> printfn "ones: %s"
    integers1 |> Seq.take 10 |> Seq.map string |> String.concat ", " |> printfn "integers': %s" 
    fibs1 |> Seq.take 10 |> Seq.map string |> String.concat ", " |> printfn "fibs': %s"
    two_powers |> Seq.take 10 |> Seq.map string |> String.concat ", " |> printfn "2 powers': %s"
    sqrt_stream 2.0 |> Seq.take 10 |> Seq.map string |> String.concat ", " |> printfn "sqrt_stream: %s"
    partial_sums integers |> Seq.take 10 |> Seq.map string |> String.concat ", " |> printfn "partials_sums integers: %s"
    pi_summands 1.0 |> Seq.take 10 |> Seq.map string |> String.concat ", " |> printfn "pi summands: %s"
    pi_stream |> Seq.take 10 |> Seq.map string |> String.concat ", " |> printfn "pi_stream: %s"
    euler_transform pi_stream |> Seq.take 10 |> Seq.map string |> String.concat ", " |> printfn "euler_transform  pi_stream: %s"
    accelerated_sequence euler_transform pi_stream |> Seq.take 10 |> Seq.map string |> String.concat ", " |> printfn "accelerated_sequence euler_transform  pi_stream: %s"
    pairs integers integers |> Seq.take 10 |> Seq.map string |> String.concat ", " |> printfn "pairs integers integers = %s"
    
    "321" |> List.ofSeq |> digits |> printfn "%A"
    "321" |> List.ofSeq |> number |> printfn "%A"
    "3.14159265" |> List.ofSeq |> number |> printfn "%A"
    "+3.14159265" |> List.ofSeq |> plus |> printfn "%A"
    "-3.14159265" |> List.ofSeq |> minus |> printfn "%A"
    "*3.14159265" |> List.ofSeq |> star |> printfn "%A"
    "/3.14159265" |> List.ofSeq |> slash |> printfn "%A"
    "1*2*3/4" |> List.ofSeq |> expression |> printfn "%A"
    "1*2+3*4" |> List.ofSeq |> expression |> printfn "%A"
    "1*(2+3)*4" |> List.ofSeq |> expression |> printfn "%A"
    "sin(3.1415/6)" |> List.ofSeq |> expression |> printfn "%A"

    0