let inline square x = x * x

square 2
square 4.0

let inline cube x = x * x * x

cube 3
cube 5.0

let inline average a b = (a + b) / (LanguagePrimitives.GenericOne + LanguagePrimitives.GenericOne) 

average 1 3
average 4.0 5.0

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

sqrt 2.0
sqrt1 2.0

let rec fact_iter product (counter: int) max_count =
    if counter > max_count
    then product
    else fact_iter (product * bigint counter) (counter + 1) max_count

let fact n = fact_iter 1I 1 n

fact 20

let is_even n = n % 2 = 0

let rec fast_power_iter result factor n =
    if n = 0 then result
    else if is_even n then fast_power_iter result (square factor) (n / 2)
    else fast_power_iter (result * factor) factor (n - 1)

let rec fast_power b n =
    fast_power_iter 1I b n
         
fast_power 3I 100

let integral f a b dx =
    seq { a .. dx .. b }
    |> Seq.map (fun x -> (f (x + dx / 2.0)) * dx)
    |> Seq.sum

integral cube 0.0 1.0 0.01

let rec search_zero (f: float -> float) neg pos =
    let mid = average neg pos
    if (abs (neg - pos)) < 0.001
    then mid
    else
        let test_value = (f mid)
        if test_value < LanguagePrimitives.GenericZero then search_zero f neg mid
        else if test_value > LanguagePrimitives.GenericZero then search_zero f mid pos
        else mid
        
search_zero sin 2.0 4.0  

let half_interval_method f a b =
    let a_value = f a
    let b_value = f b
    if a_value > LanguagePrimitives.GenericZero || b_value < LanguagePrimitives.GenericZero
    then search_zero f a b
    else if b_value > LanguagePrimitives.GenericZero || a_value < LanguagePrimitives.GenericZero
    then search_zero f b a
    else failwith "Parameters have different signs"

half_interval_method sin 4.0 2.0

let fixed_point f first_guess =
    let rec try_guess guess =
        let next = f guess
        if (abs (guess - next)) < 0.00001 then next
        else try_guess next

    try_guess first_guess

fixed_point cos 1.0
fixed_point (fun x -> sin x + cos x) 1.0
fixed_point (fun x -> 1.0 + 1.0/x) 1.0
    
//let sqrt2 x = fixed_point (fun y -> x / y) 1.0
let sqrt2 x = fixed_point (fun y -> average y (x / y)) 1.0

sqrt2 2.0

let average_damp (f: float -> float) =
    fun x -> average x (f x)

(average_damp square) 10.0

let sqrt3 x = fixed_point (average_damp (fun y -> x / y)) 1.0

sqrt3 2.0

let cube_root x = fixed_point (average_damp (fun y -> x / (y * y))) 1.0

cube_root 8.0

let derive g =
    let dx = 0.00001
    fun x -> ((g (x + dx)) - (g x))/dx
    
(derive cube) 5.0
    
let newton_transform f =
    fun x -> x - (f x)/((derive f) x)
    
let newton_method f guess =
    fixed_point (newton_transform f) guess


let sqrt4 z = newton_method (fun x -> (square x - z)) 1.0
    
sqrt4 2.0

let cube_root2 z = newton_method (fun x -> (cube x - z)) 1.0

cube_root2 8.0
    
let fixed_point_of_transform f transform guess =
    fixed_point (transform f) guess
    
    
let sqrt5 x = fixed_point_of_transform (fun y -> x / y) average_damp 1.0

sqrt5 2.0

let sqrt6 x = fixed_point_of_transform (fun y -> y * y - x) newton_transform 1.0

sqrt6 2.0
    
let inline double f =
    fun x -> (f (f x))
    
double ((+)1) 5
    
let inline compose f g =
    fun x -> (f (g x))
    
compose square ((+)1) 6
(square << ((+)1)) 6
    
let rec repeate f n =
    if n = 1
    then f
    else f >> repeate f (n - 1)
    
(repeate square 2) 5
    
// let fourth_root x = fixed_point_of_transform (fun y -> x / (y * y * y)) average_damp 1.0
let fourth_root x = fixed_point_of_transform (fun y -> x / cube y) (repeate average_damp 2) 1.0

fourth_root 625.0

// Chapter 2

//let make_rat (n: int) d =
//    fun case -> if case then n else d

let make_rat (n: int) d =
    let dispatch case =
        if case then n else d
        
    dispatch
    
let numerator r = (r true)
let denominator r = (r false)

numerator (make_rat 3 4)
denominator (make_rat 3 4)

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

add_rat (make_rat 3 4) (make_rat 1 2) |> print_rat
