open System

let inline square x = x * x
let inline cube x = x * x * x
let inline average a b = (a + b)/(LanguagePrimitives.GenericOne + LanguagePrimitives.GenericOne)


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


let rec fact_iter product (counter: int) max_count =
    if counter > max_count
    then product
    else fact_iter (product * bigint counter) (counter + 1) max_count

let fact n = fact_iter 1I 1 n


let is_even n = n % 2 = 0


let rec fast_expt b n : bigint =
    if n = 1
    then 1I
    else if is_even n
         then square (fast_expt b (n / 2))
         else b * fast_expt b (n - 1)


let integral f a b dx =
    seq { a .. dx .. b }
    |> Seq.map (fun x -> (f (x + dx / 2.0)) * dx)
    |> Seq.sum


let rec search f (neg: float) (pos: float) =
    let mid = average neg pos
    if (abs (neg - pos)) < 0.001
    then mid
    else
        let test_value = (f mid)
        if test_value > LanguagePrimitives.GenericZero then search f neg mid
        else if test_value < LanguagePrimitives.GenericZero then search f mid pos
        else mid


let half_interval_method f a b =
    let a_value = f a
    let b_value = f b
    if a_value < LanguagePrimitives.GenericZero && b_value > LanguagePrimitives.GenericZero
    then search f a b
    else if b_value < LanguagePrimitives.GenericZero && a_value > LanguagePrimitives.GenericZero
    then search f b a
    else failwith "У аргументов не разные знаки"


let fixed_point f first_guess =
    let rec try_guess guess =
        let next = f guess
        if (abs (guess - next)) < 0.00001
        then next
        else try_guess next

    try_guess first_guess


// let sqrt2 x = fixed_point (fun y -> x / y) 1.0
let sqrt2 x = fixed_point (fun y -> average y (x / y)) 1.0


let cont_frac (n: int -> float) (d: int -> float) k =
    let rec iter i =
        if i = k
        then (n k)/(d k)
        else (n i)/((d i) + iter (i + 1))
        
    iter 1
    
    
let euler_d n =
    if (n + 1) % 3 = 0
    then 2.0 * (float n + 1.0) / 3.0
    else 1.0
    
    
let average_damp (f: float -> float) =
    fun x -> average x (f x)
    

let sqrt3 x = fixed_point (average_damp (fun y -> x / y)) 1.0


let cube_root x = fixed_point (average_damp (fun y -> x / (y * y))) 1.0


let derive g =
    let dx = 0.00001
    
    fun x -> ((g (x + dx)) - (g x))/dx
    
    
let newton_transform g =
    fun x -> x - (g x)/((derive g) x)
    
    
let newton_method g guess =
    fixed_point (newton_transform g) guess


let sqrt4 x =
    newton_method (fun y -> (y * y - x)) 1.0
    
    
let fixed_point_of_transform g transform guess =
    fixed_point (transform g) guess
    
    
let sqrt5 x =
    fixed_point_of_transform (fun y -> x / y) average_damp 1.0


let sqrt6 x =
    fixed_point_of_transform (fun y -> y * y - x) newton_transform 1.0
    
    
let inline double f =
    fun x -> (f (f x))
    
    
let inline compose f g =
    fun x -> (f (g x))
    
    
let rec repeated f n =
    if n = 1
    then f
    else f << repeated f (n - 1)
    
    
let smooth f =
    let dx = 0.00001
    fun x -> ((f (x - dx)) + (f x) + (f (x + dx)))/3.0
    


[<EntryPoint>]
let main _ =
    printfn $"sqrt 2 = %f{sqrt 2.0}"
    printfn $"sqrt' 2 = %f{sqrt1 2.0}"
    printfn $"20! = %A{fact 20}"
    printfn $"3 ^ 100 = %A{fast_expt 3I 100}"
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
    0