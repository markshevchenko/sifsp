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
    0