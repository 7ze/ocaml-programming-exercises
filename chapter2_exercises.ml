(** @author:7ze
    This file contains the solutions to the exercises from chapter 2. *)

(** [close_enough] is a helper function that checks if a floating point number
   is close enough to another floating point number. This is useful because
   floating point numbers are not exact, and so we cannot check for equality
   with them. Instead, we check if they are close enough to each other. *)

(** [close_enough value expected] is true if [value] is close enough to [expected] *)
let close_enough value expected =
    let tolerance = 1e-5 in
    Float.abs (value -. expected) < tolerance;;

(** [double n] is the double of a number [n] *)
let double n = ( * ) n 2;;
assert (double 2 = 4);;
assert (double 100 = 200);;

(** [cube n] is the floating point cube of a floating point number [n] *)
let cube n = n ** 3.0;;
assert (cube 2.0 = 8.0);;
assert (cube 27.0 = 19683.0);;

(** [sign_of_int n] is the sign of an integer [n] *)
let sign_of_int n = 
    if n < 0 then -1
    else if n = 0 then 0
    else 1;;

assert (sign_of_int (-10) = -1);;
assert (sign_of_int 0 = 0);;
assert (sign_of_int 10 = 1);;

(** [area r] is the area of a circle given the radius [r] *)
let area r = 
    let pi = Float.pi in
    pi *. r ** 2.0;;

assert (close_enough (area 1.0) Float.pi);;
assert (close_enough (area 2.0) (4.0 *. Float.pi));;

(** [rms x y] is the root mean square of two numbers [x] and [y] *)
let rms x y =
    sqrt ((x ** 2. +. y ** 2.) /. 2.);;

assert (close_enough (rms 1. 1.) 1.);;
assert (close_enough (rms 2. 10.) (sqrt 52.));;


(** [date_validator d m] is true if the date [d] is valid for the month [m] *)
let date_validator d m =
    if m = "Jan" || m = "Mar" || m = "May" || m = "Jul" || m = "Aug" || m = "Oct" || m = "Dec" then
        d <= 31
    else if m = "Apr" || m = "Jun" || m = "Sep" || m = "Nov" then
        d <= 30
    else if m = "Feb" then
        d <= 28
    else
        false;;

assert ((date_validator 31 "Jan") = true);;
assert ((date_validator 31 "Feb") = false);;
assert ((date_validator 28 "Feb") = true);;

(** [fib_aux n a b] is a helper function for fib *)
let rec fib_aux n a b =
    if n = 0 then a
    else if n = 1 then b
    else fib_aux (n - 1) b (a + b);;

(** [fib n] is the [n]th fibonacci number *)
let fib n = fib_aux n 0 1;;

assert (fib 0 = 0);;
assert (fib 1 = 1);;
assert (fib 10 = 55);;

(** [find_overflow_fib n] is the smallest fibonacci number that overflows *)
let rec find_overflow_fib n =
    if fib n < 0 then n
    else find_overflow_fib (n + 1);;

Printf.printf "The smallest fibonacci number that overflows is %d\n" (find_overflow_fib 0);;

(** series of functions that take in a boolean and return a boolean, an 'a, or
    an 'a * 'b. Exercises from book *)
let f x = if x then x else x
(* val f: bool -> bool = <fun> *)
let g x y = if y then x else x
(* val g: 'a -> bool -> a' = <fun> *)
let h x y z = if x then y else z
(* val h: bool -> 'a -> 'a -> 'a = <fun> *)
let i x y z = if x then y else y
(* val i: bool -> 'a -> 'b -> 'a = <fun> *)

(** [divide ~numerator ~denominator] is the quotient of two floating point numbers
    [numerator] and [denominator] *)
let divide ~numerator ~denominator = numerator /. denominator
(* function application would look something like 
   divide ~numerator: 3. ~denominator: 2.
   which would give 1.5*)

(** [( +/. ) x y] is an infix operator that finds average of two floating point
    numbers [x] and [y] *)
let ( +/. ) x y = (x +. y) /. 2.;;

assert (close_enough (1. +/. 2.) 1.5);;
assert (close_enough (0. +/. 0.) 0.);;
