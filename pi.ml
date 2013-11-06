(*
 * pi.ml
 *
 * Implements a few numerical computations of the 
 * mathematical constant pi.
 *
 * @author chindesaurus
 * @version 0.10
 *)


(* helper functions *)
let undefined : unit -> 'a = fun () -> failwith "undefined" ;;
let bad_arg (s:string) = failwith ("bad argument: "^s);;
let rec fact (i:int): int = if i <= 1 then 1 else i * fact (i - 1);;
(*******************)

let name : (string * string) = ("Adam", "Chin");;
let email : string = "adamchin@princeton.edu";;
type year = Freshman | Sophomore | Junior | Senior | Other of string;;

let class_year : year = Junior;;
let print = Printf.printf;;

let print_survey = 
  let (first, last) = name in
  let string_year = 
    (match class_year with
       | Freshman -> "2015"
       | Sophomore -> "2014"
       | Junior -> "2013"
       | Senior -> "2012"
       | Other s -> "Other: " ^ s
    ) in
    (print "----------------------------------------\n";
     print "Name: %s %s\n" first last;
     print "Email: %s\n" email;
     print "Year: %s\n" string_year; 
     print "----------------------------------------\n";);;


(*** Approximating Pi ***)

(*>* Sinusoidal Approximation *>*)
(* sin_pi : int -> float *)
(* The following equations define a function that returns the ith
 * approximation of pi.  
 *
 * approx(0) = 3
 * approx(n+1) = approx(n) + sin(approx(n))
 *
 * Calls bad_arg if the argument i to sin_pi is less than 0.
 * Using this approximation, we converge on many digits of pi very
 * quickly. 
 * The first few digits of pi are 3.14159 26535 89793 23846 26433.
 *
 * Approximation 1 accurately predicts these digits:  3.141
 * Approximation 2 accurately predicts these digits:  3.14159 26535
 * Approximation 3 accurately predicts these digits:  3.14159 26535 89793
 *)

let rec sin_pi (i:int): float =
    if i < 0 then bad_arg (string_of_int i)

    else match i with
    | 0 -> 3.
    | n -> sin_pi (n - 1) +. sin (sin_pi (n - 1));;


(* print the first few approximations *)
(print "sin_pi 1 = %s\n" (string_of_float (sin_pi 1));
print "sin_pi 2 = %s\n" (string_of_float (sin_pi 2));
print "sin_pi 3 = %s\n" (string_of_float (sin_pi 3));
print "----------------------------------------\n";);;



(*>* Ramanujan Infinite Series *>*)
(* ramanujan_pi : int -> float *)
(* Srinivasa Ramanujan's infinite series converges even
 * more rapidly on digits of pi. The series is as follows:
 *
 
 1     2 * sqrt(2)                             (4k)! (1103 + 26390k)
--- = ------------ * (sum from 0 to infinity) ----------------------
pi        9801                                  (k!)^4  *  396^(4k)

 *
 * ramanujan_pi(i) returns the sum of this series from 0 to i.
 * Calls bad_arg if the argument i to ramanujan_pi is less than 0.
 *)

let rec ramanujan_pi (i:int): float = 
    if i < 0 then bad_arg (string_of_int i)
    else 

    let factor = (2. *. sqrt 2.) /. 9801. in
    let num = float_of_int (fact (4 * i) * (1103 + 26390 * i)) in
    let den = ((float_of_int (fact i)) ** 4.) *. (396. ** (4. *. float_of_int i)) in
 
    match i with
    | 0 -> factor *. 1103.
    | n -> (factor *. num /. den) +. ramanujan_pi (n - 1);;


(* print the first few approximations *)
(print "ramanujan_pi 0 = %s\n" (string_of_float (1. /. (ramanujan_pi 0)));
print "ramanujan_pi 1 = %s\n" (string_of_float (1. /. (ramanujan_pi 1)));
print "----------------------------------------\n\n";);;
