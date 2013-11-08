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
(print "sin_pi 1 =        %.15f\n" (sin_pi 1);
print "sin_pi 2 =        %.15f\n" (sin_pi 2);
print "sin_pi 3 =        %.15f\n" (sin_pi 3);
print "----------------------------------------\n";);;



(*>* Monte Carlo Approximation *>*)
(* monte_pi : int -> float 
 *
 * Pi can be computed using Monte Carlo simulation through a series
 * of experiments. Here is a single experiment:
 *
 *  -- choose a pair of random floating point numbers between 0 and 1
 *  -- call the numbers x and y 
 *      -- think of (x,y) as a point on the plane in the unit square
 *  -- test whether the point falls within the unit circle by measuring
 *     the distance from the point to the origin:  x^2 + y^2 <= 1
 *
 * Now suppose you do m experiments and in n of those experiments, the
 * random point chosen falls within the upper right quarter of the unit circle.
 * Since the area of a circle is known to be pi * r^2 and the area of
 * a square is r^2 (and here we are dealing with a radius/square side
 * of length 1), the following equations hold:

  n    quarter of area of circle     1/4 * pi * r^2
 --- = -------------------------  =  -------------- = 1/4 * pi
  m        area of square                r^2

 * monte_pi takes a positive number indicating the number of random points 
 * n to sample and approximates pi using that number of random points.
 * Calls bad_arg when a non-positive argument is thrown.
 *
 * Note: this estimation method will converge way more slowly than the
 * sinusoidal method. May cause stack overflow with large i.
 *)

Random.init 17;; 

let inUnitCircle () : int = 
    let x = Random.float 1.0 in
    let y = Random.float 1.0 in
    if x *. x +. y *. y <= 1. then 1 else 0;;

let rec numInCircle (i:int) : int = 
    match i with
    | 0 -> 0
    | n -> inUnitCircle() + numInCircle(i - 1);;


let monte_pi (i:int): float =
    if i < 0 then bad_arg (string_of_int i)
    else
        4. *. (float (numInCircle i)) /. (float i);;


(* print the first few approximations *)
(print "monte_pi 1 =      %.15f\n" (monte_pi 1);
print "monte_pi 1000 =   %.15f\n" (monte_pi 1000);
print "monte_pi 100000 = %.15f\n" (monte_pi 100000);
print "monte_pi 250000 = %.15f\n" (monte_pi 250000);
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
    let num = float (fact (4 * i) * (1103 + 26390 * i)) in
    let den = ((float (fact i)) ** 4.) *. (396. ** (4. *. float i)) in
 
    match i with
    | 0 -> factor *. 1103.
    | n -> (factor *. num /. den) +. ramanujan_pi (n - 1);;


(* print the first few approximations *)
(print "ramanujan_pi 0 =  %.15f\n" (1. /. (ramanujan_pi 0));
print "ramanujan_pi 1 =  %.15f\n" (1. /. (ramanujan_pi 1));
print "ramanujan_pi 2 =  %.15f\n" (1. /. (ramanujan_pi 2));
print "----------------------------------------\n\n";);;

