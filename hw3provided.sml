(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

fun only_capitals x = List.filter (fn str => Char.isUpper(String.sub(str,0))) x

fun longest_string1 list = List.foldl (fn (s,acc) => if String.size s > String.size acc then s else acc) "" list

val longest_string2 = List.foldl (fn (s,acc) => if String.size s< String.size acc then acc else s) ""

fun longest_string_helper f strings =
  foldl (fn(x,y) => if f(String.size x, String.size y) then x else y) "" strings

val longest_string3 =
  longest_string_helper (fn (x,y) => x > y)

val longest_string4 =
  longest_string_helper (fn (x,y) => x >= y)

fun longest_capitalized list =( longest_string1 o only_capitals ) list

val rev_string = implode o rev o explode
			     
fun first_answer f list =
    case list of
	[] => raise NoAnswer
      | x::xs => case f x of
		     SOME v => v
		   | NONE => first_answer f xs

fun all_answers f list =
    let
	fun helper acc list =
	    case (acc,list) of
		(_,[]) => acc
	      | (SOME v,x::xs) =>( case f x of
			     SOME xv => helper (SOME (xv @ v)) xs
				    | NONE => NONE)
			      | _ => NONE 
					  
    in
	helper (SOME []) list
    end

val count_wildcards = g (fn () =>1) (fn x => 0)

val count_wild_and_variable_lengths = g (fn () => 1) (fn s => String.size s)

fun count_some_var (s,p) =
    g (fn() => 0) (fn str => if(s = str) then 1 else 0) p

fun check_pat p =
    let
	fun filterString pat acc = case pat of
				       Variable x => x :: acc
				     | ConstructorP (_, p) => filterString p acc
				     | TupleP ps =>
				       List.foldl
					   (fn (p, acc) => (filterString p []) @ acc) [] ps
				     | _ => []
    in
	let
	    val strList = filterString p []
	    fun checkDuplicate remList = 
		case remList of
		    [] => true
		  | x :: xs => if List.exists (fn item => item = x) xs
			       then false
			       else checkDuplicate xs
	in
	    checkDuplicate strList
	end
    end

fun match valptrn =
  case valptrn
    of (_, Wildcard) => SOME []
     | (v, Variable s) => SOME [(s, v)]
     | (Unit, UnitP) => SOME []
     | (Const v, ConstP v') => if v = v' then SOME [] else NONE
     | (Tuple vs, TupleP ps) =>
         if length(vs) = length(ps)
         then all_answers match (ListPair.zip(vs, ps))
         else NONE
     | (Constructor(s2, v), ConstructorP(s1, p)) => 
         if s1 = s2
         then match(v, p)
         else NONE
     | _ => NONE

fun first_match v ptrnlist =
  SOME(first_answer (fn p => match(v, p)) ptrnlist)
  handle NoAnswer => NONE


    
    


	
    
