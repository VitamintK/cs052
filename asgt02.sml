(*
* asgt02.sml
*
* Kevin Wang
* 19 Sept 2013
* CS 52, Assignment 02
*
*)

(* 1 *)
fun square n : int = n * n;

fun squareAll xl = map square xl;

(* 2 *)
fun myZip nil _ = nil
  | myZip _ nil = nil
  | myZip (x::xs) (y::ys) = [(x,y)]@(myZip xs ys);


(* 3 *)
fun split xl =
    let
        fun splitaux nil i = nil
          | splitaux (x::xs) i =
            if i = 0
               then (splitaux xs (1-i))
               else x::(splitaux xs (1-i));
    in
        ((splitaux xl 1) , (splitaux xl 0))
    end;

(* 4 *)

fun cartesian nil _ = []
  | cartesian _ nil = []
  | cartesian (x::xs) (y::ys) = 
    let 
        fun consAll i nil = []
          | consAll i (x::xs) = (i,x)::(consAll i xs);
    in
        (consAll x (y::ys))@(cartesian xs (y::ys))
    end;

(* 5 *)

fun addAllNew xl xs = map (op +) (cartesian xl xs);

(* 6 *)

fun change xl i =
    let
        fun changeaux nil _ _ = []
		  | changeaux xl 0 curchange = [curchange]
		  | changeaux (x::xs) i curchange =
			if i<0 then nil
			else (changeaux (x::xs) (i-x) (x::curchange))@(changeaux xs i curchange)
    in
        changeaux xl i []
    end;

(* 7 *)
exception NegativeNumber;
exception BadDigit;

fun toDigitList 0 = []
  | toDigitList n =
    if n<0
    then raise NegativeNumber
    else (n mod 10)::(toDigitList (n div 10));

fun fromDigitList nil = 0
  | fromDigitList (x::xs) = 
    if x>9 orelse x<0
    then raise BadDigit
    else x + (10 * (fromDigitList xs));

(* 8 *)
local
    fun addDigitListaux carry nil nil = if carry = 0 then [] else [carry]
      | addDigitListaux carry lft nil = addDigitListaux carry lft [0]
      | addDigitListaux carry nil rht = addDigitListaux carry [0] rht
      | addDigitListaux carry (x::xs) (y::ys) = 
        let
            val s = x+y+carry
        in
            if s<10
            then s::(addDigitListaux 0 xs ys)
            else (s-10)::(addDigitListaux 1 xs ys)
        end;
in
    val addDigitList = addDigitListaux 0;
end;

local 
    fun multiDigitAux carry nil nil = if carry = 0 then [] else [carry]
      | multiDigitAux carry lft nil = if carry = 0 then [] else [carry]
      | multiDigitAux carry nil rht = multiDigitAux carry [0] rht
      | multiDigitAux carry (x::xs) (y::ys) =
        let
            val dhuahfsafufsadiiihluasdfhiulh = carry + (x * y);
        in
            if dhuahfsafufsadiiihluasdfhiulh>9
            then (dhuahfsafufsadiiihluasdfhiulh mod 10)::(multiDigitAux (dhuahfsafufsadiiihluasdfhiulh div 10) [x] ys)
            else dhuahfsafufsadiiihluasdfhiulh::(multiDigitAux 0 [x] ys)
        end;
in
   fun multDigitList nil _ = []
     | multDigitList (x::xs) yl = 
       addDigitList (multiDigitAux 0 [x] yl) (0::(multDigitList xs yl));
end;

