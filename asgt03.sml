(*
 *  asgt03.sml
 *
 *  <YOUR NAME>
 *  <TODAY'S DATE>
 *
 *  <DESCRIPTIVE COMMENT>
 *
 *)


    (*
     * cs52int
     *
     * The type cs52int packages lists of "digits" between
     * 0 and radix-1 (see below for radix). The lists are
     * supposed to represent the absolute value of the 
     * number in base-radix, with the least significant
     * "digit" coming first and no trailing zeroes.
     *)
    datatype cs52int = Pos of int list
                     | Zero
                     | Neg of int list;

    (*
     * radix
     *
     * The radix is an even power of two with the property that
     * 3*radix is an ordinary SML int. The largest possible 
     * radix is used here.
     *
     * The square root of the radix is useful for multiplication.
     * The value radixAsCS52Int is used in some of the conversion
     * routines, below.
     *)
    fun powerOfTwo 0 = 1
      | powerOfTwo k =
        let
            val halfPower = powerOfTwo (k div 2)
        in
            halfPower * halfPower * (if k mod 2 = 0 then 1 else 2)
        end;

    val logRadix       = 28;
    val sqrtRadix      = powerOfTwo (logRadix div 2);
    val radix          = sqrtRadix * sqrtRadix;
    val radixAsCS52Int = Pos [0,1];


    (* Part 4: <DEFINE THE CONSTANTS zero, one, AND two HERE> *)


    (* Part 1: <INSERT YOUR LIST FUNCTIONS, INCLUDING normalize, HERE> *)


    (* Part 2: <WRITE CODE FOR isOdd, negate, sum, diff, prod, quo, rem 
                and compare HERE> *)


    (* Part 3: <WRITE CODE FOR THE SIMPLE ARITHMETIC FUNCTIONS HERE> *)


    (* Part 5: <WRITE CODE FOR THE CONVERSION FUNCTIONS HERE>
               fromString is provided for you.                *)

    fun fromString s =
        let
            val decimalRadix = fromInt 10;
            val digitList = explode s;
            val isNeg   = not (null digitList) andalso hd digitList = #"~";
            val digits  = if isNeg then tl digitList else digitList;
            fun fs acc nil        = SOME (if isNeg then negate acc else acc)
              | fs acc (#"0"::ds) =
                    fs (sum (prod acc decimalRadix) (fromInt 0)) ds
              | fs acc (#"1"::ds) =
                    fs (sum (prod acc decimalRadix) (fromInt 1)) ds
              | fs acc (#"2"::ds) =
                    fs (sum (prod acc decimalRadix) (fromInt 2)) ds
              | fs acc (#"3"::ds) =
                    fs (sum (prod acc decimalRadix) (fromInt 3)) ds
              | fs acc (#"4"::ds) =
                    fs (sum (prod acc decimalRadix) (fromInt 4)) ds
              | fs acc (#"5"::ds) =
                    fs (sum (prod acc decimalRadix) (fromInt 5)) ds
              | fs acc (#"6"::ds) =
                    fs (sum (prod acc decimalRadix) (fromInt 6)) ds
              | fs acc (#"7"::ds) =
                    fs (sum (prod acc decimalRadix) (fromInt 7)) ds
              | fs acc (#"8"::ds) =
                    fs (sum (prod acc decimalRadix) (fromInt 8)) ds
              | fs acc (#"9"::ds) =
                    fs (sum (prod acc decimalRadix) (fromInt 9)) ds
              | fs _ _ = NONE;
        in
            if null digits
               then NONE
               else fs Zero digits
        end;

