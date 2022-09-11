datatype ('r, 'e) result =
    Ok of 'r
  | Error of 'e

type number = {
  integer: int,
  fraction: int,
  precision: int,
  exponent: int
  }

datatype json =
    Null
  | Bool of bool
  | Number of number
  | String of string
  | Array of json list
  | Object of dict
withtype dict = (string * json) list

datatype error =
          EmptyString
        | CharMismatch of char
        | HexCharExpected
        | NullExpected
        | TrueExpected
        | FalseExpected
        | ExponentRequired
        | NumberExpected
        | UnrecognisedEscapeSequence of char
        | InvalidValue
        | OutOfBounds
        | Garbage

fun describe EmptyString = "Empty string parsed"
  | describe (CharMismatch c) = "Expected " ^ str(c)
  | describe HexCharExpected = "Expected hex char"
  | describe NullExpected = "Expected null"
  | describe TrueExpected = "Expected true"
  | describe FalseExpected = "Expected false"
  | describe ExponentRequired = "Exponent required"
  | describe NumberExpected = "Number expected"
  | describe (UnrecognisedEscapeSequence c) = "Unrecognised escape sequence \\" ^ str(c)
  | describe InvalidValue = "Invalid value"
  | describe OutOfBounds = "Out of bounds read attempt"
  | describe Garbage = "garbage found at the end of string"

fun is_ws #" " = true
  | is_ws #"\t" = true
  | is_ws #"\n" = true
  | is_ws #"\r" = true
  | is_ws _ = false

infix >>=
fun op>>= (r, f) =
  case r of
       Ok ok => f ok
     | Error e => Error e

infix >>!
fun op>>! (r, f) =
  case r of
       Ok ok => Ok ok
     | Error e => f e

val hexdigits = (Vector.fromList o explode) "0123456789abcdef"

fun is_hex ch =
  let
    val ch = Char.toLower ch
  in
    isSome (Vector.find (fn x => x = ch) hexdigits)
  end

fun strlen str =
  let
    val len = size str
    fun proc 0 = 0
      | proc idx =
        if is_ws (String.sub (str, idx - 1)) then proc (idx - 1)
        else idx
  in
    proc len
  end

fun s2i str = getOpt (Int.fromString str, 0)

fun parse1 (str, strlen) =
  let
    fun peek idx = String.sub (str, idx)
    fun take (f, idx) =
      let
        fun proc last =
          if last < strlen andalso f (peek last) then proc (last + 1)
          else last
        val last = proc idx
        val count = last - idx
      in
        if count = 0 then Error (EmptyString, idx)
        else Ok (String.substring (str, idx, count), last)
      end
    fun ask (f, idx) =
      let
        fun proc (acc, last) =
          if not (last < strlen) then Ok (implode (rev acc), last)
          else case f last of
                    Ok (SOME ch, idx) => proc ((ch :: acc), idx)
                  | Ok (NONE, _) => Ok (implode (rev acc), last)
                  | Error e => Error e
      in
        proc ([], idx)
      end
    fun skip (f, idx) =
      if idx < strlen andalso f (peek idx) then skip (f, (idx + 1))
      else idx
    fun skip_ws idx = skip (is_ws, idx)
    fun chr (ch, idx) =
      if idx < strlen andalso peek idx = ch then Ok (ch, idx + 1)
      else Error (CharMismatch ch, idx)
    fun hex idx =
      if idx < strlen andalso is_hex (peek idx) then Ok ((peek idx), idx + 1)
      else Error (HexCharExpected, idx)
    fun hexword idx =
      hex idx >>=
      (fn (_, idx) => hex idx) >>=
      (fn (_, idx) => hex idx) >>=
      (fn (_, idx) => hex idx) >>=
      (fn (_, idx) => Ok (Char.chr 0xBF, idx))  (* we don't support unicode, just return inverted question mark *)
    fun parse_null idx =
      case take (Char.isAlpha, idx) of
           Ok ("null", idx') => Ok (Null, idx')
         | _ => Error (NullExpected, idx)
    fun parse_true idx =
      case take (Char.isAlpha, idx) of
           Ok ("true", idx') => Ok (Bool true, idx')
         | _ => Error (TrueExpected, idx)
    fun parse_false idx =
      case take (Char.isAlpha, idx) of
           Ok ("false", idx') => Ok (Bool false, idx')
         | _ => Error (FalseExpected, idx)
    fun intify ((num, frac, exp), idx) =
      ((s2i num, s2i frac, size frac, s2i exp), idx)
    fun make_number ((num, frac, prec, exp), idx) =
      Ok (Number {
        integer = num,
        fraction = frac,
        precision = prec,
        exponent = exp}, idx)
    fun make_neg_number ((num, frac, prec, exp), idx) =
      make_number ((~ num, frac, prec, exp), idx)
    fun parse_number_parts idx =
      let
        fun parse_fraction (num, idx) =
          case take (Char.isDigit, idx) of
               Ok (frac, idx') => Ok ((num, frac), idx')
             | _ => Ok ((num, ""), idx)
        fun parse_exponent ((num, frac), idx) =
            case take (Char.isDigit, idx) of
                 Ok (exp, idx') => Ok ((num, frac, exp), idx')
               | Error (e, _) => Error (ExponentRequired, idx)
      in
        take (Char.isDigit, idx) >>!
        (fn (_, idx) => Error (NumberExpected, idx)) >>=
        (fn (num, idx) =>
          case chr (#".", idx) of
              Ok (_, idx') => parse_fraction (num, idx')
            | Error _ => Ok ((num, ""), idx)) >>=
        (fn ((num, frac), idx) =>
          case chr (#"e", idx) of
              Ok (_, idx') => parse_exponent ((num, frac), idx')
            | Error _ => Ok ((num, frac, ""), idx))
      end
    fun parse_unsigned idx =
      parse_number_parts idx >>=
      (make_number o intify)
    fun parse_neg_integer idx =
      chr (#"-", idx) >>=
      (fn (_, idx) => parse_number_parts idx) >>=
      (make_neg_number o intify)
    fun string_char idx =
      case (peek idx) of
           #"\\" =>
            (case peek (idx + 1) of
                  #"\"" => Ok (SOME #"\"", idx + 2)
                | #"\\" => Ok (SOME #"\\", idx + 2)
                | #"/" => Ok (SOME #"/", idx + 2)
                | #"b" => Ok (SOME #"\b", idx + 2)
                | #"f" => Ok (SOME #"\f", idx + 2)
                | #"n" => Ok (SOME #"\n", idx + 2)
                | #"r" => Ok (SOME #"\r", idx + 2)
                | #"t" => Ok (SOME #"\t", idx + 2)
                | #"u" => hexword (idx + 2) >>= (fn (ch, idx) => Ok (SOME ch, idx))
                | c => Error (UnrecognisedEscapeSequence c, idx))
         | #"\"" => Ok (NONE, idx + 1)
         | c => Ok (SOME c, idx + 1)
    fun parse_string_raw idx =
      chr (#"\"", idx) >>=
      (fn (_, idx) => ask (string_char, idx)) >>=
      (fn (sub, idx) => chr (#"\"", idx) >>=
        (fn (_, idx) => Ok (sub, idx)))
    fun parse_string idx =
      parse_string_raw idx >>=
      (fn (str, idx) => Ok (String str, idx))
    fun parse_array_rest (acc, idx) =
      case chr (#",", (skip_ws idx)) of
           Ok (_, idx) =>
             parse_value idx >>=
             (fn (next, idx) => parse_array_rest ((next :: acc), idx))
         | Error (e, idx) => Ok (acc, idx)
    and parse_array idx =
      chr (#"[", idx) >>=
      (fn (_, idx) =>
        case parse_value idx of
             Ok (item, idx') => parse_array_rest ([item], idx')
           | Error (_, idx') => Ok ([], idx')
      ) >>=
      (fn (items, idx) => chr (#"]", (skip_ws idx)) >>=
        (fn (_, idx) => Ok (Array (rev items), idx))
      )
    and parse_object_item idx =
      parse_string_raw idx >>=
      (fn (key, idx) => chr (#":", (skip_ws idx)) >>=
        (fn (_, idx) =>
          parse_value idx >>=
          (fn (value, idx) => Ok ((key, value), idx))))
    and parse_object_rest (acc, idx) =
      case chr (#",", (skip_ws idx)) of
           Ok (_, idx) =>
             parse_object_item (skip_ws idx) >>=
             (fn ((key, value), idx) => parse_object_rest (((key, value) :: acc), idx))
         | Error (_, idx) => Ok (rev acc, idx)
    and parse_object idx =
      chr (#"{", idx) >>=
      (fn (_, idx) =>
        if peek (skip_ws idx) = #"\"" then parse_object_item (skip_ws idx) >>=
          (fn ((key, value), idx) => parse_object_rest ([(key, value)], idx))
        else Ok ([], idx)
      ) >>=
      (fn (items, idx) => chr (#"}", (skip_ws idx)) >>=
        (fn (_, idx) => Ok (Object items, idx)))
    and parse_value idx =
      let
        val idx = skip_ws idx
      in
        if idx < strlen then
          case peek idx of
               #"n" => parse_null idx
             | #"t" => parse_true idx
             | #"f" => parse_false idx
             | #"0" => parse_unsigned idx
             | #"1" => parse_unsigned idx
             | #"2" => parse_unsigned idx
             | #"3" => parse_unsigned idx
             | #"4" => parse_unsigned idx
             | #"5" => parse_unsigned idx
             | #"6" => parse_unsigned idx
             | #"7" => parse_unsigned idx
             | #"8" => parse_unsigned idx
             | #"9" => parse_unsigned idx
             | #"-" => parse_neg_integer idx
             | #"\"" => parse_string idx
             | #"[" => parse_array idx
             | #"{" => parse_object idx
             | _ => Error (InvalidValue, idx)
        else Error (OutOfBounds, idx)
      end
  in
    parse_value 0 >>=
    (fn (v, idx) =>
      if idx = strlen then Ok v
      else Error (Garbage, idx))
  end

fun parse str =
  let
    val strlen = strlen str
  in
    if strlen > 0 then parse1 (str, strlen)
    else Error (EmptyString, 0)
  end

fun escape ch =
  if ch = #"\"" then "\\\""
  else str ch

fun print_number {integer, fraction, precision, exponent} =
  (print (Int.toString integer);
  if fraction <> 0 then
    (print ".";
    print (Int.toString fraction))
  else ();
  if exponent <> 0 then
    (print "e";
    print (Int.toString exponent))
  else ())

fun print_string str =
  (print "\"";
  print (String.translate escape str);
  print "\"")

fun print_array [] =
    print "[]"
  | print_array [item] =
    (print "[";
    print_json item;
    print "]")
  | print_array (item :: tl) =
    (print "[";
    print_json item;
    print_array_items tl;
    print "]")
and print_array_items [] = ()
  | print_array_items (hd :: tl) =
    (print ", ";
    print_json hd;
    print_array_items tl)
and print_object [] =
    print "{}"
  | print_object [item] =
    (print "{";
    print_object_item item;
    print "}")
  | print_object (item :: tl) =
    (print "{";
    print_object_item item;
    print_object_items tl;
    print "}")
and print_object_item (key, value) =
    (print_string key;
    print ": ";
    print_json value)
and print_object_items [] = ()
  | print_object_items (item :: tl) =
    (print ", ";
    print_object_item item;
    print_object_items tl)
and print_json Null = print "null"
  | print_json (Bool false) = print "false"
  | print_json (Bool true) = print "true"
  | print_json (Number n) = print_number n
  | print_json (String s) = print_string s
  | print_json (Array a) = print_array a
  | print_json (Object obj) = print_object obj

fun main () = ()
