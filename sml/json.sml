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

fun is_ws #" " = true
  | is_ws #"\t" = true
  | is_ws #"\n" = true
  | is_ws _ = false

infix >>=
fun op>>= (r, f) =
  case r of
       Ok ok => f ok
     | Error e => Error e

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

fun parse1 str strlen =
  let
    fun peek idx = String.sub (str, idx)
    fun take f idx =
      let
        fun proc last =
          if last < strlen andalso f (peek last) then proc (last + 1)
          else last
        val last = proc idx
        val len = last - idx
      in
        if len = 0 then Error ("Empty string parsed", idx)
        else Ok (String.substring (str, idx, len), last)
      end
    fun ask2 f idx =
      let
        fun proc idx' prev =
          if idx' < strlen andalso f (peek idx') prev then proc (idx' + 1) (peek idx')
          else idx'
          (*
           * Accessing idx - 1 should always be safe, because we always
           * match on first char to decide which parse_* function and we
           * ever call ask on the following chars
           *)
        val last = proc idx (peek (idx - 1))
        val len = last - idx
        val sub = String.substring (str, idx, len)
      in
        Ok (sub, last)
      end
    fun skip f idx =
      if idx < strlen andalso f (peek idx) then skip f (idx + 1)
      else idx
    fun skip_ws idx = skip is_ws idx
    fun chr ch idx =
      if idx < strlen andalso peek idx = ch then Ok (ch, idx + 1)
      else Error ("Expectetd " ^ Char.toString ch, idx)
    fun parse_null idx =
      case take Char.isAlpha idx of
           Ok ("null", idx') => Ok (Null, idx')
         | _ => Error ("Expected null", idx)
    fun parse_true idx =
      case take Char.isAlpha idx of
           Ok ("true", idx') => Ok (Bool true, idx')
         | _ => Error ("Expected true", idx)
    fun parse_false idx =
      case take Char.isAlpha idx of
           Ok ("false", idx') => Ok (Bool false, idx')
         | _ => Error ("Expected false", idx)
    fun make_int ((num, frac, exp), idx) =
      Ok (Number {
        integer=getOpt (Int.fromString num, 0),
        fraction=getOpt (Int.fromString frac, 0),
        precision=size frac,
        exponent=getOpt (Int.fromString exp, 0)}, idx)
    fun parse_unsigned idx =
      let
        fun parse_integer idx =
          case take Char.isDigit idx of
               Ok (num, idx') => Ok (num, idx')
             | _ => Error ("Expected number", idx)
        fun parse_fraction (num, idx) =
          case take Char.isDigit idx of
               Ok (frac, idx') => Ok ((num, frac), idx')
             | _ => Ok ((num, ""), idx)
        fun parse_dot_fraction (num, idx) =
          case chr #"." idx of
               Ok (_, idx') => parse_fraction (num, idx')
             | Error (e, _) => Ok ((num, ""), idx)
        fun parse_exponent ((num, frac), idx) =
            case take Char.isDigit idx of
                 Ok (exp, idx') => Ok ((num, frac, exp), idx')
               | Error (e, _) => Error ("Expected exponent", idx)
        fun parse_e_exponent ((num, frac), idx) =
          case chr #"e" idx of
               Ok (_, idx') => parse_exponent ((num, frac), idx')
             | _ => Ok ((num, frac, ""), idx)
      in
        parse_integer idx >>=
        parse_dot_fraction >>=
        parse_e_exponent >>=
        make_int
      end
    fun parse_neg_integer idx =
      let
        fun parse_integer idx =
          case parse_unsigned idx of
               Ok (Number n, idx') => Ok (Number {
                 integer = ~ (#integer n),
                 fraction = #fraction n,
                 precision = #precision n,
                 exponent = #exponent n}, idx')
             | Error e => Error e
        fun parse_minus_integer idx =
          case chr #"-" idx of
               Ok (_, idx') => parse_integer idx'
             | Error e => Error e
      in
        parse_minus_integer idx
      end
    fun ending_quote ch prev = ch = #"\"" andalso prev <> #"\\"
    fun non_ending_quote ch prev = not (ending_quote ch prev)
    fun parse_string idx =
      chr #"\"" idx >>=
      (fn (_, idx) => ask2 non_ending_quote idx) >>=
      (fn (sub, idx) => chr #"\"" idx >>= (fn (_, idx) => Ok (sub, idx))) >>=
      (fn (sub, idx) => Ok (String sub, idx))
    fun parse_array_rest acc idx =
      case chr #"," idx of
           Ok (_, idx) =>
             parse_value idx >>=
             (fn (next, idx) => parse_array_rest (next :: acc) idx)
         | Error (e, idx) => Ok (acc, idx)
    and parse_array idx =
      chr #"[" idx >>=
      (fn (_, idx) =>
        case parse_value idx of
             Ok (item, idx') => parse_array_rest [item] idx'
           | Error (_, idx') => Ok ([], idx')
      ) >>=
      (fn (items, idx) => (chr #"]" idx) >>= (fn (_, idx) => Ok (items, idx))) >>=
      (fn (_, idx) => Ok (Array [], idx))
    and parse_object_item idx =
      parse_string idx >>=
      (fn (String key, idx) => (chr #":" idx) >>= (fn (_, idx) => Ok (key, idx))) >>=
      (fn (key, idx) =>
        case parse_value idx of
             Ok (value, idx) => Ok ((key, value), idx)
           | Error e => Error e
      )
    and parse_object_rest acc idx =
      case chr #"," idx of
           Ok (_, idx) =>
             parse_object_item idx >>=
             (fn ((key, value), idx) => parse_object_rest ((key, value) :: acc) idx)
         | Error (_, idx) => Ok (rev acc, idx)
    and parse_object idx =
      chr #"{" idx >>=
      (fn (_, idx) =>
        if peek idx = #"\"" then parse_object_item idx >>=
          (fn ((key, value), idx) => parse_object_rest [(key, value)] idx)
        else Ok ([], idx)
      ) >>=
      (fn (items, idx) => (chr #"}" idx) >>= (fn (_, idx) => Ok (items, idx))) >>=
      (fn (items, idx) => Ok (Object items, idx))
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
             | _ => Error ("Invalid value", idx)
        else Error ("Out of bounds read attempt", idx)
      end
  in
    case parse_value 0 of
         e as Error _ => e
       | Ok (v, idx) =>
           if idx = strlen then Ok (v, idx)
           else Error ("garbage found at the end of string", idx)
  end

fun parse str =
  let
    val strlen = strlen str
  in
    if strlen > 0 then parse1 str strlen
    else Error ("Empty string", 0)
  end

fun main () = ()
