val result = ref OS.Process.success

(* Input string was parsed, but the result was not what was expected *)
fun incorrect_result value expected actual = (
  result := OS.Process.failure;
  print "Failed for \"";
  print value;
  print "\":\n\texpected json: ";
  print_json expected;
  print "\n\tactual json:   ";
  print_json actual;
  print "\n")

(* Input string failed to parse and the returned error was not what was expected *)
fun incorrect_error value expected actual = (
  result := OS.Process.failure;
  print "Failed for \"";
  print value;
  print "\":\n\texpected error: ";
  print (describe expected);
  print "\n\tactual error:   ";
  print (describe actual);
  print "\n")

(* Input string was parsed, but shouldn't *)
fun incorrect_parse value expected actual = (
  result := OS.Process.failure;
  print "Failed for \"";
  print value;
  print "\":\n\texpected error: ";
  print (describe expected);
  print "\n\tactual json:  ";
  print_json actual;
  print "\n")

(* Input string failed to parse, but should have *)
fun failed_to_parse value expected actual = (
  result := OS.Process.failure;
  print "Failed for \"";
  print value;
  print "\":\n\texpected json:  ";
  print_json expected;
  print "\n\tactual error: ";
  print (describe actual);
  print "\n")

fun success value expected actual = ()

fun check b success fail =
  if b then success
  else fail

fun check_ok value expected actual =
  check (actual = expected) success incorrect_result value expected actual

fun check_err value expected actual =
  check (actual = expected) success incorrect_error value expected actual

fun fails f arg expected =
  let
    val actual = f arg
  in
    case actual of
         Ok actual_json => incorrect_parse arg expected actual_json
       | Error (actual_error, _) => check_err arg expected actual_error
  end

fun ok f arg expected =
  let
    val actual = f arg
  in
    case actual of
         Ok actual_json => check_ok arg expected actual_json
       | Error (actual_error, _) => failed_to_parse arg expected actual_error
  end

fun eq f arg expected =
  check (f arg = expected) arg

val _ =
  eq strlen "foo" 3;
  eq strlen " x " 2;
  eq strlen "s" 1;
  eq strlen " " 0;
  eq strlen "" 0;

  fails parse "" EmptyString;
  fails parse "x" InvalidValue;

  (* literals *)
  ok parse "null" Null;
  ok parse "true" (Bool true);
  ok parse "false" (Bool false);
  fails parse "truefalse" TrueExpected;

  (* numbers *)
  ok parse "42" (Number {integer = 42, fraction = 0, precision = 0, exponent = 0});
  ok parse "0" (Number {integer = 0, fraction = 0, precision = 0, exponent = 0});
  ok parse "-1" (Number {integer = ~1, fraction = 0, precision = 0, exponent = 0});
  ok parse "1.23" (Number {integer = 1, fraction = 23, precision = 2, exponent = 0});
  ok parse "1.230" (Number {integer = 1, fraction = 230, precision = 3, exponent = 0});
  ok parse "1." (Number {integer = 1, fraction = 0, precision = 0, exponent = 0});
  ok parse "0." (Number {integer = 0, fraction = 0, precision = 0, exponent = 0});
  ok parse "-0." (Number {integer = 0, fraction = 0, precision = 0, exponent = 0});
  ok parse "6.999e3" (Number {integer = 6, fraction = 999, precision = 3, exponent = 3});
  ok parse "-1.2e9" (Number {integer = ~1, fraction = 2, precision = 1, exponent = 9});
  fails parse "6.999e" ExponentRequired;
  ok parse "1.e1" (Number {integer = 1, fraction = 0, precision = 0, exponent = 1});
  fails parse "1.x" Garbage;
  fails parse "-1.y" Garbage;
  fails parse ".12" InvalidValue;
  fails parse "-.12" EmptyString;

  (* strings *)
  ok parse "\"\"" (String "");
  fails parse "\"" (CharMismatch #"\"");
  ok parse "\"foobar\"" (String "foobar");
  ok parse "\"a\\nb\"" (String "a\nb");
  ok parse "\"foo\\\\bar\"" (String "foo\\bar");
  ok parse "\"foo bar\"" (String "foo bar");
  ok parse "\"foo/bar\"" (String "foo/bar");
  fails parse "\"foobar" (CharMismatch #"\"");
  fails parse "\"foo\"bar" Garbage;
  fails parse "\"foo\\\"bar" (CharMismatch #"\"");
  ok parse "\"a b c\"" (String "a b c");
  ok parse "\" a b c \"" (String " a b c ");
  ok parse "\"foo\\\"bar\"" (String "foo\"bar");
  ok parse "\"\\u1234\"" (String "?");
  ok parse "\"\\u1234\\uabcd\"" (String "??");
  ok parse "\"\\u1234\\uabcd\\u00Ff\"" (String "???");
  ok parse "\"foo\\u12cdbar\"" (String "foo?bar");
  fails parse "\"\\u12cx\"" (HexCharExpected);
  fails parse "\"\\" OutOfBounds;

  (* arrays *)
  ok parse "[]" (Array []);
  ok parse "[null]" (Array [Null]);
  ok parse "[[null]]" (Array [Array [Null]]);
  ok parse "[true]" (Array [Bool true]);
  ok parse "[false]" (Array [Bool false]);
  ok parse "[true,false]" (Array [Bool true, Bool false]);
  ok parse "[1.2]" (Array [Number {integer = 1, fraction = 2, precision = 1, exponent = 0}]);
  ok parse "[\"abc\"]" (Array [String "abc"]);
  ok parse "[[[]]]" (Array [Array [Array []]]);
  ok parse "[[[\"a\"]]]" (Array [Array [Array [String "a"]]]);
  fails parse "[" (CharMismatch #"]");
  fails parse "]" InvalidValue;
  fails parse "[[[" (CharMismatch #"]");
  fails parse "]]]" InvalidValue;
  ok parse "[1,2,3]" (Array [
    Number {integer = 1, fraction = 0, precision = 0, exponent = 0},
    Number {integer = 2, fraction = 0, precision = 0, exponent = 0},
    Number {integer = 3, fraction = 0, precision = 0, exponent = 0}
  ]);
  fails parse "[\"" (CharMismatch #"]");
  ok parse "[true,false,null,0]" (Array [
    Bool true, Bool false, Null,
    Number {integer = 0, fraction = 0, precision = 0, exponent = 0}
  ]);
  ok parse "[[],[]]" (Array [Array [], Array []]);
  fails parse "[1," OutOfBounds;
  fails parse "[1,]" InvalidValue;
  fails parse "[1,2,]" InvalidValue;
  fails parse "[1,2," OutOfBounds;

  (* objects *)
  ok parse "{}" (Object []);
  ok parse "{\"1\":1}" (Object [ ("1", Number {integer = 1, fraction = 0, precision = 0, exponent = 0}) ]);
  ok parse "{\"foo\":\"bar\"}" (Object [ ("foo", String "bar") ]);
  ok parse "{\"\":\"\"}" (Object [ ("", String "") ]);
  ok parse "{\"12\":[]}" (Object [ ("12", Array [ ] ) ]);
  ok parse "{\"a\":1,\"b\":2,\"c\":3}" (Object [
          ("a", Number {integer = 1, fraction = 0, precision = 0, exponent = 0} ),
          ("b", Number {integer = 2, fraction = 0, precision = 0, exponent = 0} ),
          ("c", Number {integer = 3, fraction = 0, precision = 0, exponent = 0} )
  ]);
  ok parse "{\"x\":9.8e7}" (Object [ ("x", Number {integer = 9, fraction = 8, precision = 1, exponent = 7} ) ]);
  fails parse "{\"1\":1" (CharMismatch #"}");
  fails parse "{\"foo\"}" (CharMismatch #":");
  fails parse "{\"foo\":}" InvalidValue;

  (* values with spaces *)
  fails parse "   " EmptyString;
  fails parse " [ " (CharMismatch #"]");
  fails parse " ] " InvalidValue;
  ok parse "   null   " Null;
  ok parse "   true   " (Bool true);
  ok parse "  false  " (Bool false);
  ok parse "\" \\u1234 \\uabcd \\u00Ff \"" (String " ? ? ? ");
  ok parse " [ true, false, null ] " (Array [Bool true, Bool false, Null]);
  ok parse " [ true , false , null ] " (Array [Bool true, Bool false, Null]);
  ok parse " { \"a\" : true , \"b\" : false , \"c\" : null } " (Object [("a", Bool true), ("b", Bool false), ("c", Null)]);
  ok parse " {  } " (Object [ ] );
  ok parse " [  ] " (Array [ ] );

  (ignore o OS.Process.exit) (!result);
