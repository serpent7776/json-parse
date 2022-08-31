(* open Json *)

exception Fail of string

fun fail arg = raise Fail arg

fun assert b arg =
  if b then ()
  else fail arg

fun fails f arg =
  case f arg of
       Ok _ => fail arg
     | Error e => ()

fun ok f arg json =
  case f arg of
       Ok (result, _) => assert (result = json) arg
     | Error e => fail arg

fun eq f arg expected =
  assert (f arg = expected) arg

val _ = (
  eq strlen "foo" 3;
  eq strlen " x " 2;
  eq strlen "s" 1;
  eq strlen " " 0;
  eq strlen "" 0;

  fails parse "";
  fails parse "x";

  (* literals *)
  ok parse "null" Null;
  ok parse "true" (Bool true);
  ok parse "false" (Bool false);
  fails parse "truefalse";

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
  fails parse "6.999e";
  ok parse "1.e1" (Number {integer = 1, fraction = 0, precision = 0, exponent = 1});
  fails parse "1.x";
  fails parse "-1.y";

  (* strings *)
  ok parse "\"\"" (String "");
  fails parse "\"";
  ok parse "\"foobar\"" (String "foobar");
  ok parse "\"a\\nb\"" (String "a\nb");
  ok parse "\"foo\\\\bar\"" (String "foo\\bar");
  ok parse "\"foo bar\"" (String "foo bar");
  ok parse "\"foo/bar\"" (String "foo/bar");
  fails parse "\"foobar";
  fails parse "\"foo\"bar";
  fails parse "\"foo\\\"bar";
  ok parse "\"a b c\"" (String "a b c");
  ok parse "\" a b c \"" (String " a b c ");
  ok parse "\"foo\\\"bar\"" (String "foo\"bar");

  (* arrays *)
  ok parse "[]" (Array []);
  ok parse "[null]" (Array [Null]);
  ok parse "[[null]]" (Array [Array [Null]]);
  ok parse "[true]" (Array [Bool true]);
  ok parse "[false]" (Array [Bool false]);
  ok parse "[true, false]" (Array [Bool true, Bool false]);
  ok parse "[1.2]" (Array [Number {integer = 1, fraction = 2, precision = 1, exponent = 0}]);
  ok parse "[\"abc\"]" (Array [String "abc"]);
  ok parse "[[[]]]" (Array [Array [Array []]]);
  ok parse "[[[\"a\"]]]" (Array [Array [Array [String "a"]]]);
  fails parse "[";
  fails parse "]";
  fails parse "[[[";
  fails parse "]]]";
  ok parse "[1, 2, 3]" (Array [
    Number {integer = 1, fraction = 0, precision = 0, exponent = 0},
    Number {integer = 2, fraction = 0, precision = 0, exponent = 0},
    Number {integer = 3, fraction = 0, precision = 0, exponent = 0}
  ]);
  fails parse "[\"";
  ok parse "[true,false,null,0]" (Array [
    Bool true, Bool false, Null,
    Number {integer = 0, fraction = 0, precision = 0, exponent = 0}
  ]);
  ok parse "[[],[]]" (Array [Array [], Array []]);
  fails parse "[1,";
  fails parse "[1,]";
  fails parse "[1,2,]";
  fails parse "[1,2,";

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
  fails parse "{\"1\":1";
  fails parse "{\"foo\"}";
  fails parse "{\"foo\":}";

  (* values with spaces *)
  fails parse "   ";
  fails parse " [ ";
  fails parse " ] ";
  ok parse "   null   " Null;
  ok parse "   true   " (Bool true);
  ok parse "  false  " (Bool false);
  ok parse " [ true, false, null ] " (Array [Bool true, Bool false, Null]);
  ok parse " [ true , false , null ] " (Array [Bool true, Bool false, Null]);
  ok parse " { \"a\" : true , \"b\" : false , \"c\" : null } " (Object [("a", Bool true), ("b", Bool false), ("c", Null)]);
  ok parse " {  } " (Object [ ] );
  ok parse " [  ] " (Array [ ] );

  ()
  )
  handle Fail e => (
    print "assertion failed for input: ";
    print e;
    print "\n"
  )
  | _ => (
    print "unhandled exception "
    (* print e; *)
    (* print "\n" *)
  )
