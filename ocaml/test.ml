let parse = Json.parse

(* Input string was parsed, but the result was not what was expected *)
let incorrect_result value expected actual =
        Printf.eprintf "Failed for \"%s\":\n\texpected json: %a\n\tactual json:   %a\n" value Json.print expected Json.print actual

(* Input string failed to parse and the returned error was not what was expected *)
let incorrect_error value expected actual =
        Printf.eprintf "Failed for \"%s\":\n\texpected error: %s\n\tactual error:   %s\n" value (Json.describe expected) (Json.describe actual)

(* Input string was parsed, but shouldn't *)
let incorrect_parse value expected actual =
        Printf.eprintf "Failed for \"%s\":\n\texpected error: %s\n\tactual json:  %a\n" value (Json.describe expected) Json.print actual

(* Input string failed to parse, but should have *)
let failed_to_parse value expected actual =
        result := 1;
        Printf.eprintf "Failed for \"%s\":\n\texpected json:  %a\n\tactual error: %s\n" value Json.print expected (Json.describe actual)

let success value expected actual =
        ()

let check b ok err =
        if b then ok
        else err

(* check that input string parses to expected json *)
let check_ok b = check b success incorrect_result

(* check that input string fails to parse with expected error *)
let check_err b = check b success incorrect_error

let fails f arg err =
        let actual = f arg in
        match actual with
        | Error (actual_error, _) ->
                check_err (actual_error = err) arg err actual_error
        | Ok actual_json ->
                incorrect_parse arg err actual_json

let ok f arg expected =
        let actual = f arg in
        match actual with
        | Ok actual_json -> check_ok (actual_json = expected) arg expected actual_json
        | Error (actual_error, _) -> failed_to_parse arg expected actual_error

let () =
        assert (Json.length "foo" = 3);
        assert (Json.length " x " = 2);
        assert (Json.length "s" = 1);
        assert (Json.length " " = 0);
        assert (Json.length "" = 0);

        fails parse "" EmptyString;
        fails parse "x" InvalidValue;

        (* literals *)
        ok parse "null" Null;
        ok parse "true" (Bool true);
        ok parse "false" (Bool false);
        fails parse "truefalse" BoolExpected;

        (* numbers *)
        ok parse "42" (Number {integer = 42; fraction = 0; precision = 0; exponent = 0});
        ok parse "0" (Number {integer = 0; fraction = 0; precision = 0; exponent = 0});
        ok parse "-1" (Number {integer = -1; fraction = 0; precision = 0; exponent = 0});
        ok parse "1.23" (Number {integer = 1; fraction = 23; precision = 2; exponent = 0});
        ok parse "1.230" (Number {integer = 1; fraction = 230; precision = 3; exponent = 0});
        ok parse "1." (Number {integer = 1; fraction = 0; precision = 0; exponent = 0});
        ok parse "0." (Number {integer = 0; fraction = 0; precision = 0; exponent = 0});
        ok parse "-0." (Number {integer = 0; fraction = 0; precision = 0; exponent = 0});
        ok parse "6.999e3" (Number {integer = 6; fraction = 999; precision = 3; exponent = 3});
        ok parse "-1.2e9" (Number {integer = -1; fraction = 2; precision = 1; exponent = 9});
        fails parse "6.999e" ExponentRequired;
        ok parse "1.e1" (Number {integer = 1; fraction = 0; precision = 0; exponent = 1});
        fails parse "1.x" Garbage;
        fails parse "-1.y" Garbage;

        (* strings *)
        ok parse {|""|} (String "");
        fails parse {|"|} InvalidStringEnd;
        ok parse {|"foobar"|} (String "foobar");
        ok parse {|"a\nb"|} (String "a\nb");
        ok parse {|"foo\\bar"|} (String {|foo\bar|});
        ok parse {|"foo bar"|} (String "foo bar");
        ok parse {|"foo/bar"|} (String "foo/bar");
        fails parse {|"foobar|} InvalidStringEnd;
        fails parse {|"foo"bar|} Garbage;
        fails parse {|"foo\"bar|} InvalidStringEnd;
        ok parse {|"a b c"|} (String "a b c");
        ok parse {|" a b c "|} (String " a b c ");
        ok parse {|"foo\"bar"|} (String {|foo"bar|});
        ok parse {|"\u1234"|} (String "\xBF");
        ok parse {|"\u1234\uabcd"|} (String "\xBF\xBF");
        ok parse {|"\u1234\uabcd\u00Ff"|} (String "\xBF\xBF\xBF");
        ok parse {|"foo\u12cdbar"|} (String "foo\xBFbar");

        (* arrays *)
        ok parse "[]" (Array [ ]);
        ok parse "[null]" (Array [ Null ]);
        ok parse "[[null]]" (Array [ Array [ Null ] ]);
        ok parse "[true]" (Array [ (Bool true) ]);
        ok parse "[false]" (Array [ (Bool false) ]);
        ok parse "[true, false]" (Array [Bool true; Bool false]);
        ok parse "[1.2]" (Array [ (Number {integer = 1; fraction = 2; precision = 1; exponent = 0}) ]);
        ok parse {|["abc"]|} (Array [ (String "abc") ]);
        ok parse "[[[]]]" (Array [ Array [ Array [ ] ] ]);
        ok parse "[[[\"a\"]]]" (Array [ Array [ Array [ String "a" ] ] ]);
        fails parse "[" (CharMismatch ']');
        fails parse "]" InvalidValue;
        fails parse "[[[" (CharMismatch ']');
        fails parse "]]]" InvalidValue;
        ok parse "[1,2,3]" (Array [
                Number {integer = 1; fraction = 0; precision = 0; exponent = 0};
                Number {integer = 2; fraction = 0; precision = 0; exponent = 0};
                Number {integer = 3; fraction = 0; precision = 0; exponent = 0};
        ]);
        fails parse {|["|} (CharMismatch ']');
        ok parse "[true,false,null,0]" (Array [
                (Bool true); (Bool false); Null;
                Number {integer = 0; fraction = 0; precision = 0; exponent = 0}
        ]);
        ok parse "[[],[]]" (Array [ Array [ ]; Array [ ] ]);
        fails parse "[1," (InvalidArrayElement OutOfBounds);
        fails parse "[1,]" (InvalidArrayElement InvalidValue);
        fails parse "[1,2,]" (InvalidArrayElement InvalidValue);
        fails parse "[1,2," (InvalidArrayElement OutOfBounds);

        (* objects *)
        ok parse "{}" (Object [ ]);
        ok parse {|{"1":1}|} (Object [ ("1", Number {integer = 1; fraction = 0; precision = 0; exponent = 0}) ]);
        ok parse {|{"foo":"bar"}|} (Object [ ("foo", String "bar") ]);
        ok parse {|{"":""}|} (Object [ ("", String "") ]);
        ok parse {|{"12":[]}|} (Object [ ("12", Array [ ] ) ]);
        ok parse {|{"a":1,"b":2,"c":3}|} (Object [
                ("a", Number {integer = 1; fraction = 0; precision = 0; exponent = 0} );
                ("b", Number {integer = 2; fraction = 0; precision = 0; exponent = 0} );
                ("c", Number {integer = 3; fraction = 0; precision = 0; exponent = 0} );
        ]);
        ok parse {|{"x":9.8e7}|} (Object [ ("x", Number {integer = 9; fraction = 8; precision = 1; exponent = 7} ) ]);
        fails parse {|{"1":1|} (CharMismatch '}');
        fails parse {|{"foo"}|} (CharMismatch ':');
        fails parse {|{"foo":}|} (InvalidObjectElement InvalidValue);

        (* values with spaces *)
        fails parse "   " EmptyString;
        fails parse " [  " (CharMismatch ']');
        fails parse " ]  " InvalidValue;
        ok parse "   null   " Null;
        ok parse "   true   " (Bool true);
        ok parse "  false  " (Bool false);
        ok parse {|" \u1234 \uabcd \u00Ff "|} (String " \xBF \xBF \xBF ");
        ok parse " [ true, false, null ] " (Array [ (Bool true); (Bool false); Null ]);
        ok parse " [ true , false , null ] " (Array [ (Bool true); (Bool false); Null ]);
        ok parse {| { "a" : true , "b" : false , "c" : null } |} (Object [ ("a", Bool true); ("b", Bool false); ("c", Null) ] );
        ok parse {| {  } |} (Object [ ] );
        ok parse {| [  ] |} (Array [ ] );
        ()
