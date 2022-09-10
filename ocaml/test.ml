let parse = Json.parse

let fails f arg err =
        let r = f arg in
        Result.is_error r && fst @@ Result.get_error r = err

let ok f arg json =
        match f arg with
        | Ok result -> result = json
        | Error _ -> false

let () =
        assert (Json.length "foo" = 3);
        assert (Json.length " x " = 2);
        assert (Json.length "s" = 1);
        assert (Json.length " " = 0);
        assert (Json.length "" = 0);

        assert (fails parse "" EmptyString);
        assert (fails parse "x" InvalidValue);

        (* literals *)
        assert (ok parse "null" Null);
        assert (ok parse "true" (Bool true));
        assert (ok parse "false" (Bool false));
        assert (fails parse "truefalse" BoolExpected);

        (* numbers *)
        assert (ok parse "42" (Number {integer = 42; fraction = 0; precision = 0; exponent = 0}));
        assert (ok parse "0" (Number {integer = 0; fraction = 0; precision = 0; exponent = 0}));
        assert (ok parse "-1" (Number {integer = -1; fraction = 0; precision = 0; exponent = 0}));
        assert (ok parse "1.23" (Number {integer = 1; fraction = 23; precision = 2; exponent = 0}));
        assert (ok parse "1.230" (Number {integer = 1; fraction = 230; precision = 3; exponent = 0}));
        assert (ok parse "1." (Number {integer = 1; fraction = 0; precision = 0; exponent = 0}));
        assert (ok parse "0." (Number {integer = 0; fraction = 0; precision = 0; exponent = 0}));
        assert (ok parse "-0." (Number {integer = 0; fraction = 0; precision = 0; exponent = 0}));
        assert (ok parse "6.999e3" (Number {integer = 6; fraction = 999; precision = 3; exponent = 3}));
        assert (ok parse "-1.2e9" (Number {integer = -1; fraction = 2; precision = 1; exponent = 9}));
        assert (fails parse "6.999e" ExponentRequired);
        assert (ok parse "1.e1" (Number {integer = 1; fraction = 0; precision = 0; exponent = 1}));
        assert (fails parse "1.x" Garbage);
        assert (fails parse "-1.y" Garbage);

        (* strings *)
        assert (ok parse {|""|} (String ""));
        assert (fails parse {|"|} InvalidStringEnd);
        assert (ok parse {|"foobar"|} (String "foobar"));
        assert (ok parse {|"a\nb"|} (String "a\nb"));
        assert (ok parse {|"foo\\bar"|} (String {|foo\bar|}));
        assert (ok parse {|"foo bar"|} (String "foo bar"));
        assert (ok parse {|"foo/bar"|} (String "foo/bar"));
        assert (fails parse {|"foobar|} InvalidStringEnd);
        assert (fails parse {|"foo"bar|} Garbage);
        assert (fails parse {|"foo\"bar|} InvalidStringEnd);
        assert (ok parse {|"a b c"|} (String "a b c"));
        assert (ok parse {|" a b c "|} (String " a b c "));
        assert (ok parse {|"foo\"bar"|} (String {|foo"bar|}));
        assert (ok parse {|"\u1234"|} (String "\xBF"));
        assert (ok parse {|"\u1234\uabcd"|} (String "\xBF\xBF"));
        assert (ok parse {|"\u1234\uabcd\u00Ff"|} (String "\xBF\xBF\xBF"));
        assert (ok parse {|"foo\u12cdbar"|} (String "foo\xBFbar"));

        (* arrays *)
        assert (ok parse "[]" (Array [ ]));
        assert (ok parse "[null]" (Array [ Null ]));
        assert (ok parse "[[null]]" (Array [ Array [ Null ] ]));
        assert (ok parse "[true]" (Array [ (Bool true) ]));
        assert (ok parse "[false]" (Array [ (Bool false) ]));
        assert (ok parse "[true, false]" (Array [Bool true; Bool false]));
        assert (ok parse "[1.2]" (Array [ (Number {integer = 1; fraction = 2; precision = 1; exponent = 0}) ]));
        assert (ok parse {|["abc"]|} (Array [ (String "abc") ]));
        assert (ok parse "[[[]]]" (Array [ Array [ Array [ ] ] ]));
        assert (ok parse "[[[\"a\"]]]" (Array [ Array [ Array [ String "a" ] ] ]));
        assert (fails parse "[" (CharMismatch ']'));
        assert (fails parse "]" InvalidValue);
        assert (fails parse "[[[" (CharMismatch ']'));
        assert (fails parse "]]]" InvalidValue);
        assert (ok parse "[1,2,3]" (Array [
                Number {integer = 1; fraction = 0; precision = 0; exponent = 0};
                Number {integer = 2; fraction = 0; precision = 0; exponent = 0};
                Number {integer = 3; fraction = 0; precision = 0; exponent = 0};
        ]));
        assert (fails parse {|["|} (CharMismatch ']'));
        assert (ok parse "[true,false,null,0]" (Array [
                (Bool true); (Bool false); Null;
                Number {integer = 0; fraction = 0; precision = 0; exponent = 0}
        ]));
        assert (ok parse "[[],[]]" (Array [ Array [ ]; Array [ ] ]));
        assert (fails parse "[1," (InvalidArrayElement OutOfBounds));
        assert (fails parse "[1,]" (InvalidArrayElement InvalidValue));
        assert (fails parse "[1,2,]" (InvalidArrayElement InvalidValue));
        assert (fails parse "[1,2," (InvalidArrayElement OutOfBounds));

        (* objects *)
        assert (ok parse "{}" (Object [ ]));
        assert (ok parse {|{"1":1}|} (Object [ ("1", Number {integer = 1; fraction = 0; precision = 0; exponent = 0}) ]));
        assert (ok parse {|{"foo":"bar"}|} (Object [ ("foo", String "bar") ]));
        assert (ok parse {|{"":""}|} (Object [ ("", String "") ]));
        assert (ok parse {|{"12":[]}|} (Object [ ("12", Array [ ] ) ]));
        assert (ok parse {|{"a":1,"b":2,"c":3}|} (Object [
                ("a", Number {integer = 1; fraction = 0; precision = 0; exponent = 0} );
                ("b", Number {integer = 2; fraction = 0; precision = 0; exponent = 0} );
                ("c", Number {integer = 3; fraction = 0; precision = 0; exponent = 0} );
        ]));
        assert (ok parse {|{"x":9.8e7}|} (Object [ ("x", Number {integer = 9; fraction = 8; precision = 1; exponent = 7} ) ]));
        assert (fails parse {|{"1":1|} (CharMismatch '}'));
        assert (fails parse {|{"foo"}|} (CharMismatch ':'));
        assert (fails parse {|{"foo":}|} (InvalidObjectElement InvalidValue));

        (* values with spaces *)
        assert (fails parse "   " EmptyString);
        assert (fails parse " [  " (CharMismatch ']'));
        assert (fails parse " ]  " InvalidValue);
        assert (ok parse "   null   " Null);
        assert (ok parse "   true   " (Bool true));
        assert (ok parse "  false  " (Bool false));
        assert (ok parse {|" \u1234 \uabcd \u00Ff "|} (String " \xBF \xBF \xBF "));
        assert (ok parse " [ true, false, null ] " (Array [ (Bool true); (Bool false); Null ]));
        assert (ok parse " [ true , false , null ] " (Array [ (Bool true); (Bool false); Null ]));
        assert (ok parse {| { "a" : true , "b" : false , "c" : null } |} (Object [ ("a", Bool true); ("b", Bool false); ("c", Null) ] ));
        assert (ok parse {| {  } |} (Object [ ] ));
        assert (ok parse {| [  ] |} (Array [ ] ));
        ()
