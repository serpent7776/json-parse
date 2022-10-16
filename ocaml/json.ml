type number = {
        integer: int;
        fraction: int;
        precision: int;
        exponent: int;
}

let make_int n = {
        integer = n;
        fraction = 0;
        precision = 0;
        exponent = 0;
}

let make_decimal i f p = {
        integer = i;
        fraction = f;
        precision = p;
        exponent = 0;
}

let make_number i f p e = {
        integer = i;
        fraction = f;
        precision = p;
        exponent = e;
}

type json =
        | Null
        | Bool of bool
        | Number of number
        | String of string
        | Array of json list
        | Object of dict
and dict = (string * json) list

type error =
        | EmptyString
        | CharMismatch of char
        | HexCharExpected
        | NullExpected
        | BoolExpected
        | ExponentRequired
        | UnrecognisedEscapeSequence of char
        | InvalidStringStart
        | InvalidStringEnd
        | InvalidArrayElement of error
        | InvalidObjectElement of error
        | Fatal
        | InvalidValue
        | OutOfBounds
        | Garbage

let sofc chr =
        String.make 1 chr

let rec describe = function
        | EmptyString -> "Empty string parsed"
        | CharMismatch c -> "Expected " ^ sofc(c)
        | HexCharExpected -> "Expected hex char"
        | NullExpected -> "Expected null"
        | BoolExpected -> "Expected bool"
        | ExponentRequired -> "Exponent required"
        | UnrecognisedEscapeSequence c -> "Unrecognised escape sequence \\" ^ sofc(c)
        | InvalidStringStart -> {|String must start with "|}
        | InvalidStringEnd -> {|String must end with "|}
        | InvalidArrayElement e -> "Failed to parse array element: " ^ (describe e)
        | InvalidObjectElement e -> "Failed to parse object element: " ^ (describe e)
        | Fatal -> "Should never happen"
        | InvalidValue -> "Invalid value"
        | OutOfBounds -> "Out of bounds read attempt"
        | Garbage -> "garbage found at the end of string"

let is_letter = function
        | 'a'..'z' -> true
        | 'A'..'Z' -> true
        | _ -> false

let is_digit = function
        | '0'..'9' -> true
        | _ -> false

let is_ws = function
        | ' ' -> true
        | '\t' -> true
        | '\n' -> true
        | '\r' -> true
        | _ -> false

let hexbytes = Bytes.of_string "0123456789abcdef"

let is_hex ch = Bytes.contains hexbytes (Char.lowercase_ascii ch)

let parse_hex_unsafe ch =
        Bytes.index hexbytes (Char.lowercase_ascii ch)

let implode_rev ls =
        let size = List.length ls in
        let bytes = Bytes.create size in
        List.iteri (fun idx ch -> Bytes.set bytes (size - idx - 1) ch) ls;
        Bytes.to_string bytes

(** length s returns length of the string s ignoring any trailing whitespaca *)
let length str =
        let strlen = String.length str in
        let rec proc idx =
                if idx >= 0 && is_ws str.[idx] then proc (idx - 1)
                else idx + 1
        in
        if strlen > 0 then proc (strlen - 1)
        else 0

let (let=) v f =
        match v with
        | Error _ as err -> err
        | Ok ok -> f ok

(** Parse non empty string into json *)
let parse1 str strlen =
        let peek idx = str.[idx]
        in
        let check idx =
                if idx < strlen then Ok ()
                else Error (OutOfBounds, idx)
        in
        let take f idx =
                let rec proc last =
                        if last < strlen && f (peek last) then proc (last + 1)
                        else last
                in
                let last = proc idx in
                let len = last - idx in
                match len with
                | 0 -> Error (EmptyString, idx)
                | _ -> Ok (String.sub str idx len, last)
        in
        let ask f idx =
                let rec proc acc idx =
                        if idx < strlen then
                                match f idx with
                                | Ok (Some c, idx') -> proc (c :: acc) idx'
                                | Ok (None, idx') -> Ok (implode_rev acc, idx')
                                | Error e -> Error e
                        else Ok (implode_rev acc, idx)
                in
                let= (data, idx') = proc [] idx in
                Ok (data, idx')
        in
        let skip f idx =
                let rec proc last =
                        if last < strlen && f (peek last) then proc (last + 1)
                        else last
                in
                proc idx
        in
        let skip_ws idx = skip is_ws idx
        in
        let chr ch idx =
                if idx < strlen && peek idx = ch then Ok (ch, idx + 1)
                else Error (CharMismatch ch, idx)
        in
        let nhex acc idx =
                if idx < strlen && is_hex (peek idx) then
                        Ok (16 * acc + parse_hex_unsafe (peek idx), idx + 1)
                else Error (HexCharExpected, idx)
        in
        let hexword idx =
                let= (a, idx) = nhex 0 idx in
                let= (b, idx) = nhex a idx in
                let= (c, idx) = nhex b idx in
                let= (d, idx) = nhex c idx in
                Ok (d, idx)
        in
        let parse_null idx =
                match take is_letter idx with
                | Ok ("null", idx') -> Ok (Null, idx')
                | _ -> Error (NullExpected, idx)
        in
        let parse_bool idx =
                match take is_letter idx with
                | Ok ("true", idx') -> Ok (Bool true, idx')
                | Ok ("false", idx') -> Ok (Bool false, idx')
                | _ -> Error (BoolExpected, idx)
        in
        let parse_number idx =
                (* parse sign *)
                let sign, idx =
                        match chr '-' idx with
                        | Ok (_, idx') -> -1, idx'
                        | Error _ -> 1, idx
                in
                (* parse integral part *)
                let= (n, idx) = take is_digit idx in
                let integer = sign * int_of_string n in
                (* parse fraction part *)
                let (fraction, precision, idx) =
                        match chr '.' idx with
                        | Error (_, idx) -> (0, 0, idx)
                        | Ok (_, idx) ->
                                match take is_digit idx with
                                | Error (_, idx) -> (0, 0, idx)
                                | Ok (n, idx) -> (int_of_string n, String.length n, idx)
                in
                (* parse exponent *)
                let= (exponent, idx) =
                        match chr 'e' idx with
                        | Error (_, idx) -> Ok (0, idx)
                        | Ok (_, idx) ->
                                match take is_digit idx with
                                | Error (_, idx) -> Error (ExponentRequired, idx)
                                | Ok (n, idx) -> Ok (int_of_string n, idx)
                in
                Ok (Number (make_number integer fraction precision exponent), idx)
        in
        let parse_string idx =
                let string_char idx =
                        let= () = check idx in
                        match peek idx with
                        | '"' -> Ok (None, idx)
                        | '\\' ->
                                (let= () = check (idx + 1) in
                                match peek (idx + 1) with
                                | '"' -> Ok (Some '"', idx + 2)
                                | '\\' -> Ok (Some '\\', idx + 2)
                                | '/' -> Ok (Some '/', idx + 2)
                                | 'b' -> Ok (Some '\b', idx + 2)
                                | 'f' -> Ok (Some '\014', idx + 2) (* '\f' not supported by OCaml? *)
                                | 'n' -> Ok (Some '\n', idx + 2)
                                | 'r' -> Ok (Some '\r', idx + 2)
                                | 't' -> Ok (Some '\t', idx + 2)
                                | 'u' ->
                                        let= (_hex, idx) = hexword (idx + 2) in
                                        Ok (Some ('?'), idx)  (* we don't support unicode, replace with question mark *)
                                | c -> Error (UnrecognisedEscapeSequence c, idx))
                        | c -> Ok (Some c, idx + 1)
                in
                match chr '"' idx with
                | Error (_, idx) -> Error (InvalidStringStart, idx)
                | Ok (_, idx) ->
                        let= str, idx = ask string_char idx in
                        match chr '"' idx with
                        | Error (_, idx) -> Error (InvalidStringEnd, idx)
                        | Ok (_, idx) -> Ok (String str, idx)
        in
        let rec parse_array idx =
                let rec parse_next_array_item acc idx =
                        let idx = skip_ws idx in
                        match chr ',' idx with
                        | Error (_, idx) -> Ok (List.rev acc, idx)
                        | Ok (_, idx) ->
                                match parse_value idx with
                                | Error (e, idx) -> Error (InvalidArrayElement e, idx)
                                | Ok (value, idx) ->
                                        parse_next_array_item (value :: acc) idx
                in
                let parse_array_items value idx =
                        parse_next_array_item [value] idx
                in
                let= (_, idx) = chr '[' idx in
                let= (items, idx) =
                        match parse_value idx with
                        | Error (_, idx) -> Ok ([], idx)
                        | Ok (value, idx) -> parse_array_items value idx
                in
                let idx = skip_ws idx in
                let= (_, idx) = chr ']' idx in
                Ok (Array items, idx)
        and parse_object idx =
                let parse_object_item idx =
                        match parse_string idx with
                        | Error _ as err -> err
                        | Ok (String key, idx) -> (
                                let idx = skip_ws idx in
                                let= (_, idx) = chr ':' idx in
                                let idx = skip_ws idx in
                                match parse_value idx with
                                | Error (e, idx) -> Error (InvalidObjectElement e, idx)
                                | Ok (value, idx) -> Ok ((key, value), idx)
                        )
                        | Ok _ -> Error (Fatal, idx)
                in
                let rec parse_next_object_item acc idx =
                        match chr ',' idx with
                        | Error (_, idx) -> Ok (List.rev acc, idx)
                        | Ok (_, idx) ->
                                 let idx = skip_ws idx in
                                 let= (item, idx) = parse_object_item idx in
                                 let idx = skip_ws idx in
                                 parse_next_object_item (item :: acc) idx
                in
                let parse_object_items item idx' =
                        parse_next_object_item [item] idx'
                in
                let parse_object_rest item idx =
                        let idx = skip_ws idx in
                        let= (items, idx) = parse_object_items item idx in
                        Ok (items, idx)
                in
                let= (_, idx) = chr '{' idx in
                let idx = skip_ws idx in
                let= (pairs, idx) =
                        if peek idx = '"' then
                                let= (item, idx) = parse_object_item idx in
                                parse_object_rest item idx
                        else
                                Ok ([], idx)
                in
                let idx = skip_ws idx in
                let= (_, idx) = chr '}' idx in
                Ok (Object pairs, idx)
        and parse_value idx =
                let idx = skip_ws idx in
                if idx < strlen then
                        match peek idx with
                        | 'n' -> parse_null idx
                        | 't' | 'f' -> parse_bool idx
                        | '0'..'9' | '-' -> parse_number idx
                        | '"' -> parse_string idx
                        | '[' -> parse_array idx
                        | '{' -> parse_object idx
                        | _ -> Error (InvalidValue, idx)
                else Error (OutOfBounds, idx)
        in
        let= (result, idx) = parse_value 0 in
        if idx = strlen then Ok result
        else Error (Garbage, idx)

(** Parse string into json *)
let parse str =
        let strlen = length str in
        if strlen > 0 then parse1 str strlen
        else Error (EmptyString, 0)

let print_bool chan = function
        | false -> output_string chan "false"
        | true -> output_string chan "true"

let print_number chan n =
        let print_fraction chan fraction =
                if fraction > 0 then
                        Printf.fprintf chan ".%i" fraction
        in
        let print_exponent chan exponent =
                if exponent > 0 then
                        Printf.fprintf chan "e%i" exponent
        in
        Printf.fprintf chan "%i%a%a" n.integer print_fraction n.fraction print_exponent n.exponent

let escape = function
        | '"' -> "\\\""
        | '\\' -> "\\\\"
        | '\r' -> "\\r"
        | '\n' -> "\\n"
        | '\t' -> "\\t"
        | '\b' -> "\\b"
        | '\014' -> "\\f"
        | ch -> sofc ch

let transliterate chan str =
        String.iter (fun ch -> output_string chan (escape ch)) str

let rec print_string chan s =
        Printf.fprintf chan {|"%a"|} transliterate s
and print_array chan a =
        let rec print_array_items chan = function
                | [] -> ()
                | hd :: tl ->
                        Printf.fprintf chan ", %a" print hd;
                        print_array_items chan tl
        in
        match a with
        | [] -> output_string chan "[]"
        | [item] -> Printf.fprintf chan "[%a]" print item
        | hd :: tl -> Printf.fprintf chan "[%a, %a]" print hd print_array_items tl
and print_object chan o =
        let rec print_object_items chan = function
                | [] -> ()
                | (key, value) :: tl ->
                        Printf.fprintf chan ", %a: %a" print_string key print value;
                        print_object_items chan tl
        in
        match o with
        | [] -> output_string chan "{}"
        | [key, value] -> Printf.fprintf chan "{%a: %a}" print_string key print value
        | (key, value) :: tl -> Printf.fprintf chan "{%a: %a%a}" print_string key print value print_object_items tl
(** Print string representation to given channall chan *)
and print chan = function
        | Null -> output_string chan "null"
        | Bool b -> print_bool chan b
        | Number n -> print_number chan n
        | String s -> print_string chan s
        | Array a -> print_array chan a
        | Object o -> print_object chan o
