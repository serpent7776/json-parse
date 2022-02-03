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
        | _ -> false

let end_quote ch prev = ch = '"' && prev <> '\\'
let non_ending_quote ch prev = not (end_quote ch prev)

let sofc chr =
        String.make 1 chr

(** length s returns length of the string s ignoring any trailing whitespaca *)
let length str =
        let strlen = String.length str in
        let rec proc idx =
                if idx >= 0 && is_ws str.[idx] then proc (idx - 1)
                else idx + 1
        in
        if strlen > 0 then proc (strlen - 1)
        else 0

(** Parse non empty string into json *)
let parse1 str strlen =
        let peek idx = str.[idx]
        in
        let take idx f =
                let rec proc last =
                        if last < strlen && f (peek last) then proc (last + 1)
                        else last
                in
                let last = proc idx in
                let len = last - idx in
                match len with
                | 0 -> Error ("Empty string parsed", idx)
                | _ -> Ok (String.sub str idx len, last)
        in
        let ask2 idx f =
                let rec proc idx prev =
                        if idx < strlen && f (peek idx) prev then proc (idx + 1) (peek idx)
                        else idx
                in
                (*
                 * Accessing idx - 1 should always be safe, because we always
                 * match on first char to decide which parse_* function and we
                 * ever call ask on the following chars
                 *)
                let prev_ch = peek (idx - 1) in
                let last = proc idx prev_ch in
                let len = last - idx in
                (String.sub str idx len, last)
        in
        let skip idx f =
                let rec proc last =
                        if last < strlen && f (peek last) then proc (last + 1)
                        else last
                in
                proc idx
        in
        let chr ch idx =
                if idx < strlen && peek idx = ch then Ok (ch, idx + 1)
                else Error ("Expected " ^ sofc(ch), idx)
        in
        let parse_null idx =
                match take idx is_letter with
                | Ok ("null", idx') -> Ok (Null, idx')
                | _ -> Error ("Expected null", idx)
        in
        let parse_bool idx =
                match take idx is_letter with
                | Ok ("true", idx') -> Ok (Bool true, idx')
                | Ok ("false", idx') -> Ok (Bool false, idx')
                | _ -> Error ("Expected bool", idx)
        in
        let parse_number idx =
                (* parse sign *)
                let sign, idx' =
                        match chr '-' idx with
                        | Ok (_, idx') -> -1, idx'
                        | Error _ -> 1, idx
                in
                let parse_number_exponent idx =
                        match chr 'e' idx with
                        | Error _ -> Ok (None, idx)
                        | Ok (_, idx') ->
                                match take idx' is_digit with
                                | Error _ -> Error ("Missing digits after exponent", idx')
                                | Ok (e, idx'2) ->
                                        Ok (Some (int_of_string e), idx'2)
                in
                let parse_number_fraction idx =
                        match chr '.' idx with
                        | Error _ -> (None, idx)
                        | Ok (_, idx') ->
                                match take idx' is_digit with
                                | Error _ -> (None, idx')
                                | Ok (f, idx'2) ->
                                        let precision = String.length f in
                                        (Some (int_of_string f, precision), idx'2)
                in
                (* parse integral part *)
                Result.bind (take idx' is_digit) (fun (n, idx') ->
                        let integer = sign * int_of_string n in
                        (* parse fraction part *)
                        let fraction, precision, idx'2 = match parse_number_fraction idx' with
                        | (None, idx'2) -> 0, 0, idx'2
                        | (Some (fraction, precision), idx'2) -> fraction, precision, idx'2
                        in
                        (* parse exponent *)
                        match parse_number_exponent idx'2 with
                        | Error e -> Error e
                        | Ok (None, idx'3) -> Ok (Number (make_decimal integer fraction precision), idx'3)
                        | Ok (Some exponent, idx'3) ->
                                Ok (Number (make_number integer fraction precision exponent), idx'3)
                )
        in
        let parse_string idx =
                match chr '"' idx with
                | Error _ -> Error ({|String must start with "|}, idx)
                | Ok (_, idx') ->
                        let str, idx'2 = ask2 idx' non_ending_quote in
                        match chr '"' idx'2 with
                        | Error _ -> Error ({|String must end with "|}, idx'2)
                        | Ok (_, idx'3) ->
                                Ok (String str, idx'3)
        in
        let rec parse_array idx =
                let rec parse_array_items idx acc =
                        let idx = skip idx is_ws in
                        match chr ',' idx with
                        | Error _ -> Ok (List.rev acc, idx)
                        | Ok (_, idx') ->
                                match parse_value idx' with
                                | Error _ -> Error ("Failed to parse array element", idx')
                                | Ok (value, idx'2) ->
                                        parse_array_items idx'2 (value :: acc)
                in
                match chr '[' idx with
                | Error _ -> Error ("Expected [", idx)
                | Ok (_, idx') ->
                        match parse_value idx' with
                        | Error _ -> (
                                let idx' = skip idx' is_ws in
                                match chr ']' idx' with
                                | Error _ -> Error ("Expected ]", idx')
                                | Ok (_, idx'2) -> Ok (Array [], idx'2)
                        )
                        | Ok (value, idx'2) ->
                                match parse_array_items idx'2 [] with
                                | Error e -> Error e
                                | Ok (items, idx'3) ->
                                        let idx'3 = skip idx'3 is_ws in
                                        match chr ']' idx'3 with
                                        | Error _ -> Error ("Expected ]", idx'3)
                                        | Ok (_, idx'4) ->
                                                Ok (Array (value :: items), idx'4)
        and parse_object idx =
                let parse_object_item idx' =
                        match parse_string idx' with
                        | Error e -> Error e
                        | Ok (String key, idx'2) -> (
                                let idx'2 = skip idx'2 is_ws in
                                match chr ':' idx'2 with
                                | Error _ -> Error ("Expected :", idx'2)
                                | Ok (_, idx'3) ->
                                        let idx'3 = skip idx'3 is_ws in
                                        match parse_value idx'3 with
                                        | Error _ -> Error ("Failed to parse object element", idx'3)
                                        | Ok (value, idx'4) ->
                                                Ok (key, value, idx'4)
                        )
                        | Ok _ -> Error ("Should never happen", idx')
                in
                let rec parse_object_items idx acc =
                        match chr ',' idx with
                        | Error _ -> Ok (List.rev acc, idx)
                        | Ok (_, idx') ->
                                let idx' = skip idx' is_ws in
                                match parse_object_item idx' with
                                | Error e -> Error e
                                | Ok (key, value, idx'2) ->
                                        let idx'2 = skip idx'2 is_ws in
                                        parse_object_items idx'2 ((key, value) :: acc)
                in
                match chr '{' idx with
                | Error _ -> Error ("Expected {", idx)
                | Ok (_, idx') ->
                        let idx' = skip idx' is_ws in
                        match parse_object_item idx' with
                        | Error _ -> (
                                let idx' = skip idx' is_ws in
                                match chr '}' idx' with
                                | Error _ -> Error ("Expected }", idx')
                                | Ok (_, idx'2) -> Ok (Object [], idx'2)
                        )
                        | Ok (key, value, idx'2) ->
                                let idx'2 = skip idx'2 is_ws in
                                match parse_object_items idx'2 [] with
                                | Error e -> Error e
                                | Ok (items, idx'3) ->
                                        let idx'3 = skip idx'3 is_ws in
                                        match chr '}' idx'3 with
                                        | Error _ -> Error ("Expected }", idx'3)
                                        | Ok (_, idx'4) ->
                                                Ok (Object ((key, value) :: items), idx'4)
        and parse_value idx =
                let idx = skip idx is_ws in
                if idx < strlen then
                        match peek idx with
                        | 'n' -> parse_null idx
                        | 't' | 'f' -> parse_bool idx
                        | '0'..'9' | '-' -> parse_number idx
                        | '"' -> parse_string idx
                        | '[' -> parse_array idx
                        | '{' -> parse_object idx
                        | _ -> Error ("parse error", idx)
                else Error ("Out of bounds read attempt", idx)
        in
        match parse_value 0 with
        | Error e -> Error e
        | Ok (result, idx) ->
                if idx = strlen then Ok result
                else Error ("garbage found at theend of string", idx)

(** Parse string into json *)
let parse str =
        let strlen = length str in
        if strlen > 0 then parse1 str strlen
        else Error ("Empty string", 0)

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

let rec print_string chan s =
        Printf.fprintf chan {|"%s"|} s
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
