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

let (let=) v f =
        match v with
        | Error _ as err -> err
        | Ok ok -> f ok

let ppi parse ok_f err_f idx =
        match parse idx with
        | Ok (_, idx') -> ok_f idx'
        | Error (_, idx') -> err_f idx'

let ppx parse ok_f err_f idx =
        match parse idx with
        | Ok (v, idx) -> ok_f v idx
        | Error (err, idx) -> err_f err idx

let none idx = (None, idx)
let none2 _ idx = (None, idx)

let oknone idx = Ok (None, idx)

(** Parse non empty string into json *)
let parse1 str strlen =
        let peek idx = str.[idx]
        in
        let take f idx =
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
                else Error ("Expected " ^ sofc(ch), idx)
        in
        let parse_null idx =
                match take is_letter idx with
                | Ok ("null", idx') -> Ok (Null, idx')
                | _ -> Error ("Expected null", idx)
        in
        let parse_bool idx =
                match take is_letter idx with
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
                        let no_exponent_digits_error _ idx' =
                                Error ("Missing digits after exponent", idx')
                        in
                        let make_exponent e idx' =
                                Ok (Some (int_of_string e), idx')
                        in
                        ppi (chr 'e')
                                (ppx (take is_digit) make_exponent no_exponent_digits_error)
                                oknone
                                idx
                in
                let parse_dot_fraction idx =
                        let make_fraction f idx' =
                                let precision = String.length f in
                                (Some (int_of_string f, precision), idx')
                        in
                        ppi (chr '.')
                                (ppx (take is_digit) make_fraction none2)
                                none
                                idx
                in
                (* parse integral part *)
                let= (n, idx') = take is_digit idx' in
                let integer = sign * int_of_string n in
                (* parse fraction part *)
                let fraction, precision, idx'2 = match parse_dot_fraction idx' with
                | (None, idx'2) -> 0, 0, idx'2
                | (Some (fraction, precision), idx'2) -> fraction, precision, idx'2
                in
                (* parse exponent *)
                match parse_number_exponent idx'2 with
                | Error _ as err -> err
                | Ok (None, idx'3) -> Ok (Number (make_decimal integer fraction precision), idx'3)
                | Ok (Some exponent, idx'3) ->
                        Ok (Number (make_number integer fraction precision exponent), idx'3)
        in
        let parse_string idx =
                let invalid_string_start_error idx' =
                        Error ({|String must start with "|}, idx')
                in
                let invalid_string_end_error idx' =
                        Error ({|String must end with "|}, idx')
                in
                let make_string s idx =
                        Ok (String s, idx)
                in
                let parse_string_body idx' =
                        let str, idx'2 = ask2 idx' non_ending_quote in
                        ppi (chr '"') (make_string str) invalid_string_end_error idx'2
                in
                ppi (chr '"') parse_string_body invalid_string_start_error idx
        in
        let rec parse_array idx =
                let make_empty_array _ idx' =
                        let idx' = skip_ws idx' in
                        let= (_, idx'2) = chr ']' idx' in
                        Ok (Array [], idx'2)
                in
                let invalid_array_element_error _ idx' =
                        Error ("Failed to parse array element", idx')
                in
                let rec parse_next_array_item acc idx' =
                        let idx' = skip_ws idx' in
                        ppi (chr ',') (
                                ppx parse_value
                                        (fun value idx'2 -> parse_next_array_item (value :: acc) idx'2)
                                        invalid_array_element_error
                                )
                                (fun idx'2 -> Ok (List.rev acc, idx'2))
                                idx'
                in
                let parse_array_items value idx' =
                        parse_next_array_item [value] idx'
                in
                let parse_array_rest value idx' =
                        let= (values, idx'2) = parse_array_items value idx' in
                        let idx'2 = skip_ws idx'2 in
                        let= (_, idx'3) = chr ']' idx'2 in
                        Ok (Array values, idx'3)
                in
                let= (_, idx') = chr '[' idx in
                ppx parse_value parse_array_rest make_empty_array idx'
        and parse_object idx =
                let invalid_object_value_error _ idx' =
                        Error ("Failed to parse object element", idx')
                in
                let make_object_item key value idx' =
                        Ok ((key, value), idx')
                in
                let parse_object_item idx' =
                        match parse_string idx' with
                        | Error _ as err -> err
                        | Ok (String key, idx'2) -> (
                                let idx'2 = skip_ws idx'2 in
                                let= (_, idx'3) = chr ':' idx'2 in
                                let idx'3 = skip_ws idx'3 in
                                ppx (parse_value) (make_object_item key) invalid_object_value_error idx'3
                        )
                        | Ok _ -> Error ("Should never happen", idx')
                in
                let rec parse_next_object_item acc idx' =
                        ppi (chr ',') (fun idx'2 ->
                                        let idx'2 = skip_ws idx'2 in
                                        let= (item, idx'3) = parse_object_item idx'2 in
                                        let idx'3 = skip_ws idx'3 in
                                        parse_next_object_item (item :: acc) idx'3
                                )
                                (fun idx'2 -> Ok (List.rev acc, idx'2))
                                idx'
                in
                let parse_object_items item idx' =
                        parse_next_object_item [item] idx'
                in
                let parse_object_rest item idx' =
                        let idx' = skip_ws idx' in
                        let= (items, idx'2) = parse_object_items item idx' in
                        let idx'2 = skip_ws idx'2 in
                        let= (_, idx'3) = chr '}' idx'2 in
                        Ok (Object items, idx'3)
                in
                let make_empty_object _ idx' =
                        let idx' = skip_ws idx' in
                        let= (_, idx'2) = chr '}' idx' in
                        Ok (Object [], idx'2)
                in
                let= (_, idx') = chr '{' idx in
                let idx' = skip_ws idx' in
                ppx parse_object_item parse_object_rest make_empty_object idx'
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
                        | _ -> Error ("Invalid value", idx)
                else Error ("Out of bounds read attempt", idx)
        in
        let= (result, idx) = parse_value 0 in
        if idx = strlen then Ok result
        else Error ("garbage found at the end of string", idx)

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
