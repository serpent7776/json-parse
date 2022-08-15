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
                                | Error (_, idx) -> Error ("Exponent required", idx)
                                | Ok (n, idx) -> Ok (int_of_string n, idx)
                in
                Ok (Number (make_number integer fraction precision exponent), idx)
        in
        let parse_string idx =
                match chr '"' idx with
                | Error (_, idx) -> Error ({|String must start with "|}, idx)
                | Ok (_, idx) ->
                        let str, idx = ask2 idx non_ending_quote in
                        match chr '"' idx with
                        | Error (_, idx) -> Error ({|String must end with "|}, idx)
                        | Ok (_, idx) -> Ok (String str, idx)
        in
        let rec parse_array idx =
                let rec parse_next_array_item acc idx =
                        let idx = skip_ws idx in
                        match chr ',' idx with
                        | Error (_, idx) -> Ok (List.rev acc, idx)
                        | Ok (_, idx) ->
                                match parse_value idx with
                                | Error (_, idx) -> Error ("Failed to parse array element", idx)
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
                                | Error (_, idx) -> Error ("Failed to parse object element", idx)
                                | Ok (value, idx) -> Ok ((key, value), idx)
                        )
                        | Ok _ -> Error ("Should never happen", idx)
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
                        match parse_object_item idx with
                        | Error (_, idx) -> Ok ([], idx)
                        | Ok (item, idx) -> parse_object_rest item idx
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
