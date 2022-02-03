let read_whole_file filename =
        let ch = open_in filename in
        let s = really_input_string ch (in_channel_length ch) in
        close_in ch;
        s

let filename = Sys.argv.(1)

let () =
        let str = read_whole_file filename in
        match Json.parse str with
        | Error (e,l) -> Printf.printf "%s at byte %i\n" e l
        | Ok json -> Printf.printf "%a\n" Json.print json

