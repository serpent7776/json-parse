let read_whole_file filename =
        let ch = open_in filename in
        let s = really_input_string ch (in_channel_length ch) in
        close_in ch;
        s

let print = Sys.argv.(1) = "-p"
let filename = Sys.argv.(2)

let () =
        let str = read_whole_file filename in
        match Json.parse str with
        | Error (e,l) ->
                Printf.printf "%s at byte %i\n" e l;
                exit 1
        | Ok json ->
                        if print then
                                Printf.printf "%a\n" Json.print json
                        else
                                Printf.printf "Ok\n"

