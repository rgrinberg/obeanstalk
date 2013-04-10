open Core.Std

(** split_exn s ~on splits the string [s] on the first occurence of [on] into
 * a tuple of strings. [on] itself is discarded *)
let split_exn s ~on = 
  let open String in
  let i = index_exn s on in
  let len = (length s) - i in
  (sub s ~pos:0 ~len:i, sub s ~pos:(i+1) ~len:(len-1))

(* just enough to parse whatever we get from obeanstalk *)
let to_dict s = 
  match String.split s ~on:'\n' with
  | [] -> [] (* first element should be header *)
  | _::lines -> (* I assume first line is the header *)
    lines |> List.filter_map ~f:(fun l -> 
      try let (k,v) = split_exn ~on:':' l in Some (k, String.strip v)
      (* TODO : fix this "inelegant" error handling *)
      with _ -> (Printf.printf "Could not parse '%s'\n" l; None))

let to_list s = 
  match String. split s ~on:'\n' with
  | [] -> []
  | _::lines ->
    lines |> List.filter_map ~f:(fun l ->
      try begin 
        assert (l.[0] = '-');
        Some(l |> String.sub ~pos:1 ~len:(String.length l - 1) |> String.strip)
      end
      with _ -> (Printf.printf "Could not parse '%s'\n" l; None))
