let find str start char =
  try String.index_from str start char
  with Not_found -> -1

let split s =
  let rec step i acc =
    if i >= String.length s then
      List.rev acc
    else
      let i_dot = find s i '.' in
      let i_mark = find s i '?' in
      let i_bang = find s i '!' in
      let i' = min i_dot (min i_mark i_bang) in
      if i' >= 0 then
        step (i' + 1) (String.sub s i (i' - i)::acc)
      else
        List.rev (String.sub s i (String.length s - i)::acc)
  in
  step 0 []

let sanitize s =
  let open String in
  List.map
    (fun s -> s |> trim |> map Char.lowercase_ascii)
    (split s)