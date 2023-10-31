type expression =
  | Concat of expression * expression
  | Alt of expression * expression
  | Optional of expression
  | Char of char
  | AnyChar

exception ParseError of string

let rec parse_expr pattern =
  let length = String.length pattern in

  let rec parse_primary i =
    if i >= length then raise (ParseError "Unexpected end of pattern")
    else match pattern.[i] with
      | '(' ->
        let (e, j) = parse_alt (i + 1) in
        if j >= length || pattern.[j] <> ')' then raise (ParseError "Expected ')'");
        (e, j + 1)
      | '.' -> (AnyChar, i + 1)
      | c -> (Char c, i + 1)
  and parse_optional i =
    let (e, j) = parse_primary i in
    if j < length && pattern.[j] = '?' then (Optional e, j + 1)
    else (e, j)
  and parse_concat i =
    let (e1, j) = parse_optional i in
    if j < length && pattern.[j] <> ')' && pattern.[j] <> '|' then
      let (e2, k) = parse_concat j in
      (Concat(e1, e2), k)
    else (e1, j)
  and parse_alt i =
    let (e1, j) = parse_concat i in
    if j < length && pattern.[j] = '|' then
      let (e2, k) = parse_alt (j + 1) in
      (Alt(e1, e2), k)
    else (e1, j)
  in

  fst (parse_alt 0)

let rec match_expr expr str idx =
  match expr with
  | Concat(e1, e2) -> 
    (match match_expr e1 str idx with
     | Some new_idx -> match_expr e2 str new_idx
     | None -> None)
  | Alt(e1, e2) ->
    (match match_expr e1 str idx with
     | Some _ as result -> result
     | None -> match_expr e2 str idx)
  | Optional(e) ->
    (match match_expr e str idx with
     | Some _ as result -> result
     | None -> Some idx)
  | Char(c) ->
    if idx < String.length str && str.[idx] = c then Some (idx + 1)
    else None
  | AnyChar ->
    if idx < String.length str then Some (idx + 1)
    else None

let match_pattern pattern str =
  let expr = parse_expr pattern in
  match match_expr expr str 0 with
  | Some idx -> idx = String.length str 
  | None -> false


let rec interact () =
  print_string "pattern? ";
  let pattern = read_line () in
  print_string "string? ";
  let str = read_line () in
  if str <> "" then (
    if match_pattern pattern str
    then print_endline "match"
    else print_endline "no match";
    interact ()
  )
  else print_endline "Exiting..."

let () = interact ()
