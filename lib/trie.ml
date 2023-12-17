type trie = Trie of int option * char_to_children
and char_to_children = (char * trie) list

let empty = Trie (None, [])

let insert w v trie =
  let rec insert' w v trie =
    match (w, trie) with
    | [], Trie (_, children) -> Trie (Some v, children)
    | c :: cs, Trie (v', children) ->
        let children' =
          match List.assoc_opt c children with
          | None -> (c, insert' cs v empty) :: children
          | Some trie -> (c, insert' cs v trie) :: List.remove_assoc c children
        in
        Trie (v', children')
  in
  insert' (List.of_seq @@ String.to_seq w) v trie

let lookup w trie =
  let rec lookup' w trie =
    match (w, trie) with
    | [], Trie (v, _) -> v
    | c :: cs, Trie (_, children) -> (
        match List.assoc_opt c children with
        | None -> None
        | Some trie -> lookup' cs trie)
  in
  lookup' (List.of_seq @@ String.to_seq w) trie
