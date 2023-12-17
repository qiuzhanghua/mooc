type trie = Trie of int option * char_to_children
and char_to_children = (char * trie) list

val empty : trie
val insert : string -> int -> trie -> trie
val lookup : string -> trie -> int option
