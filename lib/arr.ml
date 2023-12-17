let knuth_shuffle = fun arr ->
  let len = Array.length arr in
  for i = 0 to len - 1 do
    let j = Random.int (len - i) + i in
    let tmp = arr.(i) in
    arr.(i) <- arr.(j);
    arr.(j) <- tmp
  done;
  arr