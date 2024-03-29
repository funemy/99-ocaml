(* problem 1, return the last element of a list *)
let rec last = function
  | [] -> None
  | [x] -> Some x
  | _::tl -> last tl
;;

(* problem 2, find the last two element of a list *)
let rec last_two = function
  | [] | [_] -> None
  | [x; y] -> Some (x, y)
  | _::tl -> last_two tl
;;

(* problem 3 find the k-th element of a list *)

(* bad example blow *)
(* let rec at k xs = match (k, xs) with
 * | k, _ when k < 1 -> None
 * | _, [] -> None
 * | 1, hd::tl -> Some hd
 * | k, hd::tl -> at (k-1) tl
 * ;; *)

let rec at k = function
  | [] -> None
  | hd::tl -> if k = 1 then Some hd else at (k-1) tl
;;

(* problem 4 length of a list *)
let length list =
  let rec aux acc = function
    | [] -> acc
    | _::tl -> aux (acc+1) tl
  in aux 0 list
;;

(* problem 5 reverse a list *)
let rev list =
  let rec aux acc = function
    | [] -> acc
    | hd::tl -> aux (hd::acc) tl
  in aux [] list
;;

(* problem 6 if a list is palindrome *)
let is_palindrome list =
  let r = rev list
  in list = r
;;

(* problem 7 flatten a nested list *)
(* since there's no nested list in OCaml, we need to define one *) type 'a node =
  | One of 'a
  | Many of 'a node list

(* list append is always inefficient *)
(* let rec flatten nlist =
 *   let rec aux acc = function
 *     | [] -> acc
 *     | (One x)::tl -> aux (acc @ [x]) tl
 *     | (Many xs)::tl -> aux (acc @ (flatten xs)) tl
 *   in aux [] nlist
 * ;; *)

(* a slightly better version *)
let flatten nlist =
  let rec aux acc = function
    | [] -> acc
    | One x :: tl -> aux (x :: acc) tl
    | Many xs :: tl -> aux (aux acc xs) tl
  in List.rev (aux [] nlist)
;;

let test_7 = flatten [ One "a" ; Many [ One "b" ; Many [ One "c" ; One "d" ] ; One "e" ] ];;

(* problem 8 eliminate consecutive duplicates of list elements *)
(* tail recursive version *)
(* let compress list =
 *   let rec aux last acc = function
 *     | [] -> acc
 *     | hd::tl -> if hd = last then aux last acc tl else aux hd (hd :: acc) tl
 *   in List.rev (aux (List.hd list) [List.hd list] (List.tl list)) *)

(* concise version *)
let rec compress = function
  | x :: y :: tl -> if x = y then compress (y :: tl) else x :: (compress (y :: tl))
  | l -> l

(* slightly better concise version *)
(* Note the use of as *)
let rec compress = function
  | x :: (y :: _ as tl) -> if x = y then compress tl else x :: compress tl
  | smaller -> smaller

(* problem 9 pack consecutive duplicates of list elements into sub-lists*)
let pack list =
  let rec aux sub acc = function
    | [] -> acc
    | x :: [] -> (x::sub)::acc
    | x :: (y :: _ as tl) -> if x = y then aux (x::sub) acc tl else aux [] ((x :: sub) :: acc) tl
  in List.rev (aux [] [] list)

(* problem 10 RLE of a list *)
let encode list =
  let rec aux count acc = function
    | [] -> []
    | [x] -> (count, x)::acc
    | x:: (y :: _ as tl) -> if x = y then aux (count + 1) acc tl else aux 1 ((count, x)::acc) tl
  in List.rev (aux 1 [] list)
;;

let text_10 = encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;

(* problem 11 modified RLE *)
type 'a rle =
  | One of 'a
  | Many of int * 'a

let encode list =
  let create_rle count e =
    if count = 1
    then One e
    else Many (count, e) in
  let rec aux count acc = function
    | [] -> []
    | [x] -> (create_rle count x) :: acc
    | x:: (y :: _ as tl) ->
      if x = y
      then aux (count + 1) acc tl
      else aux 1 ((create_rle count x) :: acc) tl
  in List.rev (aux 1 [] list)
;;

let text_11 = encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;

(* problem 12 decode RLE of problem 11 *)
let decode list =
  let rec append_n acc elem = function
    | 1 -> elem :: acc
    | n -> append_n (elem :: acc) elem (n-1) in
  let rec aux acc = function
  | [] -> acc
  | One x :: tl -> aux (append_n acc x 1) tl
  | Many (count, x) :: tl -> aux (append_n acc x count) tl
  in List.rev (aux [] list)
  (* can also be written as below *)
  (* in aux [] (List.rev list) *)
;;

let test_12 = decode [Many (4,"a"); One "b"; Many (2,"c"); Many (2,"a"); One "d"; Many (4,"e")];;

(* problem 13 *)
(* this one is weird, I see no difference from problem 11 *)

(* problem 14 duplicate the element of a list*)
let rec duplicate = function
  | [] -> []
  | x::tl -> x :: x :: duplicate tl
;;

(* problem 15 replicate the list element for the given times *)
let replicate list n =
  let rec n_times n x acc =
    if n = 1
    then x :: acc
    else n_times (n-1) x (x::acc) in
  let rec aux acc = function
    | [] -> acc
    | x :: tl -> aux (n_times n x acc) tl
  in aux [] (List.rev list)
;;

(* problem 16 drop every n-th element in a list *)
let drop list n =
  let rec aux acc n' = function
    | [] -> acc
    | x :: tl -> if n' = 1 then aux acc n tl else aux (x :: acc) (n' - 1) tl
  in List.rev (aux [] n list)
;;

(* problem 17 split a list into two, the length of the first part is given *)
(* does not handle negative number *)
let split list n =
  let rec aux acc n' = function
    | [] -> (List.rev acc, [])
    | x :: tl as l -> if n' = 0 then (List.rev acc, l) else aux (x :: acc) (n' - 1) tl
  in aux [] n list
;;

let test_17_1 = split [1;2;3;4;5] 2;;
let test_17_2 = split [1;2;3;4;5] 0;;

(* Problem 18 extract a slice from a list *)
let slice list s e =
  let rec aux acc n = function
    | [] -> acc
    | x :: tl -> if n < s then aux acc (n + 1) tl
      else if n >= s && n <= e then aux (x :: acc) (n + 1) tl
      else acc
  in List.rev (aux [] 0 list)
;;

(* the standard answer is slightly different, but I think it's actually worse *)
(* so the process of slice can be split into two sub-actions *)
(* we first drop first s elements from the list *)
(* then we pick (s - e + 1) elements from the rest of the list *)
(* hence, we write 2 helper functions: take and drop *)
let slice list i k =
  let rec drop n = function
    | [] -> []
    | x :: tl as l -> if n = 0 then l else drop (n - 1) tl in
  let rec take n = function
    | [] -> []
    | x :: tl -> if n = 0 then [] else x :: take (n - 1) tl in
  take (k - i + 1) (drop i list)
;;

(* the above answer can be further improved to catch the common patterns in drop and take *)
(* I think this style is bad, the result of fold_until is hard to understand *)
(* actually take and drop a doing totally 2 different thins *)
let slice list i k =
  (* the return type is a tuple, the first element is the take list, then second is the drop list *)
  let rec fold_until f acc n = function
    | [] -> (acc, [])
    | x :: tl as l -> if n = 0 then (acc, l) else fold_until f (f acc x) (n - 1) tl
  in
  let _, drop = fold_until (fun _ _ -> []) [] i list in
  let take, _ = fold_until (fun acc x -> x :: acc) [] (k - i + 1) drop in
  List.rev take
;;

let test_18 = slice ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 2 6;;

(* problem 19 rotate a list N places to the left *)
(* this answer uses previous helper function *)
let rotate list n =
  let len = length list in
  (* make sure s is always a positive int within the length *)
  (* also be careful about division by zero *)
  let s = if len = 0 then 0 else (n mod len + len ) mod len in
  let (fst, snd) = split list s in
  snd @ fst
;;

(* problem 20 remove the k-th element from a list (start from 0) *)
let rec remove_at k = function
  | [] -> []
  | x :: tl -> if k = 0 then tl else x :: remove_at (k - 1) tl
;;

(* problem 21 insert an element at the given position *)
let rec insert_at e k = function
  | [] -> [e]
  | x :: tl as l -> if k = 0 then e :: l else x :: insert_at e (k - 1) tl
;;

(* problem 22 create a list of range *)
let rec range s e =
  let step = if s <= e then 1 else -1 in
  if s = e then [s] else s :: range (s + step) e
;;

(* tail recursion *)
let range s e =
  let l, h = if s > e then e, s else s, e in
  let rec aux acc n =
    if n <= h
    then aux (n :: acc) (n + 1)
    else acc
  in if s > e then aux [] l else List.rev (aux [] l)

(* problem 23 extract given number of randomly selected elements from a list *)
(* I think this problem is ill-posed *)
let rand_select list n =
  (* return a tuple, the first element is the selected element *)
  (* the second element is the rest of the list *)
  (* but notice the rest of list is not in the same order *)
  (* because the elements in acc is in reversed order *)
  (* but this is still alright, because we are doing a random selection *)
  let rec extract_n acc n = function
    | [] -> raise Not_found
    | x :: tl -> if n = 0 then (x, acc @ tl) else extract_n (x::acc) (n - 1) tl in
  let rec aux acc n list len =
    if n = 0 then acc
    else let picked, rest = extract_n [] (Random.int len) list in
      aux (picked :: acc) (n - 1) rest (len - 1) in
  let len = List.length list in
  aux [] n list len
;;

let test_23 = rand_select ["a";"b";"c";"d";"e";"f";"g";"h"] 3;;

(* problem 24, draw n different random numbers from a range *)
let lotto_select n e =
  let list = range 1 e in
  rand_select list n
;;

let test_24 = lotto_select 6 49;;

(* problem 25, random permutation of a list *)
let permutation list =
  rand_select list (List.length list)
;;

let test_25 = permutation ["a"; "b"; "c"; "d"; "e"; "f"];;

(* problem 26, generate combination of k distinct objects from a list *)
(* all subset of size K *)
let extract_2 list =
  let rec combine acc h = function
    | [] -> acc
    | x :: tl -> combine ((h :: [x]) :: acc) h tl in
  let rec aux acc = function
    | [] -> acc
    | x :: tl -> let result = combine acc x tl in
      aux result tl
  in aux [] list
;;

let extract n list =
  let rec combine acc h = function
    | [] -> acc
    | x :: tl -> combine ((h :: x) :: acc) h tl in
  let rec aux acc n list =
    if n = 2 then extract_2 list
    else match list with
      | [] -> acc
      | x :: tl -> let result = aux [] (n - 1) tl in
        (combine acc x result) @ aux [] n tl
  in List.rev (aux [] n list)
;;

(* Above is a over-complex solution *)
(* The main thing is the extract_2 function, we can simply use List.map to achieve that *)
(* also my previous solution has bug when n = 1*)
(* solution below generalize n to 1 *)
(* it's elegant but inefficient *)
let rec extract n list =
  (* for combine_head, each time it will return a single element wrapped with brackets [x] *)
  if n <= 0 then [[]]
  else
  (* this part is almost the same as solution above *)
  (* the remaining problem for this part is when to stop *)
  match list with
  | [] -> []
  | h :: t ->
    let combine_head = List.map (fun x -> h :: x) (extract (n - 1) t) in
    let combine_rest = extract n t in
    combine_head @ combine_rest
;;

let test_26_1 = extract 1 ["a";"b";"c";"d"];;
let test_26_2 = extract 2 ["a";"b";"c";"d"];;
let test_26_3 = extract 3 ["a";"b";"c";"d"];;

(* problem 27, group the elements of a set into disjoint subsets *)
let group list sizes =
  let initial = List.map (fun size -> size, []) sizes in
  let prepend
;;

