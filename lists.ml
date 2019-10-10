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
(* since there's no nested list in OCaml, we need to define one *)
type 'a node =
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
let split list n =
  let rec aux acc n' = function
    | [] -> (List.rev acc, [])
    | x :: tl -> if n' = 1 then (List.rev (x :: acc), tl) else aux (x :: acc) (n' - 1) tl
  in aux [] n list
;;

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

(* this answer can be further improved to catch the common patterns in drop and take *)
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
let rotate list n = []
;;

