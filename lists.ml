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

