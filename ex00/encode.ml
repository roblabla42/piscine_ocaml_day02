(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   encode.ml                                          :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: roblabla </var/spool/mail/roblabla>        +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/17 09:54:34 by roblabla          #+#    #+#             *)
(*   Updated: 2015/06/17 11:08:59 by roblabla         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let encode l =
    let rec revlist acc = function
        | [] -> acc
        | a :: b -> revlist (a :: acc) b
    in
    let rec inner count acc = function
        | [] -> []
        | [elem] -> (count + 1, elem) :: acc
        | a :: (b :: _ as t) when a = b -> inner (count + 1) acc t
        | a :: t                        -> inner 0 ((count + 1, a) :: acc) t
    in
    revlist [] (inner 0 [] l)

let () =
    let my_print (a, b) = Printf.printf "(%d, %d), " a b in
    List.iter my_print (encode (0 :: 0 :: 0 :: 3 :: 2 :: 3 :: 3 :: 3 :: 3 :: 0 :: 0 :: 4 :: []));
