(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   gray.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: roblabla </var/spool/mail/roblabla>        +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/17 18:15:53 by roblabla          #+#    #+#             *)
(*   Updated: 2015/06/17 19:53:28 by roblabla         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let rec gray x =
    let rec concat l1 l2 = match l1 with
        | [] -> l2
        | a::b -> a :: (concat b l2)
    in
    let rec map fn = function
        | [] -> []
        | a::b -> fn a :: (map fn b)
    in
    let prefix = (^) in
    let rec revlist acc = function
        | [] -> acc
        | a :: b -> revlist (a :: acc) b
    in
    if x <= 1 then "0" :: "1" :: []
    else let g = gray (x - 1) in
    concat (map (prefix "0") g) (map (prefix "1") (revlist [] g))

let () =
    let my_print a = Printf.printf "%s, " a in
    List.iter my_print (gray 1); print_char '\n';
    List.iter my_print (gray 2); print_char '\n';
    List.iter my_print (gray 3); print_char '\n';
    List.iter my_print (gray 4); print_char '\n';
    List.iter my_print (gray 5); print_char '\n';
