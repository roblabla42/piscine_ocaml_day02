(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   crossover.ml                                       :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: roblabla </var/spool/mail/roblabla>        +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/17 12:11:27 by roblabla          #+#    #+#             *)
(*   Updated: 2015/06/17 12:43:01 by roblabla         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let crossover l1 l2 =
    let rec inner acc = function
        | ([], _) -> acc
        | (a::b, []) -> inner acc (b, l2)
        | (a::_ as t, c::d) when a = c -> inner (a :: acc) (t, d)
        | (a::_ as t, _::d) -> inner acc (t, d)
    in
    inner [] (l1, l2)

let () =
    let my_print = Printf.printf "%d, " in
    List.iter my_print (crossover (1 :: 2 :: 3 :: 4 :: 6 :: []) (1 :: 2 :: 5 :: 4 :: []))
