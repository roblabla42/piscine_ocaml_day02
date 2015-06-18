(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   sequence.ml                                        :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: roblabla </var/spool/mail/roblabla>        +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/18 08:43:16 by roblabla          #+#    #+#             *)
(*   Updated: 2015/06/18 17:02:33 by roblabla         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let sequence n =
    let rec rev acc = function
        | [] -> acc
        | a :: b -> rev (a :: acc) b
    in
    let rec inner count acc = function
        | []                                -> []
        | [e]                               -> rev [] (e :: string_of_int (count + 1) :: acc)
        | a :: (b :: _ as t) when a = b     -> inner (count + 1) acc t
        | a :: t                            -> inner 0 (a :: (string_of_int (count + 1)) :: acc) t
    in
    let rec strconcat = function
        | [] -> ""
        | a :: b -> a ^ (strconcat b)
    in
    let rec loop n lst =
        if n < 0 then ""
        else if n = 0 then strconcat lst
        else loop (n - 1) (inner 0 [] lst)
    in
    loop n ("1" :: [])

let () =
    let my_print a = Printf.printf "seq(%d) = %s\n" a (sequence a) in
    my_print (-3);
    my_print 0;
    my_print 1;
    my_print 2;
    my_print 3;
    my_print 4;
    my_print 5;
    my_print 6;
    my_print 7;
