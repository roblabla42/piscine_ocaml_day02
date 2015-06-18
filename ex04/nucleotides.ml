(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   nucleotides.ml                                     :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: roblabla </var/spool/mail/roblabla>        +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/18 09:37:57 by roblabla          #+#    #+#             *)
(*   Updated: 2015/06/18 10:06:41 by roblabla         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

type phosphate = string
type deoxyribose = string
type nucleobase = A | T | C | G | None
type nucleotide = {
    phosphate: phosphate;
    deoxyribose: deoxyribose;
    nucleobase: nucleobase;
}

let generate_nucleotide n = {
    phosphate = "phosphate";
    deoxyribose = "deoxyribose";
    nucleobase = match n with
        | 'A' -> A
        | 'T' -> T
        | 'C' -> C
        | 'G' -> G
        | _   -> None
}

let () =
    let my_print n =
        let m = generate_nucleotide n in
        Printf.printf "DNA : %s %s %c\n" m.phosphate m.deoxyribose
        (match m.nucleobase with
        | A -> 'A'
        | T -> 'T'
        | C -> 'C'
        | G -> 'G'
        | None -> '?')
    in
    my_print 'T';
    my_print 'A';
    my_print 'G';
    my_print 'C';
    my_print 'e';
    my_print 'n';
