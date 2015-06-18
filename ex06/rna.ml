(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   rna.ml                                             :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: roblabla </var/spool/mail/roblabla>        +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/18 13:18:01 by roblabla          #+#    #+#             *)
(*   Updated: 2015/06/18 15:53:34 by roblabla         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

type phosphate = string
type deoxyribose = string
type nucleobase = A | T | C | G | U | None
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
        | 'U' -> U
        | _   -> None
}

type helix = nucleotide list

let generate_helix n =
    let rec inner (acc : helix) n =
        if n = 0 then acc
        else          inner (generate_nucleotide (match (Random.int 4) with
            | 0 -> 'A'
            | 1 -> 'T'
            | 2 -> 'C'
            | 3 -> 'G'
            | _ -> 'n') :: acc) (n - 1)
    in
    inner [] n

let rec helix_to_string (h : helix) =
    let nucleotide_to_string m =
        (match m.nucleobase with
        | A -> "A"
        | T -> "T"
        | C -> "C"
        | G -> "G"
        | U -> "U"
        | None -> "?")
    in
    match h with
    | [] -> ""
    | a :: [] -> nucleotide_to_string a
    | a :: b -> nucleotide_to_string a ^ (helix_to_string b)

let complementary_helix (h: helix) : helix =
    let rec inner acc = function
        | [] -> acc
        | ({ nucleobase = A; _ } as n) :: b -> inner ({ n with nucleobase = T } :: acc) b
        | ({ nucleobase = T; _ } as n) :: b -> inner ({ n with nucleobase = A } :: acc) b
        | ({ nucleobase = C; _ } as n) :: b -> inner ({ n with nucleobase = G } :: acc) b
        | ({ nucleobase = G; _ } as n) :: b -> inner ({ n with nucleobase = C } :: acc) b
        | a :: b -> inner (a :: acc) b
    in
    let rec revlist acc = function
        | [] -> acc
        | a :: b -> revlist (a :: acc) b
    in
    revlist [] (inner [] h)

type rna = nucleobase list

let generate_rna (h: helix) : rna =
    let rec revlist acc = function
        | [] -> acc
        | a :: b -> revlist (a :: acc) b
    in
    let rec inner acc = function
        | [] -> acc
        | { nucleobase = T; _ } :: b -> inner (U :: acc) b
        | { nucleobase = n; _ } :: b -> inner (n :: acc) b
    in
    revlist [] (inner [] (complementary_helix h))

let () =
    let nucleobase_out m =
        print_string (match m with
        | A -> "A"
        | T -> "T"
        | C -> "C"
        | G -> "G"
        | U -> "U"
        | None -> "?")
    in
    Random.self_init ();
    let x = generate_helix 10 in
    print_endline (helix_to_string x);
    List.iter nucleobase_out (generate_rna x);
    print_char '\n';
