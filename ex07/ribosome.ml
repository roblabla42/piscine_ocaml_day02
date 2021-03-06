(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ribosome.ml                                        :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: roblabla </var/spool/mail/roblabla>        +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/18 15:42:11 by roblabla          #+#    #+#             *)
(*   Updated: 2015/07/01 13:40:21 by roblabla         ###   ########.fr       *)
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
        match m.nucleobase with
        | A -> "A"
        | T -> "T"
        | C -> "C"
        | G -> "G"
        | U -> "U"
        | None -> "?"
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

let generate_bases_triplets (rna: rna) =
    let rec revlist acc = function
        | [] -> acc
        | a :: b -> revlist (a :: acc) b
    in
    let rec inner acc = function
        | a :: b :: c :: d -> inner ((a, b, c) :: acc) d
        | _ -> revlist [] acc
    in
    inner [] rna

type aminoacid = Stop | Ala | Arg | Asn | Asp | Cys | Gln | Glu | Gly | His
    | Ile | Leu | Lys | Met | Phe | Pro | Ser | Thr | Trp | Tyr | Val

type protein = aminoacid list

let rec string_of_protein n =
    let aminoacid_to_str = function
        | Stop -> "End of translation"
        | Ala -> "Alanine"
        | Arg -> "Arginine"
        | Asn -> "Asparagine"
        | Asp -> "Aspartique"
        | Cys -> "Cysteine"
        | Gln -> "Glutamine"
        | Glu -> "Glutamique"
        | Gly -> "Glycine"
        | His -> "Histidine"
        | Ile -> "isoleucine"
        | Leu -> "Leucine"
        | Lys -> "Lysine"
        | Met -> "Methionine"
        | Phe -> "Phenylalanine"
        | Pro -> "Proline"
        | Ser -> "Serine"
        | Thr -> "Threonine"
        | Trp -> "Tryptophane"
        | Tyr -> "Tyrosine"
        | Val -> "Valine"
    in
    match n with
        | a :: b -> (aminoacid_to_str a) ^ " " ^ (string_of_protein b)
        | [] -> ""

let decode_arn (n: rna) : protein =
    let rec inner = function
        | [] -> []
        | (U, A, A) :: b | (U, A, G) :: b | (U, G, A) :: b -> Stop :: []
        | (G, C, A) :: b | (G, C, C) :: b | (G, C, U) :: b -> Ala :: (inner b)
        | (A, G, A) :: b | (A, G, G) :: b | (C, G, A) :: b | (C, G, C) :: b | (C, G, G) :: b | (C, G, U) :: b -> Arg :: (inner b)
        | (A, A, C) :: b | (A, A, U) :: b -> Asn :: (inner b)
        | (G, A, C) :: b | (G, A, U) :: b -> Asp :: (inner b)
        | (U, G, C) :: b | (U, G, U) :: b -> Cys :: (inner b)
        | (C, A, A) :: b | (C, A, G) :: b -> Gln :: (inner b)
        | (G, A, A) :: b | (G, A, G) :: b -> Glu :: (inner b)
        | (G, G, A) :: b | (G, G, C) :: b | (G, G, G) :: b | (G, G, U) :: b -> Gly :: (inner b)
        | (C, A, C) :: b | (C, A, U) :: b -> His :: (inner b)
        | (A, U, A) :: b | (A, U, C) :: b | (A, U, U) :: b -> Ile :: (inner b)
        | (C, U, A) :: b | (C, U, C) :: b | (C, U, G) :: b | (C, U, U) :: b | (U, U, A) :: b | (U, U, G) :: b -> Leu :: (inner b)
        | (A, A, A) :: b | (A, A, G) :: b -> Lys :: (inner b)
        | (A, U, G) :: b -> Met :: (inner b)
        | (U, U, C) :: b | (U, U, U) :: b -> Phe :: (inner b)
        | (C, C, C) :: b | (C, C, A) :: b | (C, C, G) :: b | (C, C, U) :: b -> Pro :: (inner b)
        | (U, C, A) :: b | (U, C, C) :: b | (U, C, G) :: b | (U, C, U) :: b | (A, G, U) :: b | (A, G, C) :: b -> Ser :: (inner b)
        | (A, C, A) :: b | (A, C, C) :: b | (A, C, G) :: b | (A, C, U) :: b -> Thr :: (inner b)
        | (U, G, G) :: b -> Trp :: (inner b)
        | (U, A, C) :: b | (U, A, U) :: b -> Tyr :: (inner b)
        | (G, U, A) :: b | (G, U, C) :: b | (G, U, G) :: b | (G, U, U) :: b -> Val :: (inner b)
        | _ -> []
    in
    inner (generate_bases_triplets n)

let () =
    Random.self_init ();
    let nucleobase_out m =
        (match m with
        | A -> "A"
        | T -> "T"
        | C -> "C"
        | G -> "G"
        | U -> "U"
        | None -> "?")
    in
    let rec triplets_out = function
        | [] -> ()
        | (a, b, c) :: d -> (Printf.printf "(%s, %s, %s) " (nucleobase_out a) (nucleobase_out b) (nucleobase_out c); triplets_out d) in
    let x = generate_helix 100 in
    print_endline (helix_to_string x);
    List.iter (function x -> print_string (nucleobase_out x)) (generate_rna x);
    print_char '\n';
    triplets_out (generate_bases_triplets (generate_rna x));
    print_endline "===";
    print_endline (string_of_protein (decode_arn (generate_rna x)))
