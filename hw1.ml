(*1.returns true iff a is a subset of b*)
let rec subset a b = match a with
	|[] -> true
	| h1::t1 -> List.mem h1 b  && subset t1 b;;

(*2.returns true iff a and b are equal*)
let equal_sets a b =
	if subset a b && subset b a 
	then true
	else false;;

(*3.returns a list representing union of a b.*)
let rec set_union a b = match a with
	| [] -> b
	| h1::t1 -> if List.mem h1 b
	then set_union t1 b
	else h1::set_union t1 b;;

(*4.returns a list representing a∩b*)
let rec set_intersection a b = match a with
	| [] -> []
	| h1::t1 -> if List.mem h1 b
	then h1::set_intersection t1 b
	else set_intersection t1 b;;

(*5.returns a list representing a−b.*)
let rec set_diff a b = match a with
	|[] -> []
	| h1::t1 -> if List.mem h1 b
	then set_diff t1 b
	else h1::set_diff t1 b;;

(*6*)
let rec computed_fixed_point eq f x =
	if eq (f x) x
	then x
	else
	computed_fixed_point eq f (f x);;

(*7*)
(*HELPER FUNCTION*)
let rec calculate f p x = 
	if p > 0
	then calculate f (p-1) (f x)
	else x;;

let rec computed_periodic_point eq f p x =
	if eq (calculate f p x) x
	then x
	else computed_periodic_point eq f p (f x);;

(*8*)
let rec while_away s p x =
	if p x
	then x:: while_away s p (s x)
	else [];;

(*9*)
(*HELPER FUNCTION*)
let rec expand num ele = 
	if num = 0
	then []
	else ele::expand (num-1) ele;;

let rec rle_decode lp = match lp with
	| [] -> []
	| (num, ele)::t1 -> expand num ele @ rle_decode t1;;

(*10. filter blind alley, removes all blind alley rules, along with preserving the same order. *)

type ('non, 'term) symbol =
	| N of 'non
	| T of 'term;;

let is_term rule = match rule with
	| T e -> true
	| N e -> false;;

let remove_NT expr = match expr with
	| T e -> e
	| N e -> e;;

(*check individual rule with the expr of the term list, 
error is located where as I can't seem to compare an element 
from the terminal list and the element of the rule*)

let rec in_termlist check list = match list with
	| [] -> false
	| hd :: tl -> if remove_NT check = hd
	then true
	else in_termlist check tl;;

(*check each of the terms in the rule to see if it is a valid expression*)
let rec check_nonterms rule terminallist = match rule with
	| [] -> true
	| hd::tl -> if is_term hd || in_termlist hd terminallist
	then check_nonterms tl terminallist
	else false;;

(*helper for create_term_list, does the actual check if it has 'T' in rule*)
let rec is_term_in_rule rule = match rule with
	| [] -> true
	| hd::tl -> if is_term hd
	then is_term_in_rule tl
	else false;;

(*creates a list of rules with 'T' included*)
let rec create_term_list initial = match initial with
	| [] -> []
	| (expr, rule) :: tl -> if is_term_in_rule rule
	then expr :: create_term_list tl
	else create_term_list tl;;

(*deletes the rules by comparing it to the term list*)
let rec del_rules rules termlist = match rules with
	| [] -> []
	| (expr, rule)::tl -> if check_nonterms rule termlist
	then (del_rules tl termlist) @ [(expr, rule)]
	else del_rules tl termlist;;

let filter_blind_alleys = function
	| (expr, rules) -> (expr, del_rules rules (create_term_list rules));;

