(* WARM UP *)
type ('a, 'b) symbol =
	| N of 'a
	| T of 'b
;;

let rec combine_lhs rules non_term = match rules with 
	| [] -> []
	| (expr, subrules)::tl -> if expr = non_term
		then subrules :: (combine_lhs tl non_term)	
		else combine_lhs tl non_term
;;

let convert_grammar old_grammar = match old_grammar with
	| (start, rules) -> (start, (combine_lhs rules)) 
;;

(*PARSE PREFIX GRAMMAR*)

let rec match_start expr rules rhs acceptor derivation fragment = match rhs with
	| [] -> None
	| rhs_hd :: rhs_tl -> match match_rule rhs_hd rules acceptor (derivation @ [(expr,rhs_hd)]) fragment with
		| Some x -> Some x  
		| None -> match_start expr rules rhs_tl acceptor derivation fragment	


and match_rule subrule allrules acceptor derivation fragment = match subrule with
	| [] -> acceptor derivation fragment
	| subrule_hd :: subrule_tl -> match fragment with 
		| [] -> None
		| fragment_hd :: fragment_tl -> match subrule_hd with 
			(*if nonterminal, throw it to match_start to piece it 
				together until it reaches terminal*)
			| N expr -> match_start expr allrules (allrules expr) (match_rule subrule_tl allrules acceptor) derivation fragment
			| T expr -> if fragment_hd = expr
				then match_rule subrule_tl allrules acceptor derivation fragment_tl
				else None
;;
 
let parse_prefix grammar acceptor fragment = match grammar with
	| (expr, rules) -> match_start expr rules (rules expr) acceptor [] fragment 
;;
# CS131
