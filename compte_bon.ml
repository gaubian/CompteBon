type lex = 	| Nb of int
		| Add of lex * lex
		| Sub of lex * lex
		| Mul of lex * lex
		| Div of lex * lex;;

let add c d = match (c,d) with
	|(n,a),(m,b)	-> (n+m,Add(a,b));;


let sub c d = match (c,d) with
	|(n,a),(m,b)	-> if n > m
				then (n-m,Sub(a,b))
				else (m-n,Sub(b,a));;

let mul c d = match (c,d) with
	|(n,a),(m,b)	-> (n * m,Mul(a,b));;


let div c d = match (c,d) with
	|(n,a),(m,b)	-> (n / m,Div(a,b));;

let divi a b = (((fst a) * (fst b) != 0) && (fst a) mod (fst b) = 0,((fst a) * (fst b) != 0) && (fst b) mod (fst a) = 0);;

let suiv a b l = match (divi a b) with
	|true, false	-> [(add a b)::l;(sub a b)::l;(mul a b)::l;(div a b)::l]
	|false, true	-> [(add a b)::l;(sub a b)::l;(mul a b)::l;(div b a)::l]
	|_		-> [(add a b)::l;(sub a b)::l;(mul a b)::l]

let rec f = function
	|[a;b]		-> suiv a b []
	|[a]		-> [[a]]
	|[]		-> []
	| t::q	-> (List.map (fun i -> t::i) (f q)) @
			(match q with
				|[a;b]		-> (suiv t a [b])@(suiv t b [a])
				|[a;b;c]	->(suiv t a [b;c])@(suiv t b [a;c])@(suiv t c [a;b])
				|[a;b;c;d]	->(suiv t a [b;c;d])@(suiv t b [a;c;d])@(suiv t c [a;b;d])@(suiv t d [a;b;c])
				|[a;b;c;d;e]	->(suiv t a [b;c;d;e])@(suiv t b [a;c;d;e])@(suiv t c [a;b;d;e])
						 @(suiv t d [a;b;c;e])@(suiv t e [a;b;c;d])
			);;

let simplifie liste =
	let rec aux = function
		| l::l'::q 	-> if List.map (fun (a,_) -> a) l =  List.map (fun (a,_) -> a) l'
					then aux (l::q)
					else l::(aux (l'::q))
		| l		-> l
	in aux (List.sort (fun l l' -> compare (List.map (fun (a,_) -> a) l) (List.map (fun (a,_) -> a) l')) liste);;

let compte but liste =
	let l = ref [List.map (fun i -> (i,Nb(i))) liste] and flag = ref (Nb(-1)) in
	while !flag = Nb(-1) do
		l := simplifie !l;
		List.iter (fun i -> List.iter (fun (a,b) -> if a=but then flag := b) i) !l;
		l := List.flatten (List.map f !l);
	done;
	!flag;;
	
let rec transcrit = function
	| Add(a,b) 	-> "(" ^ (transcrit a) ^ "+" ^ (transcrit b) ^ ")"
	| Sub(a,b) 	-> "(" ^ (transcrit a) ^ "-" ^ (transcrit b) ^ ")"
	| Mul(a,b) 	-> "(" ^ (transcrit a) ^ "*" ^ (transcrit b) ^ ")"
	| Div(a,b) 	-> "(" ^ (transcrit a) ^ "/" ^ (transcrit b) ^ ")"
	| Nb(a)		-> string_of_int a;;

let rec ecrire oc = function
	| Add(a,b) 	-> (let c = ecrire oc a and d = ecrire oc b in Printf.fprintf oc "%d\t= %d\t+ %d\n" (c+d) c d; c+d)
	| Sub(a,b) 	-> (let c = ecrire oc a and d = ecrire oc b in Printf.fprintf oc "%d\t= %d\t- %d\n" (c-d) c d; c-d)
	| Mul(a,b) 	-> (let c = ecrire oc a and d = ecrire oc b in Printf.fprintf oc "%d\t= %d\t* %d\n" (c*d) c d; c*d)
	| Div(a,b) 	-> (let c = ecrire oc a and d = ecrire oc b in Printf.fprintf oc "%d\t= %d\t/ %d\n" (c/d) c d; c/d)
	| Nb(a)		-> a;;
	
let final entree sortie =
	let ic = open_in entree and l = ref [] in
	let resultat = Scanf.fscanf ic "%d " (fun j -> j) in
		for i=1 to 6 do
			l := (Scanf.fscanf ic "%d " (fun j -> j))::!l;
		done;
	let oc = open_out sortie in
		ecrire oc (compte resultat !l);
		(* Printf.fprintf oc "%s " (transcrit (compte resultat !l)); *)
		flush oc;;

final "/home/guilaub/Documents/entree" "/home/guilaub/Documents/sortie";;
