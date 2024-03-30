open Lexeur

type op = Plus | Moins | Mult | Div

type exp_arith =
 | Cst of int
 | Var of string
 | ABin of op * exp_arith * exp_arith

type comp = Egal | Inferieur
type log_op = Et | Ou

type exp_log =
 | Vrai | Faux
 | Comparaison of comp * exp_arith * exp_arith
 | LBin of log_op * exp_log * exp_log
 | Non of exp_log

type program =
 | Rien
 | Affectation of string * exp_arith
 | Sequence of program list
 | Si of exp_log * program * program
 | TantQue of exp_log * program

let rec parse_A l = 
  match l with 
    | LCst(i)::q -> (Cst(i), q)
    | LVar(s)::q -> (Var(s), q)
    | LPlus::q   -> let (e1, q') = parse_A q in
                    let (e2, q'') = parse_A q' in
                    (ABin(Plus, e1, e2), q'')
    | LMoins::q  -> let (e1, q') = parse_A q in
                    let (e2, q'') = parse_A q' in
                    (ABin(Moins, e1, e2), q'')
    | LFois::q   -> let (e1, q') = parse_A q in
                    let (e2, q'') = parse_A q' in
                    (ABin(Mult, e1, e2), q'')
    | LDiv::q    -> let (e1, q') = parse_A q in
                    let (e2, q'') = parse_A q' in
                    (ABin(Div, e1, e2), q'')
    | _ -> failwith "Erreur parsing, expression arithmétique attendue"

and parse_B l = 
  match l with 
    | LVrai::q      ->  (Vrai, q)
    | LFaux::q      ->  (Faux, q)
    | LOu::q        ->  let (e1, q') = parse_B q in
                        let (e2, q'') = parse_B q' in
                        (LBin(Ou, e1, e2), q'')
    | LEt::q        ->  let (e1, q') = parse_B q in
                        let (e2, q'') = parse_B q' in
                        (LBin(Et, e1, e2), q'')
    | LNon::q       ->  let (e1, q') = parse_B q in
                        (Non(e1), q')
    | LEgal::q      ->  (
        try
          match parse_A q with
            | (e1, q') -> let (e2, q'') = parse_A q' in 
                          (Comparaison(Egal, e1, e2), q'')
        with e -> failwith "Erreur parsing, expression arithmétique/booléenne attendue"
    )
    | LInferieur::q ->  let (e1, q') = parse_A q in
                        let (e2, q'') = parse_A q' in 
                        (Comparaison(Inferieur, e1, e2), q'')
    | _ -> failwith "Erreur parsing, expression booléenne attendue"

and parse_P l = 
  match l with 
    | LVar(s)::q      ->  (
            match q with
              | LAffectation::q' -> (
                  let e, l = parse_A q' in 
                  match parse_E l with 
                    | (Rien, _) -> (Affectation(s, e), l)
                    | (e2, l') -> (Sequence([Affectation(s,e); e2]), l')
              )
              | _ -> failwith "Erreur parsing, affectation attendue"

    )
    | LRien::q        ->  (Rien, q)
    | LSi::q          ->  (
            let (b, q') = parse_B q in
            match q' with
              | LAlors::q' ->
                (
                  let e1, q' = parse_P q' in
                  match q' with 
                    | LSinon::q' -> (
                      let e2, q' = parse_P q' in
                      match q' with 
                        | LFinSi::l -> (
                          match parse_E l with 
                            | (Rien, _) -> (Si(b, e1, e2), l)
                            | (e, l) -> (Sequence([Si(b, e1, e2); e]), l)
                        )
                        | _ -> failwith "Erreur parsing, fin si attendue"
                    )
                    | _ -> failwith "Erreur parsing, sinon attendu" 
                )
              | _ -> failwith "Erreur parsing, alors attendu"
    )
    | LTantQue::q     -> (
            let (b, q') = parse_B q in
            match q' with
              | LFaire::q'' ->
                (
                  let body, q''' = parse_P q'' in
                  match q''' with 
                    | LFinTq::l-> (
                        match parse_E l with 
                              | (Rien, _) -> (TantQue(b, body), l)
                              | (e, l) -> (Sequence([TantQue(b, body); e]), l)
                      )
                    | _ -> failwith "Erreur parsing, fin boucle attendue"
                ) 
              | _ -> failwith "Erreur parsing, début boule attendu"
    )
    | _ -> failwith "Expression manquante"

and parse_E l = 
  match l with 
    | LPointVirgule::q -> let (e1, q') = parse_P q in
                          (e1, q')
    | _::q             -> (Rien, q)
    | _ -> (Rien, [])

let parse (filename: string): program =
  let lexems = analyse_fichier filename in
  let (prgm, q) = parse_P lexems in
  if q <> [] then failwith "Erreur parsing: Fin de fichier attendue."
  else prgm

let rec ajoute_no_doublons (e: 'a) (l: 'a list): 'a list =
  match l with 
    | [] -> [e]
    | h::q -> if h = e then l else h::(ajoute_no_doublons e q)

let rec vars (p: program): string list = 
  match p with 
    | Sequence(p)       -> seq p
    | Affectation(s, _) -> [s]
    | Si(_, p1, p2)     -> (vars p1)@(vars p2)
    | TantQue(_, p)     -> (vars p)
    | Rien              -> []

and seq (l: program list): string list = 
  match l with 
    | Sequence(p)::q        -> (seq p)@(seq q)
    | Affectation(s, _)::q  -> ajoute_no_doublons s (seq q)
    | Si(_, p1, p2)::q      -> (vars p1)@(vars p2)@(seq q)
    | TantQue(_, p)::q      -> (vars p)@(seq q)
    | Rien::q               -> seq q   
    | []                    -> []

let rec arith_to_c (e: exp_arith): string =
  match e with
    | Cst(i)          -> string_of_int i
    | Var(s)          -> s
    | ABin(e, e1, e2) -> "("^(arith_to_c e1)^(
                          match e with 
                            | Plus  -> " + "
                            | Moins -> " - "
                            | Mult  -> " * "
                            | Div   -> " / "
                          )^(arith_to_c e2)^")"

let rec log_to_c (e: exp_log): string = 
  match e with 
    | Vrai -> "true"
    | Faux -> "false"
    | Comparaison(c, e1, e2) -> (
        match c with 
          | Egal -> (arith_to_c e1)^" == "^(arith_to_c e2)
          | Inferieur -> (arith_to_c e1)^" < "^(arith_to_c e2)
    )
    | LBin(c, e1, e2) -> (
        match c with 
          | Et -> (log_to_c e1)^" && "^(log_to_c e2)
          | Ou -> (log_to_c e1)^" || "^(log_to_c e2)
    )
    | Non(e) -> "!("^(log_to_c e)^")"

let prog_to_c (p: program): string =
  let rec indent i = 
    if i = 0 then ""
    else "  "^(indent (i-1))
  in
  let rec prog_indent i p = 
    match p with 
      | Sequence(l)       -> (
          let rec aux l = match l with 
            | [] -> ""
            | h::[] -> (prog_indent i h)
            | h::q -> (prog_indent i h)^";\n"^(aux q)
          in (aux l)
      )
      | Affectation(s, e) -> (indent i)^s^" = "^(arith_to_c e)
      | Si(c, p1, p2)     -> (indent i)^"if ("^(log_to_c c)^
                "){\n"^(prog_indent (i+1) p1)^"\n} else {\n"^(prog_indent (i+1) p2)^"}"
      | TantQue(c, p)     -> (indent i)^"while ("^(log_to_c c)^"){\n"^(prog_indent (i+1) p)^";\n"^(indent i)^"}"
      | Rien              -> ""
  in prog_indent 1 p

let compile_numerix (p: program): string = 
  let s = "#include <stdlib.h>\n#include <stdio.h>\n\nint main() {" in
  let rec var_to_def l = match l with 
    | h::[] -> "  int "^h^";\n"
    | h::q  -> "  int "^h^";\n"^(var_to_def q)
    | _     -> ""
  in 
  let variables = var_to_def (vars p) in
  let body = prog_to_c p in

  let rec var_to_print l = match l with 
    | h::[] -> "  printf(\""^h^" = %d\\n\", "^h^");\n"
    | h::q  -> "  printf(\""^h^" = %d\\n\", "^h^");\n"^(var_to_print q)
    | _     -> ""
  in 

  s^"\n"^variables^"\n"^body^"\n\n"^(var_to_print (vars p))^"  return 0;\n}"

let compilation (infile: string) (outfile: string): unit =
  let program = parse infile in
  let code = compile_numerix program in
  let oc = open_out outfile in
  Printf.fprintf oc "%s" code;
  close_out oc

let _ = 
  compilation "ex1.num" "ex1.c"
