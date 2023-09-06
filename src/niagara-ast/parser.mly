%{
(*****************************************************************************)
(*                                                                           *)
(*  Copyright (c) 2023 OCamlPro SAS                                          *)
(*                                                                           *)
(* All rights reserved.                                                      *)
(* This source code is licensed under the GNU Affero General Public License  *)
(* version 3 found in the LICENSE.md file in the root directory of this      *)
(* source tree.                                                              *)
(*                                                                           *)
(*****************************************************************************)

open Ast

let pos (start, stop) = Pos.make ~start ~stop
%}

%token OPERATION QUOTEPART BONUS SUR ASSIETTE ANS MOIS ENTREE LBRA RBRA DEFAUT
%token CONTEXTUALISEE PAR TYPE ENTIER RATIONNEL ARGENT TOTAL COURANT
%token ACTEUR POUR EVENEMENT ET OU AVANT APRES QUAND CONTEXTE TOUT CONSTANTE
%token LPAR RPAR VERS ATTEINT PLUS MINUS MULT DIV EQ COLON EOF DEFICIT
%token AVANCE MONTANT COMMA RETROCESSION // OPPOSABLE SECTION FIN
%token<float> FLOAT
%token<int> INT MONEY
%token<string> LIDENT UIDENT LABEL
%token<Date.Date.t> DATE

%nonassoc LIDENT
%nonassoc LPAR

%left PLUS MINUS OU
%left MULT DIV ET
%nonassoc TOTAL COURANT

%on_error_reduce literal formula named actor

%start<Ast.source Ast.program> program

%%

// Dispatch

operation:
| OPERATION op_label = LABEL op_default_dest = destinataire?
    op_context = op_context* op_source = source
    exprs = expression
{
  operation_decl
    ~loc:(pos $sloc)
    op_label
    ?default_dest:op_default_dest
    ~context:op_context
    ~source:op_source
    ~guarded_redistrib:exprs
}

advance:
| AVANCE adv_label = LABEL SUR adv_output = holder PAR
    adv_provider = actor MONTANT adv_amount = formula
  {{
      adv_label;
      adv_output;
      adv_provider;
      adv_amount;
  }}

simple_expr:
| QUOTEPART f = formula d = destinataire? {
  redistribution ~loc:f.formula_loc (Part f), d
}
| BONUS f = formula d = destinataire? {
  redistribution ~loc:f.formula_loc (Flat f), d
}
| RETROCESSION f = formula SUR h = holder d = destinataire?
  { let start, stop = $startpos(f), $endpos(h) in
    redistribution ~loc:(Pos.make ~start ~stop) (Retrocession (f, h)), d
  }

simple_exprs:
| es = simple_expr+ { Redists (List.map (fun (e, d) -> WithHolder (e, d)) es) }

sub_expression:
| es = simple_exprs { es }
| LPAR es = expression RPAR { es }

expression:
| es = simple_exprs { es }
| es = branch_expr { Branches { befores = fst es; afters = snd es } }
| es = when_expr+ { Whens es }

branch_expr:
| ae = after_expr+ { ([], ae) }
| be = before_expr+ ae = after_expr* { (be, ae) }

after_expr:
| APRES g = event_expr se = sub_expression { (g, se) }

before_expr:
| AVANT g = event_expr se = sub_expression { (g, se) }

when_expr:
| QUAND g = event_expr se = sub_expression { (g, se) }

source:
| SUR p = pool { p }
| PAR a = actor { holder ~loc:(pos $sloc) (Actor a) }

// Formula

formula:
| fd = formula_desc { formula ~loc:(pos $sloc) fd }
| LPAR f = formula RPAR { f }
;

formula_desc:
| l = literal { Literal l }
| n = named { Named n }
| f1 = formula op = binop f2 = formula { Binop(op, f1, f2) }
| f = formula COURANT { Instant f }
| f = formula TOTAL { Total f }
;

%inline binop:
| PLUS { Add }
| MINUS { Sub }
| MULT { Mult }
| DIV { Div }

%inline comp:
| EQ { Eq }

named:
| n = LIDENT c = ioption(context_refinement)
    { let ctx = Option.fold c ~none:[] ~some:(fun c -> c) in
      named ~loc:(pos $sloc) (Name (n, ctx))
    }
| a = labeled_actor {
  let loc = pos $sloc in
  named ~loc (Holder (holder ~loc (Actor a)))
}
| p = pool { named ~loc:(pos $sloc) (Holder p) }

holder:
| a = actor { holder ~loc:(pos $sloc) (Actor a) }
| p = pool { p }

actor:
| a = actor_desc; {actor ~loc:(pos $sloc) a}
;

actor_desc:
| a = LIDENT { PlainActor a }
| la = labeled_actor_desc { la }
;

labeled_actor:
| a = labeled_actor_desc; {actor ~loc:(pos $sloc) a}
;

labeled_actor_desc:
| a = LIDENT LBRA l = LIDENT RBRA { LabeledActor(a, l) }
;

pool:
| ASSIETTE p = LIDENT c = ioption(context_refinement)
    { let ctx = Option.fold c ~none:[] ~some:(fun c -> c) in
      holder ~loc:(pos $sloc) (Pool(p, ctx))
    }

context_refinement:
| LPAR c = separated_nonempty_list(COMMA,context_refine_item) RPAR { c }

context_refine_item:
| crid = context_refinement_item_desc {
  context_refinement_item ~loc:(pos $sloc) crid
}
;

context_refinement_item_desc:
| c = UIDENT { CCase c }
| TOUT ct = UIDENT { CFullDomain ct }
;

literal:
| i = INT { LitInt i }
| r = FLOAT { LitRational r }
| m = MONEY { LitMoney m }
| d = duration { LitDuration d }
| d = DATE { LitDate d }

duration:
| d = duration_year { d }
| d = duration_month { d }

duration_year:
| i = INT ANS m = duration_month?
 { let y = Date.Duration.year i in
   match m with
   | None -> y
   | Some m -> Date.Duration.add y m
 }

duration_month:
| i = INT MOIS { Date.Duration.month i }

// Flow and IO

input_decl:
| ENTREE input_name = LIDENT input_type = input_type input_context = loption(input_context_decl)
{
  input_decl
    ~loc:(pos $sloc)
    ~context:input_context
    ~typ:input_type
    ~kind:ReadOnly
    input_name
}
| ENTREE ASSIETTE input_name = LIDENT input_context = loption(input_context_decl)
{
  input_decl
    ~loc:(pos $sloc)
    ~context:input_context
    ~typ:TMoney
    ~kind:Attributable
    input_name
 }

input_context_decl:
| CONTEXTUALISEE PAR ctx = input_context_list { ctx }

input_context_list:
| cs = input_context { [cs] }
| css = nonempty_list(preceded(MINUS,input_context)) { css }

input_context:
| cs = nonempty_list(context) { cs }

input_type:
| TYPE t = base_type { t }

base_type:
| ENTIER { TInteger }
| RATIONNEL { TRational }
| ARGENT { TMoney }

actor_decl:
| ACTEUR id = LIDENT { actor_decl ~loc:(pos $sloc) id }

destinataire:
| VERS d = holder { d }

/* destinataire: */
/* | VERS d = holder { d, NoOpposition } */
/* | VERS d = holder o = opposition { d, o } */

/* opposition: */
/* | OPPOSABLE f = formula POUR a = actor { Opposable (f, a) } */

// Event

event_decl:
| EVENEMENT event_name = LIDENT ATTEINT QUAND event_expr = event_expr
 { event_decl ~loc:(pos $sloc) event_name event_expr }

event_expr:
| eed = event_expr_desc { event_expr ~loc:(pos $sloc) eed }
;

event_expr_desc:
| EVENEMENT id = LIDENT { EventId id }
| f1 = formula c = comp f2 = formula { EventComp(c, f1, f2) }
| e1 = event_expr ET e2 = event_expr { EventConj(e1, e2) }
| e1 = event_expr OU e2 = event_expr { EventDisj(e1, e2) }
;
// Context

context_decl:
| CONTEXTE context_type_name = UIDENT COLON context_type_cases = context_case+
 {{ context_type_name; context_type_cases }}

context_case:
| MINUS id = UIDENT { id }

context:
| tid = UIDENT LPAR cases = separated_nonempty_list(COMMA,UIDENT) RPAR { Cases(tid, cases) }
| TOUT id = UIDENT { Forall id }

op_context:
| POUR c = context { c }

// Program

constant_decl:
| CONSTANTE const_name = LIDENT COLON const_value = literal
 {{ const_name; const_value }}

default_decl:
| DEFAUT SUR src = pool VERS dst = holder { src, dst }

deficit_decl:
| DEFICIT SUR src = pool PAR def = holder { src, def }

/* section: */
/* | SECTION section_name = UIDENT section_context = context* */
/*   section_guards = event_guard* section_decl = toplevel_decl* FIN */
/*  {{ */
/*    section_name; */
/*    section_context; */
/*    section_guards; */
/*    section_decl; */
/*  }} */

toplevel_decl:
| o = operation { DHolderOperation o }
| e = event_decl { DHolderEvent e }
| c = constant_decl { DConstant c }
| c = context_decl { DContext c }
| i = input_decl { DInput i }
| o = actor_decl { DActor o }
| d = default_decl
  { let (default_source, default_dest) = d in
    DHolderDefault { default_source; default_dest }
  }
| d = deficit_decl
  { let (deficit_pool, deficit_provider) = d in
    DHolderDeficit { deficit_pool; deficit_provider } }
| a = advance { DHolderAdvance a }
/* | s = section { DSection s } */

program: d = toplevel_decl* EOF { Source d }
