open Odot
open Surface
open Internal
open Ir

type var_incl_policy = Exact | DepsTo | DepsOf

type filtering = {
  event_knowledge : bool Variable.Map.t;
  context : Context.Group.t option;
  variable_inclusion : (Variable.Set.t * var_incl_policy) option;
}

let no_filtering = {
  event_knowledge = Variable.Map.empty;
  context = None;
  variable_inclusion = None;
}

let label s =
  Simple_id "label", Some (Double_quoted_id s)

let tlabel s =
  Simple_id "taillabel", Some (Double_quoted_id s)

let color s =
  Simple_id "color", Some (Double_quoted_id s)

let shape s =
  Simple_id "shape", Some (Double_quoted_id s)

let add_var p g v =
  let is_actor = Variable.Map.mem v p.infos.Ast.actors in
  let ncolor =
    if is_actor then color "red" else color "blue"
  in
  let n =
    Double_quoted_id(Format.asprintf "%a"
                       (FormatIr.print_variable ~with_ctx:(not is_actor) p.infos) v),
    None
  in
  g.stmt_list <- Stmt_node (n,[ncolor])::g.stmt_list;
  n

let id = let c = ref 0 in fun () -> incr c; "anon"^(string_of_int !c)

let add_event p g v =
  let n = Double_quoted_id(id ()), None in
  g.stmt_list <-
    Stmt_node (n,
               [shape "box";
                label (Format.asprintf "%a" (FormatIr.print_variable ~with_ctx:false p.infos) v)
               ])
    ::g.stmt_list;
  n

let add_edge g s e ls =
  if s <> e then
  g.stmt_list <-
    Stmt_edge (Edge_node_id s, [Edge_node_id e],ls)
    ::g.stmt_list

let dot_of_redist (type a) ~filter p g (r : a Ir.RedistTree.redist) =
  let is_var_included v =
    match filter.variable_inclusion with
    | None -> true
    | Some (vars, _) -> Variable.Set.mem v vars
  in
  match r with
  | NoInfo -> []
  | Shares sh ->
    Variable.Map.fold (fun v f es ->
        if is_var_included v then
          let dest = add_var p g v in
          let attr = [ label (Format.asprintf "%a%%" R.pp_print R.(f * ~$100)) ] in
          (dest, attr)::es
        else es)
      sh []
  | Flats fs ->
    let es =
      Variable.Map.fold (fun dest s es ->
          if is_var_included dest then
            let dest = add_var p g dest in
            let attr = [ label (Format.asprintf "%a" (FormatIr.print_formula p.infos) s) ] in
            (dest, attr)::es
          else es)
        fs.transfers []
    in
    Variable.Map.fold (fun dest f es ->
        if is_var_included dest then
          let dest = add_var p g dest in
          let attr = [ label (Format.asprintf "deficit %a%%" R.pp_print R.(f * ~$100)) ] in
          (dest, attr)::es
        else es)
      fs.balances es

let rec dot_of_tree : type a. filter:filtering -> program -> graph -> a Ir.RedistTree.tree -> ((id * _ option) * attr list) list =
  fun ~filter p g t ->
  match Variable.BDT.cut filter.event_knowledge t with
  | NoAction -> []
  | Action r ->
    dot_of_redist ~filter p g r
  | Decision (evt, after, before) ->
    let e = add_event p g evt in
    let bf = dot_of_tree ~filter p g before in
    let af = dot_of_tree ~filter p g after in
    List.iter (fun (bn,l) -> add_edge g e bn ((tlabel "avant")::l)) bf;
    List.iter (fun (an,l) -> add_edge g e an ((tlabel "apres")::l)) af;
    [e, []]

let dot_of_trees ~filter p g ts =
  List.map (dot_of_tree ~filter p g) ts
  |> List.flatten

let dot_of_t ~filter p g src t =
  match (t : Ir.RedistTree.t) with
  | Fractions { base_shares; balance; branches } ->
    let balance_dot, balance_tree =
      match (balance : Ir.RedistTree.frac_balance) with
      | BalanceVars b ->
        begin match b.deficit with
          | None -> ()
          | Some v ->
            let v = add_var p g v in
            add_edge g v src [label "deficit"]
        end;
        begin match b.default with
          | None -> [], Variable.BDT.NoAction
          | Some v ->
            let v = add_var p g v in
            [v, [label "default"]], Variable.BDT.NoAction
        end
      | BalanceTree tree -> [], tree
    in
    let merged_tree =
      let mergef _ r1 r2 =
        match r1, r2 with
        | None, None -> None
        | None, r | r, None -> r
        | Some r1, Some r2 ->
          Some (Ir.RedistTree.merge_redist0 r1 r2)
      in
      List.fold_left (fun t tb ->
          Variable.BDT.merge mergef t
            (Variable.BDT.cut filter.event_knowledge tb))
        (Variable.BDT.merge mergef
           (Variable.BDT.Action base_shares)
           (Variable.BDT.cut filter.event_knowledge balance_tree))
        branches
    in
    dot_of_tree ~filter p g merged_tree @ balance_dot
  | Flat fs -> dot_of_trees ~filter p g fs

let graph_of_program p filter =
  let graph = {
    strict = false;
    kind = Digraph;
    id = None;
    stmt_list = [
      (* Stmt_attr (Attr_graph [Simple_id "ranksep", Some (Simple_id "0.8")]) *)
    ];
  }
  in
  let module Traversal = Graph.Traverse.Dfs(Variable.Graph) in
  let variable_inclusion =
    match filter.variable_inclusion with
    | None | Some (_, Exact) -> filter.variable_inclusion
    | Some (vars, DepsOf) ->
      let module GOP = Graph.Oper.P(Variable.Graph) in
      let rev_graph = GOP.mirror p.dep_graph in
      let vars =
        Variable.Set.fold (fun v vars ->
            if Variable.Graph.mem_vertex rev_graph v then
              Traversal.fold_component Variable.Set.add vars rev_graph v
            else vars)
          vars vars
      in
      Some (vars, Exact)
    | Some (vars, DepsTo) ->
      let vars =
        Variable.Set.fold (fun v vars ->
            if Variable.Graph.mem_vertex p.dep_graph v then
              Traversal.fold_component Variable.Set.add vars p.dep_graph v
            else vars)
          vars vars
      in
      Some (vars, Exact)
  in
  let filter = { filter with variable_inclusion } in
  Variable.Map.iter (fun v t ->
      let is_included =
        match filter.variable_inclusion with
        | None -> true
        | Some (vars, Exact) -> Variable.Set.mem v vars
        | _ -> assert false
      in
      let match_context =
        match filter.context with
        | None -> true
        | Some ctx ->
          let s = Variable.Map.find v p.infos.var_shapes in
          not (Context.is_empty_shape (Context.shape_overlap_subshape s ctx))
      in
      if is_included && match_context then begin
        let src = add_var p graph v in
        let es = dot_of_t ~filter p graph src t in
        List.iter (fun (e,a) ->
            add_edge graph src e a)
          es
      end)
    p.trees;
  graph

let dot_string_of_program p filter =
  string_of_graph @@ graph_of_program p filter

let dot_of_program p filter =
  print_file "graph.dot" @@ graph_of_program p filter
