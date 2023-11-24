
(* identifiers *)
type domain = int
type case = int

module CaseSet = IntSet
module CaseMap = IntMap
module DomainMap = IntMap

module Group = struct

  (* Groups are encoded with bitvectors. This makes for an efficient way to
     manipulate them during the context analysis. The size of the bitvector the
     number of point in the entire context space, which means it will can ramp
     up really fast with a high number of context domains.

     For a context space defined by the domains d_i, 1<=i<=n, n the number of
     domains, and c_i the number of cases of d_i, the actual size would be the
     product of all c_i.

     Contexts being, for our current use cases, something that should fit in the
     humain brain, we should not reach the point were this implementation become
     untractable. And if it appears that it does, this module internals needs
     only to be known be this file, so complitely rework the implementation
     should not come at un unfair price.
  *)

  type t = int (* a type with bitwise operators *)

  (* For now, 63 elements sets are enough. We could extend this using an array
     if the need arise. *)
  let max_val = Sys.int_size

  module Map = IntMap
  module Set = IntSet

  let empty = 0

  let everything_up_to n = (1 lsl (n+1)) - 1

  let is_empty g = g = 0

  (* Set operations *)

  let add n g =
    if n > max_val then
      Errors.raise_error "Added value exceeds group capacity";
    g lor (1 lsl n)

  let union = (lor)

  let inter = (land)

  let equal = (=)

  let diff g1 g2 = g1 land (lnot g2)

  (* Clipping refers here to the operation of computing the maximum common sets.
     Think Venn diagrams.
  *)
  type clip_result = { only_left : t; common : t; only_right : t }

  let clip g1 g2 =
    { only_left = diff g1 g2;
      common = inter g1 g2;
      only_right = diff g2 g1;
    }

  (* Select bits of a set corresponding to the given pattern description. The
     pattern is defined with a starting offset, a length of consecutive bits to
     select, and a period on which the pattern repeats.

     Examples:
     [select g 1 1 4], with [g] of size 16, will have the mask [0010001000100010]
     [select g 0 2 8], [0000001100000011]
  *)
  let select g (offset : int) (length : int) (period : int) =
    let rec aux off len acc =
      if off + len > max_val then acc else
        let acc = add off acc in
        if len = 0
        then aux (off-length+period+1) (length -1) acc
        else aux (off+1) (len-1) acc
    in
    let mask = aux offset (length -1) 0 in
    inter g mask

end

type domain_info = {
  domain_name : string;
  domain_cases : CaseSet.t;
  domain_case_size : int; (* Number of bits to reach next case *)
  domain_period : int; (* Number of bits to loop on the domain cases.
                          Equals domain_case_size * size(domain_cases) *)
}

type case_info = {
  case_name : string;
  case_domain : domain;
}

type group_desc = CaseSet.t DomainMap.t list

type world = {
  domains : domain_info DomainMap.t;
  cases : case_info CaseMap.t;
  domain_table : domain StrMap.t;
  case_table : case StrMap.t;
  group_repr_size : int; (* needed size of group bitset *)
}

type shape = Group.t list

let empty_world = {
  domains = DomainMap.empty;
  cases = CaseMap.empty;
  domain_table = StrMap.empty;
  case_table = StrMap.empty;
  group_repr_size = 1;
}

let empty_shape = []

let is_empty_shape s = s = []

let any_projection world = Group.everything_up_to (world.group_repr_size - 1)

let shape_of_everything world = [ any_projection world ]

let are_disjoint_groups (groups : Group.t list) =
  let rec aux u gs =
    match gs with
    | [] -> true
    | g::gs ->
      if Group.is_empty (Group.inter u g)
      then aux (Group.union u g) gs
      else false
  in
  aux Group.empty groups

let shape_of_groups (groups : Group.t list) =
  if not (are_disjoint_groups groups) then
    Errors.raise_error "Overlapping groups in shape definition";
  groups

let is_any_projection world (g : Group.t) = Group.equal g (any_projection world)

let shape_perimeter (s : shape) =
  List.fold_left Group.union Group.empty s

let shape_filter_projection (s : shape) (p : Group.t) =
  List.filter_map (fun g ->
      let i = Group.inter g p in
      if Group.is_empty i then None
      else if Group.equal i g then Some i
      else Errors.raise_error "Incompatible projection")
    s

let shape_clip (s1 : shape) (s2 : shape) =
  let clip_group s g =
    let clipped, grem =
      List.fold_left (fun (clipped, g) sg ->
          let clip = Group.clip sg g in
          let clipped =
            if Group.is_empty clip.only_left
            then clipped
            else clip.only_left::clipped
          in
          let clipped =
            if Group.is_empty clip.common
            then clipped
            else clip.common::clipped
          in
          clipped, clip.only_right)
        ([], g) s
    in
    if Group.is_empty grem then clipped else grem::clipped
  in
  List.fold_left (fun clipped g2 ->
      clip_group clipped g2)
    s1 s2

let shape_imprint_projection (s : shape) (g : Group.t) =
  List.map (fun sg ->
      let clip = Group.clip sg g in
      List.filter (fun g -> not (Group.is_empty g))
        [clip.only_left; clip.common])
    s
  |> List.flatten

let shape_cut_out (s : shape) (g : Group.t) =
  List.filter_map (fun sg ->
      let i = Group.inter g sg in
      if Group.is_empty i then None else Some i)
    s

let shape_overlap_subshape (s : shape) (g : Group.t) =
  List.filter (fun sg -> not (Group.is_empty (Group.inter sg g))) s

let shape_fold (f : 'a -> Group.t -> 'a) (acc : 'a) (s : shape) =
  List.fold_left f acc s

let find_domain world (dom : string) =
  match StrMap.find_opt dom world.domain_table with
  | Some d -> d
  | None -> Errors.raise_error "Unknown domain %s" dom

let find_case world (case : string) =
  match StrMap.find_opt case world.case_table with
  | Some d -> d
  | None -> Errors.raise_error "Unknown case %s" case

let case_is_in_domain world (case : case) (dom : domain) =
  (DomainMap.find dom world.domains).domain_cases |> CaseSet.mem case

let domain_of_case world (case : case) =
  (CaseMap.find case world.cases).case_domain

(* About the structure of group bitsets and their relation with [world] informations.

   Each domain and their cases are added sequentially to the world, the initial
   world with no domains have group_repr_size = 1 bit since there is only one
   possible case of context.

   For a world with d domains, represented with bitsets of size s. Recall that
   each bit corresponds to one point in the space defined by the d domains. When
   a new domain with n cases is added (i.e. when a new dimension is added to the
   space), the prexisting layout is duplicated n times, one to express existence,
   in each new case, of the previous domains.

   Example:
   Assume preexisting domains A and B with two cases each. Groups bitsets for
   this will be of size 4: [A1B1, A2B1, A1B2, A2B2].
   To add a domain C with two cases, this pattern is duplicated:
   [A1B1C1, A2B1C1, A1B2C1, A2B2C1, A1B1C2, A2B1C2, A1B2C2, A2B2C2].

   The resulting world information is now:
   group_repr_size = 8
   A.domain_period = 2
   A.domain_case_size = 1
   B.domain_period = 4
   B.domain_case_size = 2
   C.domain_period = 8
   C.domain_case_size = 4

   An invariant for domain and cases identifier:
   A domain with identifier d with cases c_i (0<=i<n, n the number of cases) has
   its cases identified as d + i. There is no two cases with the same identifier
   (and so is for domains).
*)

let add_domain =
  (* global counter for fresh identifiers *)
  let c = ref 0 in
  fun world (dom : string) (cases : string list) ->
    let domain = !c in
    let case_table, cases, domain_cases =
      List.fold_left (fun (ct, cs, dc) case_name ->
          if StrMap.mem case_name ct then
            Errors.raise_error "Case %s already declared" case_name;
          let i = !c in
          incr c;
          StrMap.add case_name i ct,
          CaseMap.add i { case_name; case_domain = domain } cs,
          CaseSet.add i dc
        )
        (world.case_table, world.cases, CaseSet.empty) cases
    in
    let domain_table =
      if StrMap.mem dom world.domain_table then
        Errors.raise_error "Domain %s already declared" dom;
      StrMap.add dom domain world.domain_table
    in
    let group_repr_size =
      let s = world.group_repr_size * (!c - domain) in
       if s > Group.max_val then
         Errors.raise_error "(internal) Too many contexts for %d bits bitvector"
           Group.max_val
       else s
    in
    let domains =
      DomainMap.add domain
        { domain_name = dom;
          domain_cases;
          domain_case_size = world.group_repr_size;
          domain_period = group_repr_size;
        }
        world.domains
    in
    { domains; domain_table; case_table; cases; group_repr_size }

let group_of_selection world (select : CaseSet.t DomainMap.t) =
  let selected_bits =
    DomainMap.fold (fun dom dinfo sels ->
        let cases =
          (* Abscence of case specification of a domain is wildcard *)
          match DomainMap.find_opt dom select with
          | None -> dinfo.domain_cases
          | Some cases ->
            if CaseSet.is_empty cases
            then dinfo.domain_cases
            else cases
        in
        List.map (fun sel ->
            CaseSet.fold (fun c sels ->
                (* see identifiers invariant *)
                (sel + dinfo.domain_case_size * (c - dom))::sels)
              cases [])
          sels
        |> List.flatten)
      world.domains [0]
  in
  List.fold_left (fun g o -> Group.add o g) Group.empty selected_bits

let group_desc world (g : Group.t) =
  let select_case_in_group (dinfos : domain_info)
      (d : domain) (c : case) (g : Group.t)
    : Group.t * int =
    let off = dinfos.domain_case_size in
    let period = dinfos.domain_period in
    let start = (c - d) * off in
    (Group.select g start off period), start
  in
  let rec aux ((d : domain), (infos : domain_info))
      (doms : domain_info DomainMap.t) (g : Group.t)
    : group_desc =
    let aux g =
      (* factorize recursive calls *)
      let ndoms = DomainMap.remove d doms in
      if DomainMap.is_empty ndoms then [DomainMap.empty] else
        aux (DomainMap.choose ndoms) ndoms g
    in
    let case_split : (CaseSet.t * Group.t) IntMap.t =
      (* Dreadful programming trick:

         We want all cases of a domain with the same existence pattern to be
         grouped together. For that we use an IntMap whose keys are the
         existence patterns of cases aliased with a right shift. Reason is, for
         a same domain every case will have the same layout in the bitset, only
         shifted. Realiasing the patterns means that if two cases have the same
         patterns they will have the same key. *)
      CaseSet.fold (fun c acc ->
          let cg, align = select_case_in_group infos d c g in
          (* [cg] is the initial group where only case [c] of the current domain
             exists. *)
          if Group.is_empty cg then acc else
            IntMap.update (cg lsr align) (function
                | None -> Some (CaseSet.singleton c, cg)
                | Some (cs, csg) ->
                  (* Reconstructing the group with several cases for recursive
                     calls *)
                  Some (CaseSet.add c cs, csg lor cg))
              acc)
        infos.domain_cases IntMap.empty
    in
    IntMap.fold (fun _ (cs, csg) acc ->
        (* [csg] is the initial group where only the cases [cs] exist. Recursive
           call to the other domains within this pattern. *)
        let odoms = aux csg in
        List.map (DomainMap.add d cs) odoms
        @ acc)
      case_split []
  in
  let doms = world.domains in
  if DomainMap.is_empty doms then [] else
    aux (DomainMap.choose doms) doms g

let print_domain world fmt (d : domain) =
  let dom = (DomainMap.find d world.domains).domain_name in
  Format.fprintf fmt "%s" dom

let print_case world fmt (c : case) =
  let case = (DomainMap.find c world.cases).case_name in
  Format.fprintf fmt "%s" case

let print_cases world fmt (cs : CaseSet.t) =
  Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
      (print_case world) fmt
      (CaseSet.elements cs)

let print_dommap world fmt (dm : CaseSet.t DomainMap.t) =
  Format.fprintf fmt "@[<hv 1>(";
  DomainMap.iter (fun d cs ->
      Format.fprintf fmt "%a(%a),@,"
        (print_domain world) d
        (print_cases world) cs)
    dm;
  Format.fprintf fmt "@])"

let print_group world fmt (g : Group.t) =
  let dommaps = group_desc world g in
  Format.fprintf fmt "@[<hv>%X@," g;
    Format.pp_print_list
      (fun fmt map ->
        Format.fprintf fmt "- %a@ " (print_dommap world) map)
      fmt dommaps;
    Format.fprintf fmt "@]"

let print_projection world fmt (p : Group.t) =
  if is_any_projection world p then
    Format.fprintf fmt "[[any]]"
  else begin
    Format.fprintf fmt "@[<hov 2>[[@,";
    print_group world fmt p;
    Format.fprintf fmt "]]@]"
  end

let print_shape world fmt (s : shape) =
  if s = [] then
    Format.fprintf fmt "{nothing}"
  else begin
    Format.fprintf fmt "@[<hv 2>{ ";
    Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@ ")
      (print_group world)
      fmt s;
    Format.fprintf fmt "@]}"
  end

let print_group_id fmt (g : Group.t) =
  Format.fprintf fmt "%X" g

let print_group_desc world fmt (desc : group_desc) =
  let print_one fmt d =
    let cs = DomainMap.fold (fun _d -> CaseSet.union) d CaseSet.empty in
    if List.length desc > 1
    then Format.fprintf fmt "@[<hov>- %a@]" (print_cases world) cs
    else Format.fprintf fmt "@[<hov>%a@]" (print_cases world) cs
  in
  Format.fprintf fmt "@[<v>%a@]"
    (Format.pp_print_list print_one) desc

let print_domain_desc world fmt (dom, dom_info) =
  Format.fprintf fmt "@[<v 2>Domain %a:@,%a@]"
    (print_domain world) dom
    (Format.pp_print_list
       (fun fmt -> Format.fprintf fmt "Case %a" (print_case world)))
    (CaseSet.elements dom_info.domain_cases)

let print_world_desc fmt world =
  Format.fprintf fmt "@[<v>%a@]"
    (Format.pp_print_list (print_domain_desc world))
    (DomainMap.bindings world.domains)
