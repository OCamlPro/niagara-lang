(** A dimension in the discrete context space *)
type domain

(** A point of a domain *)
type case

module CaseSet : Set.S with type elt = case
module DomainMap : Map.S with type key = domain

(** A group is a set of points in the context space. It represents an
    aggregation of undistinct contexts (presumably because there was no need for
    them to be separate).

    A group can also represent a projection, which is a selection of points in
    the space in the surface language. *)
module Group : sig
  type t

  module Map : Map.S with type key = t
  module Set : Set.S with type elt = t

  (** [union g1 g2] returns a group that contains points that are either in [g1]
      or in [g2]. *)
  val union : t -> t -> t

  (** [inter g1 g2] returns a group that contains points that are both in [g1]
      and [g2]. *)
  val inter : t -> t -> t

  (** [diff g1 g2] returns a group that contains points that are in [g1] but not
      in [g2]. *)
  val diff : t -> t -> t

end

(** A shape is a set of disjoint groups that represents the various
    aggregation of points in the context space. The space is not necessarily
    present in its entirety. *)
type shape

(** Representation of a group population. Each domain to a set of cases, the
    corresponding space point being the cartesian product of each case set.

    This representation might in some cases need to be fragmented, hence the list.

    An example:
    - Domain A: cases A1, A2
      Domain B: case B1
    - Domain A: case A2
      Domain B: case B2

    There is two fragments: the first represent the points (A1, B1) and (A2,
    B1), and the second (A2, B2). It would not be possible to include the latter
    in the former because
    - Domain A: cases A1, A2
      Domain B: cases B1, B2
    represents the points (A1, B1), (A1, B2), (A2, B1) and (A2, B2), which is
    one too many.

    Some invariants of a well-formed description:
    - Every fragment are disjoints
    - Every domain exists in every fragment and contains at least on case.
*)
type group_desc = CaseSet.t DomainMap.t list

(** All the context informations are contained in this type. It can be querried
    through various functions below. *)
type world

(** Returns a group that contains every points of the space.

    In the perspective of projections, this is an all inclusive selection. *)
val any_projection : world -> Group.t

(** The shape where no context exists *)
val empty_shape : shape

(** Checks a shape for existence of groups *)
val is_empty_shape : shape -> bool

(** The shape where every points of the space is contained in the same group. *)
val shape_of_everything : world -> shape

(** [shape_perimeter s] returns a group that contains all the points that exists
    in any group of the shape [s]. *)
val shape_perimeter : shape -> Group.t

(** [shape_filter_projection s g] returns a shape whose groups are the groups of [s] that
    are entirerly included in [g].

    Raise an error if any group of [s] is partially included in [g].
*)
val shape_filter_projection : shape -> Group.t -> shape

(** [shape_clip s1 s2] returns a shape whose groups contains points that are in
    [s1] or [s2], and if in both, were in the same group of [s1] and the same of
    [s2].

    In other words, its an union of shapes that conserves the distinctions of
    points. *)
val shape_clip : shape -> shape -> shape

(** [shape_imprint_projection s g] returns a shape whose groups are distinct in
    the same way as [s] with the additional distinction whever their points are
    in [g] or not.

    In other words, it slices the input shape along the lines of the input
    group.
*)
val shape_imprint_projection : shape -> Group.t -> shape

(** [shape_overlap_subshape s g] returns a shape whose groups are the one of [s]
    that intersect with [g].
*)
val shape_overlap_subshape : shape -> Group.t -> shape

(** [shape_cut_out s g] returns a shape whose groups are the non-empty
    intersection of the ones of [s] and [g].

    It is equivalent to [shape_filter_projection (shape_imprint_projection s g)
    g].

*)
val shape_cut_out : shape -> Group.t -> shape

(** Creates a shape from groups.
    An error is thrown if the groups are not all disjoints.
*)
val shape_of_groups : Group.t list -> shape

(** Fold on the groups of a shape. *)
val shape_fold : ('a -> Group.t -> 'a) -> 'a -> shape -> 'a

(** The zero dimension space *)
val empty_world : world

(** [add_domain world dom cases] returns a world supplemented with the new
    domain named [dom] with the cases named [cases]. *)
val add_domain : world -> string -> string list -> world

(** Returns the domain identifier corresponding to the given domain name. *)
val find_domain : world -> string -> domain

(** Returns the case identifier corresponding to the given case name. *)
val find_case : world -> string -> case

(** Checks if a case is one of the given domain. *)
val case_is_in_domain : world -> case -> domain -> bool

(** [group_of_selection world select] returns a group corresponding to the given
    selection of domain cases.

    The selection has a structure similar to [group_desc] (i.e. a cartesian
    product of cases). A notable difference is that if a domain is absent or has
    been attributed zero cases, the group will contain every cases of that
    domain. This is akin to a wildcard.
*)
val group_of_selection : world -> CaseSet.t DomainMap.t -> Group.t

(** Returns the domain of the given case. *)
val domain_of_case : world -> case -> domain

(** Check whever the given group contains the entire space of the given
    world.
*)
val is_any_projection : world -> Group.t -> bool

(** Return the group description of a given group. *)
val group_desc : world -> Group.t -> group_desc


(** Various debug printing *)

val print_group_id : Format.formatter -> Group.t -> unit

val print_group_desc : world -> Format.formatter -> group_desc -> unit

val print_projection : world -> Format.formatter -> Group.t -> unit

val print_shape : world -> Format.formatter -> shape -> unit

val print_domain : world -> Format.formatter -> domain -> unit

val print_case : world -> Format.formatter -> case -> unit

val print_world_desc : Format.formatter -> world -> unit
