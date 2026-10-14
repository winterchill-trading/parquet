open! Core

module Column = struct
  module Accessor = struct
    type 'a t =
      | Int32 of ('a -> int)
      | Int64 of ('a -> int)
      | Double of ('a -> float)
      | Byte_array of ('a -> string)
      | Boolean of ('a -> bool)
  end

  module Kind = struct
    type 'a t =
      | Required of 'a Accessor.t
      | Optional : ('a -> 'b option) * 'b Accessor.t -> 'a t
  end

  type 'a t =
    { kind : 'a Kind.t
    ; schema_element : Metadata.Schema_element.t
    ; dictionary : bool
    }

  let leaf ?converted_type ?scale ?precision ?logical_type name type_ accessor =
    { kind = Required accessor
    ; dictionary = false
    ; schema_element =
        { type_ = Some type_
        ; type_length = None
        ; repetition_type = Some Required
        ; name
        ; num_children = None
        ; converted_type
        ; scale
        ; precision
        ; logical_type
        }
    }
  ;;

  let int32 name f = leaf name Int32 (Int32 f)
  let int64 name f = leaf name Int64 (Int64 f)
  let float name f = leaf name Double (Double f)

  let utf8 name f =
    leaf name Byte_array (Byte_array f) ~converted_type:Utf8 ~logical_type:String
  ;;

  let bytes name f = leaf name Byte_array (Byte_array f)

  let timestamp_ns ?(adjusted_to_utc = false) name f =
    leaf
      name
      Int64
      (Int64 (fun row -> Time_ns.to_int_ns_since_epoch (f row)))
      ~logical_type:(Timestamp { is_adjusted_to_utc = adjusted_to_utc; unit = Nanos })
  ;;

  let decimal ~precision ~scale name f =
    if precision < 1 || precision > 18 || scale < 0 || scale > precision
    then
      raise_s
        [%message
          "Parquet.Schema.Column.decimal: unsupported precision or scale"
            (name : string)
            (precision : int)
            (scale : int)];
    leaf
      name
      Int64
      (Int64 f)
      ~converted_type:Decimal
      ~scale
      ~precision
      ~logical_type:(Decimal { scale; precision })
  ;;

  let bool name f = leaf name Boolean (Boolean f)
  let name t = t.schema_element.name
  let kind t = t.kind
  let schema_element t = t.schema_element
  let is_dictionary t = t.dictionary

  let is_optional t =
    match t.kind with
    | Required _ -> false
    | Optional _ -> true
  ;;

  let optional make name f =
    let inner = make name Fn.id in
    match inner.kind with
    | Optional _ ->
      raise_s
        [%message "Parquet.Schema.Column.optional: already optional" (name : string)]
    | Required accessor ->
      { inner with
        kind = Optional (f, accessor)
      ; schema_element = { inner.schema_element with repetition_type = Some Optional }
      }
  ;;

  let dictionary t =
    match t.kind with
    | Required (Boolean _) | Optional (_, Boolean _) ->
      raise_s
        [%message
          "Parquet.Schema.Column.dictionary: not for boolean columns"
            ~name:(name t : string)]
    | Required _ | Optional _ -> { t with dictionary = true }
  ;;

  let[@cold] absent name =
    raise_s [%message "Parquet.Schema.Column.accessor: value absent" (name : string)]
  ;;

  let accessor (type a) (t : a t) : a Accessor.t =
    match t.kind with
    | Required accessor -> accessor
    | Optional (f, accessor) ->
      let get row =
        match f row with
        | Some v -> v
        | None -> absent (name t)
      in
      (match accessor with
       | Int32 g -> Int32 (fun row -> g (get row))
       | Int64 g -> Int64 (fun row -> g (get row))
       | Double g -> Double (fun row -> g (get row))
       | Byte_array g -> Byte_array (fun row -> g (get row))
       | Boolean g -> Boolean (fun row -> g (get row)))
  ;;

  let physical_type t =
    match t.schema_element.type_ with
    | Some type_ -> type_
    | None -> assert false
  ;;
end

type 'a t = { columns : 'a Column.t list }

let create columns =
  if List.is_empty columns then raise_s [%message "Parquet.Schema.create: no columns"];
  (match List.find_a_dup (List.map columns ~f:Column.name) ~compare:String.compare with
   | Some name ->
     raise_s [%message "Parquet.Schema.create: duplicate column" (name : string)]
   | None -> ());
  { columns }
;;

let columns t = t.columns

let schema_elements t : Metadata.Schema_element.t list =
  { type_ = None
  ; type_length = None
  ; repetition_type = None
  ; name = "schema"
  ; num_children = Some (List.length t.columns)
  ; converted_type = None
  ; scale = None
  ; precision = None
  ; logical_type = None
  }
  :: List.map t.columns ~f:Column.schema_element
;;
