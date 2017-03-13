con orderByCols = fn cols :: {Unit} => mapU sql_direction cols
datatype orderDirection = ODasc | ODdesc
con orderBy = fn (cols :: {Unit}) => variant (map (fn _ => orderDirection) cols)
                                     
con args = fn cols :: {Unit} => [
              OrderBy = variant (map (fn _ => orderDirection) cols)
            , Top = int
            , Skip = int
              ]                                 

fun pagination_orderby_parse
      [nm :: Name] [cols0 ::: {Unit}] [[nm] ~ cols0]
      (def_dir: orderDirection)
      (fl: folder ([nm] ++ cols0))
      (names: $(map (fn _ => string) ([nm] ++ cols0)))
      (x: string) =
    let
      val (col, dir) = Option.get (x, "asc") (String.split x #"-")
      val dir = case dir of
                  "asc" => ODasc
                | "desc" => ODdesc
                | _ => def_dir
      val tmp = @Prelude.mapNm0
                 [fn r _ => variant (map (fn _ => orderDirection) r)]
                 fl
                 (fn [others ::_]
                       [nm' ::_]
                       [t :::_]
                       [[nm'] ~ others]
                       (fl0: folder others)
                       (pf_eq: Prelude.equal ([nm] ++ cols0) ([nm' = t] ++ others)) =>
                 let
                   val res = @@make [nm'] [orderDirection] [map (fn _ => orderDirection) others] ! dir
                 in
                   res
                 end)
  val res = @foldR2
             [fn t => string]
             [fn _ => variant (map (fn _ => orderDirection) ([nm] ++ cols0))]
             [fn _ => unit -> variant (map (fn _ => orderDirection) ([nm] ++ cols0))]
             (fn [nm ::_] [t ::_] [r ::_] [[nm] ~ r] name' sel acc f =>
                 if name' = col then sel
                 else acc {})
             (fn _ => make [nm] def_dir)
             fl names tmp {}
in
  res
end

fun
pagination_orderby_unparse
  [cols ::: {Unit}] (fl : folder cols) (names: $(map (fn _ => string) cols)) (x: variant (map (fn _ => orderDirection) cols)) =
let
  val tmp =
      @mp [fn _ => string]
         [fn _ => orderDirection -> string]
         (fn [t :::_] name dir => name ^ "-" ^ (case dir of ODasc => "asc" | ODdesc => "desc"))
         fl
         names
in
  match x tmp
end

fun
qstringvalue_orderby
  [cols ::: {Unit}]
  (fl: folder cols)
  (names : $(map (fn _ => string) cols)) = let
  fun to v =
      let
        val v = @pagination_orderby_unparse fl names v
      in
        v :: []
      end

  fun from def x =
      case x of
        [] => (
        case def of
          None => error <xml>qstringvalue_pagination: expected a value</xml>
        | Some v => v
        )
      | x :: [] =>
        let
          val (col, dir) = Option.get (x, "asc") (String.split x #"-")
          val dir = case dir of
                      "asc" => ODasc
                    | "desc" => ODdesc
                    | _ => error <xml>qstringvalue_pagination: unexpected direction {[dir]}</xml>
          val tmp = @Prelude.mapNm0
                     [fn r _ => variant (map (fn _ => orderDirection) r)]
                     fl
                     (fn [others ::_]
                           [nm' ::_]
                           [t :::_]
                           [[nm'] ~ others]
                           (fl0: folder others)
                           (pf_eq: Prelude.equal cols ([nm' = t] ++ others)) =>
                         let
                           val res = @@make [nm'] [orderDirection] [map (fn _ => orderDirection) others] ! dir
                         in
                           res
                         end)
          val res = @foldR2
                     [fn t => string]
                     [fn _ => variant (map (fn _ => orderDirection) cols)]
                     [fn _ => unit -> variant (map (fn _ => orderDirection) cols)]
                     (fn [nm ::_] [t ::_] [r ::_] [[nm] ~ r] name' sel acc f =>
                         if name' = col then sel
                         else acc {})
                     (fn _ =>
                         case def of
                           None => error <xml>qstringvalue_from: unable to locate alternative for {[col]}</xml>
                         | Some x => x)
                     fl names tmp {}
        in
          res
        end
      | _ => error <xml>qstringvalue_pagination: multiple values not supported: {[x]}</xml>
in
  Qstring.mkQstringValue {ToQstringValue = to, FromQstringValue = from}
end

fun
get_args [cols ::: {Unit}] (qs : Qstring.qstring $(args cols)) =
qs <- HtmlReadFfi.getQueryString;
let
  val pagination = Qstring.fromQstring qs
  val pagination = D4.redefine1 [#Skip] pagination (fn skip => if skip < 0 then 0 else skip)
  val pagination = D4.redefine1 [#Top] pagination (fn top => if top <= 0 then 10 else if top > 50 then 50 else top)
in
  return pagination
end

fun
args_get_pagenum [cols ::: {Unit}] (xs: $(args cols)) = let
  val skip = float(xs.Skip)
  val top = float(xs.Top)
  val res = skip / top
  val res = ceil (res)
  val res = res + 1
in
  res
end

fun
args_get_default_url [cols ::: {Unit}] (qs: Qstring.qstring $(args cols)) (x : {Base : url, OrderBy : orderBy cols, Top : int}) = let
  val qs = Qstring.toQstring {OrderBy = x.OrderBy, Top = x.Top, Skip = 0}
in
  Qstring.withQueryString x.Base qs
end

(* ****** ****** *)

type links = {
     PLfirst : option (string * url) (* None if: last element of "before" list is the first element of whole collection *)
   , PLprev : option url (* link to previous page, if it exists *)
   , PLbefore : list (string * url) (* reversed! as in list zipper *)
   , PLafter : list (string * url) (* current link will be the head of this list (if this list is empty, there is no data! *)
   , PLnext : option url (* link to next page, if it exists *)
   , PLlast : option (string * url) (* None if: last element of "after" list is the last element of whole collection *)
}

fun
build_links
  [cols ::: {Unit}]
  (qv : Qstring.qstringvalue (orderBy cols))
  (qs : Qstring.qstring $(args cols))
  (baseurl : url)
  (pagination : $(args cols))
  {Total=_total, Links=links} =
let
  val _limit = pagination.Top
  val _page = args_get_pagenum pagination
  fun
  handler2url (info : {Page : int, Limit : int}) = let
    val info : $(args cols) = { OrderBy = pagination.OrderBy, Top = info.Limit, Skip = (info.Page - 1) * info.Limit}
    val qs = Qstring.toQstring info
  in
    Qstring.withQueryString baseurl qs
  end

  val last = ceil(float(_total) / float(_limit)) (* NOTE: _limit must be > 0! *)
  val start = if _page - links > 0 then _page - links else 1
  val end_ = if _page + links < last then _page + links else last
  val lst = []
            
  val lnk_prev = if _page = 1 then None else Some (handler2url {Page = _page-1, Limit = _limit})
  val lnk_first = if start > 1 then Some ("1", handler2url {Page = 1, Limit = _limit}) else None

  val (bef, aft) = let
    fun
    aux i bef aft =
    if i <= end_ then (
      if i < _page then
        let
          val lnk = handler2url {Page = i, Limit = _limit}
        in
          aux (i+1) ((show i, lnk) :: bef) aft
        end
      else (* i >= _page *)
        let
          val lnk = handler2url {Page = i, Limit = _limit}
        in
          aux (i+1) bef ((show i, lnk) :: aft)
        end
      )
    else (List.rev bef, List.rev aft)
  in
    aux start lst []
  end

  val lnk_last =
      if end_ < last then Some (show last, handler2url {Limit = _limit, Page = last}) else None
  val lnk_next = if _page = last then None else Some (handler2url {Limit = _limit, Page = _page + 1})
in {
  PLfirst= lnk_first
, PLprev = lnk_prev
, PLbefore = bef
, PLafter = aft
, PLnext = lnk_next
, PLlast = lnk_last
  }
end

fun
pagination_widget
  [cols ::: {Unit}]
  (baseurl : url)
  (qv: Qstring.qstringvalue (orderBy cols))
  (qs: Qstring.qstring $(args cols))
  (xs : list {Title : string, OrderBy : orderBy cols})
  (default: orderBy cols)
  (cls: css_class): transaction xbody = let
  fun
  flatten (xs: list string): string = List.foldl (fn x y => String.append x y) "" xs
  val current_order = flatten (Qstring.toQstringValue default)
  val order_list =
      List.mp (fn x => x ++ {Key=flatten (Qstring.toQstringValue x.OrderBy)}) xs
in
  order_src <- source current_order;
  let
    val onorderbychange =
        ob <- get order_src;
        let
          val ob_opt = List.find (fn x => x.Key = ob) order_list
          val ob_opt = Option.mp (fn x => x.OrderBy) ob_opt
          val ob = Option.get default ob_opt
          val u = args_get_default_url {Base= baseurl, OrderBy= ob, Top = 10}
        in
          redirect (u)
        end
  in
    return <xml>
      <cselect class={cls} source={order_src} onchange={onorderbychange}>
        {List.mapX (fn x => <xml><coption value={x.Key} selected={x.Key = current_order}>{[x.Title]}</coption></xml>) order_list}
      </cselect>
    </xml>
  end
end
