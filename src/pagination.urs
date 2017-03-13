con orderByCols = fn (cols :: {Unit}) => mapU sql_direction cols

datatype orderDirection = ODasc | ODdesc
con orderBy = fn (cols :: {Unit}) => variant (map (fn _ => orderDirection) cols)

(*val mkOrderByEq : cols ::: {Unit} -> folder cols -> eq (orderBy cols)*)
                                     
con args = fn (cols :: {Unit}) => [
                        OrderBy = orderBy cols,
                        Top = int,
                        Skip = int
                        ]

val qstringvalue_orderby
    : cols ::: {Unit} -> folder cols
      -> $(map (fn _ => string) cols)
      -> Qstring.qstringvalue (orderBy cols)

val get_args : cols ::: {Unit} -> Qstring.qstring $(args cols) -> transaction $(args cols)
val args_get_pagenum : cols ::: {Unit} -> $(args cols) -> int
val args_get_default_url : cols ::: {Unit} -> Qstring.qstring $(args cols) -> {Base : url, OrderBy : orderBy cols, Top : int} -> url

val
pagination_widget :
cols ::: {Unit} -> url(*base url*)
-> Qstring.qstringvalue (orderBy cols)
-> Qstring.qstring $(args cols)
-> list {Title : string, OrderBy : orderBy cols}
-> orderBy cols
-> css_class
-> transaction xbody
                                         
type links = {
     PLfirst : option (string * url) (* None if: last element of "before" list is the first element of whole collection *)
   , PLprev : option url (* link to previous page, if it exists *)
   , PLbefore : list (string * url) (* reversed! as in list zipper *)
   , PLafter : list (string * url) (* current link will be the head of this list (if this list is empty, there is no data! *)
   , PLnext : option url (* link to next page, if it exists *)
   , PLlast : option (string * url) (* None if: last element of "after" list is the last element of whole collection *)
}

val build_links :
    cols ::: {Unit}
    -> Qstring.qstringvalue (orderBy cols)
    -> Qstring.qstring $(args cols)
    -> url (* base url *)
    -> $(args cols)
    -> {Total : int, Links : int}
    -> links
