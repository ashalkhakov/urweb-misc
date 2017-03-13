(* query string utilities *)

class qstringvalue
class qstring

(* TODO: introduce a typeful API? e.g. abstract type "key-value pair"
val uri_encode : string -> string
val uri_decode : string -> string
*)

val toQstringValue : a ::: Type -> qstringvalue a -> a -> list string
val fromQstringValue : a ::: Type -> qstringvalue a -> option a -> list string -> a
                                                                                  
val mkQstringValue : a ::: Type ->
                     {ToQstringValue : a -> list string,
                      FromQstringValue : option a -> list string -> a} -> qstringvalue a

val qstringvalue_string : qstringvalue string
val qstringvalue_int : qstringvalue int
val qstringvalue_float : qstringvalue float
val qstringvalue_bool : qstringvalue bool
val qstringvalue_option : a ::: Type -> qstringvalue a -> qstringvalue (option a)
val qstringvalue_list : a ::: Type -> qstringvalue a -> qstringvalue (list a)
                                                                                     
val qstring_record : ts ::: {Type} -> folder ts -> $(map qstringvalue ts) -> $(map (fn t => string * option t) ts) -> qstring $ts

val toQstring : a ::: Type -> qstring a -> a -> queryString
val fromQstring : a ::: Type -> qstring a -> queryString -> a

val withQueryString : url -> queryString -> url
