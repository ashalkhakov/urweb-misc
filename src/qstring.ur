con qstringvalue a = {ToQstringValue : a -> list string,
                      FromQstringValue : option a -> list string -> a}
con qstring a = {ToQstring : a -> queryString,
                 FromQstring : queryString -> a}


fun
uri_encode (s:string):string = let
  fun
  tohexstr i = let
    val low = i % 16
    val high = i / 16
    fun hexdigit i =
        case i of
          0 => "0" | 1 => "1" | 2 => "2" | 3 => "3" | 4 => "4" | 5 => "5" | 6 => "6" | 7 => "7" | 8 => "8" | 9 => "9"
        | 10 => "A" | 11 => "B" | 12 => "C" | 13 => "D" | 14 => "E" | 15 => "F"
        | _ => error <xml>tohexstr: invalid digit {[i]}</xml>
  in
    hexdigit high ^ hexdigit low
  end

  fun
  aux i n s acc =
  if i < n then let
      val c = strsub s i
    in
      (* NOTE: strcat seems to be QUITE inefficient here *)
      if isalnum c || Option.isSome (String.index ";,/?:@&=+$#" c) then
        aux (i+1) n s (strcat acc (str1 c))
      else
        aux (i+1) n s (strcat acc (strcat "%" (tohexstr (ord c))))
    end
  else acc
  val res = aux 0 (strlen s) s ""
in
  res
end

fun
uri_decode (s: string): string = let
  fun
  aux i n s acc =
  if i < n then let
      val c = strsub s i
    in
      (* NOTE: strcat seems to be QUITE inefficient here *)
      if c = #"%" then (
        if i+1 >= n || i+2 >= n then error <xml>decode: premature EOS</xml>
        else let
            val c1 = strsub s (i+1)
            val c2 = strsub s (i+2)
            val digit1 =
                if c1 >= #"a" && c1 <= #"f" then ord c1 - ord #"a" + 10
                else if c1 >= #"A" && c1 <= #"F" then ord c1 - ord #"A" + 10
                else ord c1 - ord #"0"
            val digit2 =
                if c2 >= #"a" && c2 <= #"f" then ord c2 - ord #"a" + 10
                else if c2 >= #"A" && c2 <= #"F" then ord c2 - ord #"A" + 10
                else ord c2 - ord #"0"
            val c0 = chr (digit1 * 16 + digit2)
          in
            aux (i+3) n s (strcat acc (str1 c0))
          end
        )
      else if c = #"+" then aux (i+1) n s (strcat acc " ")
      else aux (i+1) n s (strcat acc (str1 c))
    end
  else acc
in
  aux 0 (strlen s) s ""
end                               

fun toQstringValue [a ::: Type] (qv: qstringvalue a) (x: a) = qv.ToQstringValue x
fun fromQstringValue [a ::: Type] (qv: qstringvalue a) (def: option a) (ls: list string) = qv.FromQstringValue def ls

fun mkQstringValue [a ::: Type] (x : {ToQstringValue : a -> list string,
                                      FromQstringValue : option a -> list string -> a}) = x

val qstringvalue_string = {ToQstringValue= fn x => x :: [],
                           FromQstringValue= fn def x =>
                                                case x of
                                                  [] => (
                                                  case def of
                                                    None => error <xml>qstringvalue_string: expected a value</xml>
                                                  | Some x => x
                                                  )
                                                | x :: [] => x
                                                | _ => error <xml>qstringvalue_string: multiple values not supported: {[x]}</xml>}

fun numIn [a] (_ : read a) s : a = readError s

fun qstringvalue_num [a] (_ : show a) (_ : read a) : qstringvalue a = {ToQstringValue = fn x => show x :: [],
                                                                       FromQstringValue =
                                                                    fn def x =>
                                                                       case x of
                                                                         [] => (
                                                                         case def of
                                                                           None => error <xml>qstringvalue_num: expected a value</xml>
                                                                         | Some x => x
                                                                         )
                                                                       | x :: [] => numIn x(*how to default here??*)
                                                                       | _ => error <xml>qstringvalue_num: exactly one value required</xml>}
                          
val qstringvalue_int = qstringvalue_num
val qstringvalue_float = qstringvalue_num
val qstringvalue_bool = {
    ToQstringValue = fn b => (if b then "true" else "false") :: [],
    FromQstringValue = fn def ls =>
                          case ls of
                            [] => (
                            case def of
                              None => error <xml>qstringvalue_bool: expected a value</xml>
                            | Some x => x
                            )
                          | s :: [] =>
                            if s = "true" then True
                            else if s = "false" then False
                            else error <xml>QstringValue: bad boolean string: {[s]}</xml>
                          | _ => error <xml>qstringvalue_bool: exactly one value required</xml>}
fun qstringvalue_option [a] (qv : qstringvalue a) : qstringvalue (option a) =
    {ToQstringValue = fn v => case v of None => [] | Some v => qv.ToQstringValue v,
     FromQstringValue = fn def ls =>
                           case ls of
                             [] => Option.get None def (* no error! *)
                           | s :: [] =>
                             if String.length s <= 0 then None
                             else let
                                 val v = qv.FromQstringValue (Option.get None def) ls
                               in
                                 Some v
                               end
                           | _ => error <xml>qstringvalue_option: 0 or 1 values are required</xml>}

fun qstringvalue_list [a] (qv : qstringvalue a) : qstringvalue (list a) =
    {ToQstringValue = fn xs => let
                           val xs = List.mp (fn x => qv.ToQstringValue x) xs
                           val xs = List.foldl List.append [] xs
                         in
                           xs
                         end,
     FromQstringValue = fn def ls =>
                           case ls of
                             [] => (
                             case def of
                               None => [] (* no error! *)
                             | Some xs => xs
                             )
                           | _ => List.mp (fn x => qv.FromQstringValue None (x :: [])) ls}

(*
some thoughts:
- absence of keys: should it be handled with an "optional" handler or not?
- a=&b=c
  - [a] is optional
*)
fun qstring_record
      [ts ::: {Type}]
      (fl : folder ts)
      (qss : $(map qstringvalue ts))
      (names : $(map (fn t => string * option t) ts))
    : qstring $ts =
      {ToQstring = fn r => let
                        val res = @foldR3 [qstringvalue] [fn t => string * option t] [ident] [fn _ => string]
                                   (fn [nm ::_] [t ::_] [r ::_] [[nm] ~ r] (qv : qstringvalue t) name v acc => let
                                         val name_enc = uri_encode name.1
                                         val vs = qv.ToQstringValue v
                                         val res =
                                             List.foldl (fn x acc =>
                                                            name_enc ^ "=" ^ uri_encode x ^ (case acc of
                                                               "" => ""
                                                             | acc => "&" ^ acc))
                                                        ""
                                                        vs
                                       in
                                         res ^ (case acc of  "" => "" | acc => "&" ^ acc)
                                       end)
                                   "" fl qss names r
                      in
                        HtmlReadFfi.string2queryString res
                      end,
     FromQstring = fn s =>
                      let
                        fun fromQV s (r : $(map (fn _ => list string) ts)) : $(map (fn _ => list string) ts) =
                           if String.length s = 0 then r
                           else
                             let
                               val (keyvalue, s') =
                                   case String.index s #"&" of
                                     None => (s, "")
                                   | Some ix => (String.substring s {Start = 0, Len = ix}, String.suffix s (ix+1))
                               val (name, value) = Option.get (keyvalue, "") (String.split keyvalue #"=")
                               val name = uri_decode name
                               val v = uri_decode value
                               val (r, s') = @foldR2
                                              [qstringvalue]
                                              [fn t => string * option t]
                                              [fn ts => $(map (fn _ => list string) ts) -> $(map (fn _ => list string) ts) * string]
                                              (fn [nm ::_] [t ::_] [r ::_] [[nm] ~ r] (qv: qstringvalue t) name' acc r =>
                                                  if name = name'.1 then
                                                      (r -- nm ++ {nm = v :: r.nm}, s')
                                                  else
                                                    let
                                                      val (r', s') = acc (r -- nm)
                                                    in
                                                      (r' ++ {nm = r.nm}, s')
                                                    end)
                                              (fn _ => error <xml>Unknown query string parameter name {[name]}</xml>)
                                              fl qss names r
                             in
                               fromQV s' r
                             end
                        val s = HtmlReadFfi.queryString2string s
                        val r = @map0 [fn _ => list string] (fn [t ::_] => []) fl
                        val r = 
                            if String.length s = 0 then r
                            else fromQV s r
                      in
                        @map3
                         [qstringvalue]
                         [fn _ => list string]
                         [fn t => string * option t]
                         [ident]
                         (fn [t ::: Type] (qv: qstringvalue t) v name =>
                             qv.FromQstringValue name.2 (List.rev v))
                         fl qss r names
                      end}

fun toQstring [a ::: Type] (qs: qstring a) (x: a): queryString = qs.ToQstring x
fun fromQstring [a ::: Type] (qs: qstring a) (x: queryString): a = qs.FromQstring x

val
withQueryString = HtmlReadFfi.withQueryString
