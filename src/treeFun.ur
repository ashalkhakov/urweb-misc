datatype t a = Tnil | Tnode of (a * list (t a))

fun mapM [m ::: (Type -> Type)] (_ : monad m) [a] [b] f x =
    let
      fun aux t =
          case t of
            Tnil => return Tnil
          | Tnode (x, ts) =>
            x' <- f x;
            ts' <- List.mapM aux ts;
            return (Tnode (x', ts'))
    in
      aux x
    end
fun mapX [a] [ctx ::: {Unit}] f =
    let
      fun aux t =
          case t of
            Tnil => <xml/>
          | Tnode (x, ts) => <xml>{f x}{List.mapX aux ts}</xml>
    in
      aux
    end

functor Make(M : sig
                 type key
                 con id :: Name
                 con parent :: Name
                 con order :: Name
                 con cols :: {Type}
                 constraint [id] ~ [parent]
                 constraint [id] ~ [order]
                 constraint [parent] ~ [order]
                 constraint [id, parent, order] ~ cols

                 val key_inj : sql_injectable_prim key

                 table tab : ([id = key, parent = option key, order = int] ++ cols)
             end) = struct

  open M
  type elt = $([id = key, parent = option key, order = int] ++ cols)

    fun tree (root : option key) : transaction (list (t elt)) =
        let
          fun aux (root : option key) =
          query (
          SELECT * FROM tab
          WHERE {eqNullable' (SQL tab.{parent}) root}
          ORDER BY tab.{order} DESC)
                    (fn r lst =>
                            children <- aux (Some r.Tab.id);
                            return (Tnode (r.Tab, children) :: lst))
                    []
        in
          aux root
        end
end
