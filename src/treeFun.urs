datatype t a =
| Tnil
| Tnode of (a * list (t a))

val mapM : m ::: (Type -> Type) -> monad m -> a ::: Type -> b ::: Type
           -> (a -> m b) -> t a -> m (t b)
val mapX : a ::: Type -> ctx ::: {Unit} -> (a -> xml ctx [] []) -> t a -> xml ctx [] []

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
             end) : sig
  
  con id = M.id
  con key = M.key
  con parent = M.parent
  con order = M.order
  type elt = $([id = key, parent = option key, order = int] ++ M.cols)
  
  val tree : option M.key -> transaction (list (t elt))
                             

end
