let rec map (f : 'a -> ('b -> 'c) -> 'c) (l : 'a list) (k : 'b list -> 'c) : 'c
    =
  match l with
  | [] -> k []
  | x :: l' ->
    f x @@ fun x' ->
    map f l' @@ fun l'' -> k @@ (x' :: l'')
