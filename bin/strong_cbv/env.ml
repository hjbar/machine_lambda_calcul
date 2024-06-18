open Lambda

type identifier = string

type sem =
  | Sem of (sem -> sem)
  | Neutral of (unit -> lambda_term)
  | Cache of lambda_term cache * sem

and 'a cache = 'a option ref

module Dict = Map.Make (struct
  type t = identifier

  let compare = compare
end)

type env = sem Dict.t
