
module B = Add


module type t = B.t -> B.t
module One = functor (Zero:B.t) -> Zero

module S(N:t)(X:B.t) = B.Op(N(X))(X)
module Op(N:t)(M:t)(X:B.t): module type of N(M(X)) = N(M(X))
module Fix(N:t) = N(B.One)(B.B.Z)


module Two = S(One)
module Four = Op(Two)(Two)
module Sixteen = Op(Four)(Four)

module F16 = Fix(Sixteen)
module T =
  F16.M
  .A.A.A.A
  .A.A.A.A
  .A.A.A.A
  .A.A.A.A
