module B = Pow


module type t = sig
  module F: B.t -> B.t
  module X: B.t
end

module One = struct
  module F = functor (Zero:B.t) -> Zero
  module X = B.One
end

module S(N:t) = struct
  module F(X:B.t)= B.Op(N.F(X))(X)
  module X = B.S(N.X)
end
module Op(N:t)(M:t) = struct
  module F(X:B.t) = N.F(M.F(X))
  module X = M.X
end
module Fix(N:t) = B.Fix(N.F(N.X))


module Two = S(One)
module Three = S(Two)
module Four = S(Three)

