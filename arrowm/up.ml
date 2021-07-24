module type t = sig
  module B: sig
    module type t
    module Fix: t -> Init.t
  end
  module type t = sig
    module F: B.t -> B.t
    module X: B.t
  end
  module One: t
  module S: t -> t
  module Op: functor(X:t)(Y:t) -> sig
    module F(Z:B.t): module type of X.F(Y.F(Z))
    module X: B.t
  end
  module Fix: functor(X:t) -> module type of B.Fix(X.F(X.X))
end

module S(B:t) = struct

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


end

module Penta = S(Tetra)

module Two = Penta.S(Penta.One)
module M = Penta.Fix(Two)
module T = M.M.A
