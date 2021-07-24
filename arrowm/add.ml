module B = Init


module type t = B.t -> B.t
module Zero = functor (Zero:B.t) -> Zero

module S(N:t)(X:B.t) = B.S.M(N(X))

module type T = sig module type t end
module TypeOf(X:sig module type t end) = struct
 module type t = sig
   module type t = X.t
   module M: t
 end
end
module Compose(X:B.t)(N:t)(M:t) = struct
  module type t = TypeOf(N(M(X))).t
end

module Op(N:t)(M:t)(X:B.t): module type of N(M(X)) = N(M(X))
module Fix(N:t) = N(B.Z)


module One = S(Zero)
module Two = S(One)
module Four = Op(Two)(Two)
module M4 = Fix(Four)

module T = M4.M.A.A.A.A

(*

module type fty = functor(Zero:ty) ->
sig
  module type nat
  module type t = Zero.t -> nat
  module M: t
end


module S(N:fty) = struct
  module type nat = sig
    module type t = sig module A: N.nat end
    module M: t
  end
  module type t = ty -> nat
  module M = functor (X:ty) -> struct
    module type t = sig module A:N.nat end
    module M: t = struct module A = N.M(X) end
   end
end


module Op(N:fty)(M:fty) = struct
  module type nat = sig
    module type t = sig module A: N.nat end
    module M: t
  end
  module type t = ty -> nat
  module M = functor (X:ty) -> struct
    module type t = sig module A:N.nat end
    module M: t = struct module A = N.M(X) end
   end
end

*)
