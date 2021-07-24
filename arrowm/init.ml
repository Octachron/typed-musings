open Common

module type t = ty
module Z = struct
  module type t = sig end
  module M = struct end
end

module S = struct
  module type t = functor(X:ty) ->
  sig
    module type t = sig
      module A: X.t
    end
    module M:t
  end
  module M(X:ty) = struct
    module type t = sig
      module A: X.t
    end
    module M = struct
      module A = X.M
    end
  end
end

module One = S.M(Z)
module Two = S.M(One)
