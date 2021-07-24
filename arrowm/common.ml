module type ty = sig
  module type t
  module M:t
end
