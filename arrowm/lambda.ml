module type Ty = sig module type t end

module Id(T:Ty)(X:T.t) = X

module type bool = functor(T:Ty)(X:T.t)(Y:T.t) -> T.t

module True(T:Ty)(X:T.t)(Y:T.t) = X

module False(T:Ty)(X:T.t)(Y:T.t) = Y

module If(T:Ty)(P:bool)(X:T.t)(Y:T.t) = P(T)(X)(Y)

module type nat = functor(T:Ty)(F:T.t->T.t)(X:T.t) -> T.t

module Zero(T:Ty)(F:T.t->T.t)(X:T.t) = X
module Succ(N:nat)(T:Ty)(F:T.t->T.t)(X:T.t) = F(N(T)(F)(X))

module Two = Succ(Succ(Zero))


module IsZero(T:Ty)(X:nat) =
  X(struct module type t = module type of True(T) end)(functor (X:module type of True(T)) -> False(T))(True(T))


module Fst(A:Ty)(B:Ty)(X:A.t)(Y:B.t) = X
module Snd(A:Ty)(B:Ty)(X:A.t)(Y:B.t) = Y

module Pair(A:Ty)(B:Ty)(X:A.t)(Y:B.t)(R:Ty)(F: functor(X:A.t)(Y:B.t) -> R.t) = F(X)(Y)

module BI = Pair(struct module type t = bool end)(struct module type t = nat end)(True)(Two)


module Int = struct module type t = sig val x: int end end
module Int_1 = struct let x = 0 end
module Int_2 = struct let x = 0 end
module Test = IsZero(Int)(Two)(Int_1)(Int_2)
