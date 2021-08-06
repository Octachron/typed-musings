type 'a line = 'a
  constraint 'a =
    <left:_; mid:_;right:_>

type o = Empty
type b = Border
type x = Visited

type 'a dup = 'a * ('a * 'a)

type 'a hline =
  <left:'a dup; mid:b; right:'a dup>

type halfbline = <left:b; mid:b; right:b * (b * b)>
type bline = b hline
type fline = o hline


type 'a board = 'a
  constraint 'a = <up:_; mid:_; down:_; border:_>

type ('p) append = 'a * ('b * 'c)
  constraint 'p = ('a*'b) * 'c

type 'a mark = <left:'l; mid:x; right:'r>
  constraint 'a =
    <left:'l; mid:o; right:'r>

type 't move_left = <left:'b; mid:'a; right:'m * 'r>
  constraint 't = <left:'a * 'b; mid:'m; right:'r>

type 't move_left_all =
  'a move_left * ('b move_left * 'c move_left)
  constraint 't = 'a * ('b * 'c)


type 't move_right = <left:'m * 'l; mid:'r; right:'r2>
  constraint 't = <left:'l; mid:'m; right:'r * 'r2>

type 't move_right_all =
  'a move_right * ('b move_right * 'c move_right)
  constraint 't = 'a * ('b * 'c)


type ('a,'b) move =
  | Up: (
      <up:'u * 'ur; mid:'m; down: 'd1 * ('d2 * _); border:'inf>,
      <up: ('ur * 'inf) append;
       mid:'u mark;
       down: 'm * ('d1 * 'd2);
       border:'inf;
      >
    ) move
  | Down: (
      <up:'u1 * ('u2 * _); mid:'m; down: 'd * 'dr; border:'inf;>,
      <up: 'm * ('u1 * 'u2);
       mid:'d mark;
       down: ('dr * 'inf) append;
       border:'inf;
      >
    ) move
  | Left:(
      <up:'up; mid:'mid; down:'down; border:'b>,
      <up:'up move_left_all;
       mid:'mid move_left mark;
       down:'down move_left_all;
       border: 'b move_left
      >
    ) move
  | Right:(
      <up:'up; mid:'mid; down:'down; border:'b>,
      <up:'up move_right_all;
       mid:'mid move_right mark;
       down:'down move_right_all;
       border: 'b move_right;
      >
    ) move



type half = <left:b; mid:o; right:o * (o * b)>

type start =
  <up  : halfbline dup;
   mid : half;
   down: half dup;
   border:halfbline
  >

type _ path =
  | []: start path
  | (::):  ('a,'b) move * 'a path -> 'b path


let ( + ) l x = x :: l

let test =
  []
  + Right + Right
  + Down
  + Left + Left
  + Down
  + Right + Right
  + Down
  + Left + Left
(*
let slow (type a): a path -> unit = function
  | [_;_;_;
     _;_;_;
     _;_;_;
     _;_;_;_
    ]-> .
  | _ -> ()
*)
(*
let err = [] :: Up
*)