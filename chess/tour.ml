type 'a line = 'a
  constraint 'a =
    <left:_; mid:_;right:_>

type 'a case = Case
type g = Ongoing
type d = Done
type o = g case
type s = d case
type b = Border
type x = Visited

type 'a dup = 'a * ('a * 'a)

type 'a hline =
  <left:'a dup; mid:b; right:'a dup>

type halfbline = <left:b; mid:b; right:b * (b * b)>
type bline = b hline
type fline = o hline


type 'a board = 'a
  constraint 'a = <up:_; mid:_; down:_; border:_; status:_>

type ('p) append = 'a * ('b * 'c)
  constraint 'p = ('a*'b) * 'c

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

type 'x mark =
  <up:'up;
   mid:<left:'l;mid:x;right:'r>;
   down:'down;
   border:'b;
   status:'st
  >
  constraint
    'x =
  <up:'up;
   mid:<left:'l;mid:'st case;right:'r>;
   down:'down;
   border:'b;
   status:g
  >

type 'x up =
  <up: ('ur * 'inf) append;
   mid:'u;
   down: 'm * ('d1 * 'd2);
   border:'inf;
   status:g;
  >
  constraint 'x =
    <up:'u * 'ur; mid:'m; down: 'd1 * ('d2 * _); border:'inf; status:g>

type 'x down =
  <up: 'm * ('u1 * 'u2);
   mid:'d;
   down: ('dr * 'inf) append;
   border:'inf;
   status:g;
  >
  constraint 'x =
    <up:'u1 * ('u2 * _); mid:'m; down: 'd * 'dr; border:'inf; status:g>

type 'x left =
      <up:'up move_left_all;
       mid:'mid move_left;
       down:'down move_left_all;
       border: 'b move_left;
       status:g;
      >
  constraint 'x =
     <up:'up; mid:'mid; down:'down; border:'b; status:g;>

type 'x right =
  <up:'up move_right_all;
   mid:'mid move_right;
   down:'down move_right_all;
   border: 'b move_right;
   status:g;
  >
  constraint 'x =
    <up:'up; mid:'mid; down:'down; border:'b;status:g>

type ('a,'b) move =
  | UUL: ('a,'a up up left mark) move
  | UUR: ('a,'a up up right mark) move
  | LLU: ('a,'a left left up mark) move
  | LLD: ('a,'a left left down mark) move
  | DDL: ('a,'a down down left mark) move
  | DDR: ('a,'a down down right mark) move
  | RRU: ('a,'a right right up mark) move
  | RRD: ('a,'a right right down mark) move


type half = <left:b; mid:o; right:o * (o * b)>
type shalf = <left:b; mid:d case; right:o * (o * b)>
type vhalf = <left:b; mid:x; right:x * (x * b)>


type start =
  <up  : halfbline dup;
   mid : shalf;
   down: half dup;
   border:halfbline;
   status:g;
  >
type tour =
  <up  : halfbline dup;
   mid : vhalf;
   down: vhalf dup;
   border:halfbline;
   status:d;
  >

type _ path =
  | []: start path
  | (::):  ('a,'b) move * 'a path -> 'b path


let ( + ) l x = x :: l

let test =
  [] + RRD + LLD + RRD + UUL

let t = [] + DDR + UUR

type 'a res =
  | Yes: tour res
  | No : _ res

let any: type a. a path -> s res = function
  | [DDR] -> No
  | [UUL;DDR] -> No
  | [_;UUL;DDR] -> .
  | [UUR;DDR] -> No
  | [LLD; UUR; DDR] -> No
  | [DDR; LLD; UUR; DDR] -> No
  | [UUR; DDR; LLD; UUR; DDR] -> No
  | [LLU; UUR; DDR; LLD; UUR; DDR] -> No
  | [_;LLU; UUR; DDR; LLD; UUR; DDR] -> .
  | [LLD; UUR; DDR; LLD; UUR; DDR] -> No
  | [UUR;LLD; UUR; DDR; LLD; UUR; DDR] -> No
  | [DDR; UUR;LLD; UUR; DDR; LLD; UUR; DDR] -> No
  | [LLD; DDR; UUR;LLD; UUR; DDR; LLD; UUR; DDR] -> No
  | [UUR;LLD; DDR; UUR;LLD; UUR; DDR; LLD; UUR; DDR] -> No
  | [DDR; UUR;LLD; DDR; UUR;LLD; UUR; DDR; LLD; UUR; DDR] -> No
  | [_; DDR; UUR;LLD; DDR; UUR;LLD; UUR; DDR; LLD; UUR; DDR] -> .
  | [_; UUR;LLD; DDR; UUR;LLD; UUR; DDR; LLD; UUR; DDR] -> .
  | [_;LLD; DDR; UUR;LLD; UUR; DDR; LLD; UUR; DDR] -> .
  | [_; DDR; UUR;LLD; UUR; DDR; LLD; UUR; DDR] -> .
  | [_; UUR;LLD; UUR; DDR; LLD; UUR; DDR] -> .
  | [RRD;LLD; UUR; DDR; LLD; UUR; DDR] -> No
  | [UUL; RRD;LLD; UUR; DDR; LLD; UUR; DDR] -> No
  | [DDL; UUL; RRD;LLD; UUR; DDR; LLD; UUR; DDR] -> No
  | [RRU;DDL; UUL; RRD;LLD; UUR; DDR; LLD; UUR; DDR] -> No
  | [UUL; RRU;DDL; UUL; RRD;LLD; UUR; DDR; LLD; UUR; DDR] -> No
  | [_;UUL; RRU;DDL; UUL; RRD;LLD; UUR; DDR; LLD; UUR; DDR] -> .
  | [_; RRU;DDL; UUL; RRD;LLD; UUR; DDR; LLD; UUR; DDR] -> .
  | [_;DDL; UUL; RRD;LLD; UUR; DDR; LLD; UUR; DDR] -> .
  | [_; UUL; RRD;LLD; UUR; DDR; LLD; UUR; DDR] -> .
  | [_; RRD;LLD; UUR; DDR; LLD; UUR; DDR] -> .
  | [_;LLD; UUR; DDR; LLD; UUR; DDR] -> .
  | [ _ ;UUR; DDR; LLD; UUR; DDR] -> .
  | [_; DDR; LLD; UUR; DDR] -> .
  | [RRD; LLD; UUR; DDR] -> No
  | [UUL; RRD; LLD; UUR; DDR] -> No
  | [DDL; UUL; RRD; LLD; UUR; DDR] -> No
  | [RRU; DDL; UUL; RRD; LLD; UUR; DDR] -> No
  | [LLU; RRU; DDL; UUL; RRD; LLD; UUR; DDR] -> No
  | [_; LLU; RRU; DDL; UUL; RRD; LLD; UUR; DDR] -> .
  | [DDL; RRU; DDL; UUL; RRD; LLD; UUR; DDR] -> No
  | [_; DDL; RRU; DDL; UUL; RRD; LLD; UUR; DDR] -> .
  | [_; RRU; DDL; UUL; RRD; LLD; UUR; DDR] -> .
  | [RRD; DDL; UUL; RRD; LLD; UUR; DDR] -> No
  | [UUL; RRD; DDL; UUL; RRD; LLD; UUR; DDR] -> No
  | [DDL; UUL; RRD; DDL; UUL; RRD; LLD; UUR; DDR] -> No
  | [_; DDL; UUL; RRD; DDL; UUL; RRD; LLD; UUR; DDR] -> .
  | [_; UUL; RRD; DDL; UUL; RRD; LLD; UUR; DDR] -> .
  | [_; RRD; DDL; UUL; RRD; LLD; UUR; DDR] -> .
  | [_; DDL; UUL; RRD; LLD; UUR; DDR] -> .
  | [_; UUL; RRD; LLD; UUR; DDR] -> .
  | [LLD; RRD; LLD; UUR; DDR] -> No
  | [UUR; LLD; RRD; LLD; UUR; DDR] -> No
  | [DDR; UUR; LLD; RRD; LLD; UUR; DDR] -> No
  | [LLU; DDR; UUR; LLD; RRD; LLD; UUR; DDR] -> No
  | [UUR; LLU; DDR; UUR; LLD; RRD; LLD; UUR; DDR] -> No
  | [_; UUR; LLU; DDR; UUR; LLD; RRD; LLD; UUR; DDR] -> .
  | [RRU; LLU; DDR; UUR; LLD; RRD; LLD; UUR; DDR] -> No
  | [LLU; RRU; LLU; DDR; UUR; LLD; RRD; LLD; UUR; DDR] -> No
  | [_; LLU; RRU; LLU; DDR; UUR; LLD; RRD; LLD; UUR; DDR] -> .
  | [DDL; RRU; LLU; DDR; UUR; LLD; RRD; LLD; UUR; DDR] -> No
  | [_; DDL; RRU; LLU; DDR; UUR; LLD; RRD; LLD; UUR; DDR] -> .
  | [_; RRU; LLU; DDR; UUR; LLD; RRD; LLD; UUR; DDR] -> .
  | [_;LLU; DDR; UUR; LLD; RRD; LLD; UUR; DDR] -> .
  | [_; DDR; UUR; LLD; RRD; LLD; UUR; DDR] -> .
  | [_; UUR; LLD; RRD; LLD; UUR; DDR] -> .
  | [_; LLD; RRD; LLD; UUR; DDR] -> .
  | [_; RRD; LLD; UUR; DDR] -> .
  | [_; LLD; UUR; DDR] -> .
  | [_; UUR; DDR] -> .
  | [_;DDR] -> .
  | [RRD] -> No
  | [LLU;RRD] -> No
  | [_;LLU;RRD] -> .
  | [LLD;RRD] -> No
  | [UUR; LLD;RRD] -> No
  | [DDR; UUR; LLD;RRD] -> No
  | [LLD; DDR; UUR; LLD;RRD] -> No
  | [UUR; LLD; DDR; UUR; LLD;RRD] -> No
  | [DDR; UUR; LLD; DDR; UUR; LLD;RRD] -> No
  | [_; DDR; UUR; LLD; DDR; UUR; LLD;RRD] -> .
  | [_; UUR; LLD; DDR; UUR; LLD;RRD] -> .
  | [_; LLD; DDR; UUR; LLD;RRD] -> .
  | [LLU; DDR; UUR; LLD;RRD] -> No
  | [DDR; LLU; DDR; UUR; LLD;RRD] -> No
  | [_; DDR; LLU; DDR; UUR; LLD;RRD] -> .
  | [RRU; LLU; DDR; UUR; LLD;RRD] -> No
  | [DDL; RRU; LLU; DDR; UUR; LLD;RRD] -> No
  | [UUL; DDL; RRU; LLU; DDR; UUR; LLD;RRD] -> No
  | [_; UUL; DDL; RRU; LLU; DDR; UUR; LLD;RRD] -> .
  | [_; DDL; RRU; LLU; DDR; UUR; LLD;RRD] -> .
  | [_; RRU; LLU; DDR; UUR; LLD;RRD] -> .
  | [_; LLU; DDR; UUR; LLD;RRD] -> .
  | [_;DDR; UUR; LLD;RRD] -> .
  | [_; UUR; LLD;RRD] -> .
  | [RRD; LLD;RRD] -> No
  | [UUL; RRD; LLD;RRD] -> No
  | [DDL; UUL; RRD; LLD;RRD] -> No
  | [RRU; DDL; UUL; RRD; LLD;RRD] -> No
  | [UUL; RRU; DDL; UUL; RRD; LLD;RRD] -> No
  | [_; UUL; RRU; DDL; UUL; RRD; LLD;RRD] -> .
  | [LLU; RRU; DDL; UUL; RRD; LLD;RRD] -> No
  | [DDR; LLU; RRU; DDL; UUL; RRD; LLD;RRD] -> No
  | [_; DDR; LLU; RRU; DDL; UUL; RRD; LLD;RRD] -> .
  | [RRU; LLU; RRU; DDL; UUL; RRD; LLD;RRD] -> No
  | [DDL; RRU; LLU; RRU; DDL; UUL; RRD; LLD;RRD] -> No
  | [UUL; DDL; RRU; LLU; RRU; DDL; UUL; RRD; LLD;RRD] -> No
  | [_; UUL; DDL; RRU; LLU; RRU; DDL; UUL; RRD; LLD;RRD] -> .
  | [_; DDL; RRU; LLU; RRU; DDL; UUL; RRD; LLD;RRD] -> .
  | [_; RRU; LLU; RRU; DDL; UUL; RRD; LLD;RRD] -> .
  | [_; LLU; RRU; DDL; UUL; RRD; LLD;RRD] -> .
  | [_; RRU; DDL; UUL; RRD; LLD;RRD] -> .
  | [_; DDL; UUL; RRD; LLD;RRD] -> .
  | [_; UUL; RRD; LLD;RRD] -> .
  | [_; RRD; LLD;RRD] -> .
  | [_;LLD;RRD] -> .
  | [DDL; RRD] -> No
  | [UUL; DDL; RRD] -> No
  | [RRU; UUL; DDL; RRD] -> No
  | [DDL; RRU; UUL; DDL; RRD] -> No
  | [UUL; DDL; RRU; UUL; DDL; RRD] -> No
  | [_; UUL; DDL; RRU; UUL; DDL; RRD] -> .
  | [_; DDL; RRU; UUL; DDL; RRD] -> .
  | [_; RRU; UUL; DDL; RRD] -> .
  | [RRD; UUL; DDL; RRD] -> No
  | [LLD; RRD; UUL; DDL; RRD] -> No
  | [UUR; LLD; RRD; UUL; DDL; RRD] -> No
  | [DDR; UUR; LLD; RRD; UUL; DDL; RRD] -> No
  | [LLU; DDR; UUR; LLD; RRD; UUL; DDL; RRD] -> No
  | [UUR; LLU; DDR; UUR; LLD; RRD; UUL; DDL; RRD] -> No
  | [_; UUR; LLU; DDR; UUR; LLD; RRD; UUL; DDL; RRD] -> .
  | [_; LLU; DDR; UUR; LLD; RRD; UUL; DDL; RRD] -> .
  | [_; DDR; UUR; LLD; RRD; UUL; DDL; RRD] -> .
  | [_; UUR; LLD; RRD; UUL; DDL; RRD] -> .
  | [_; LLD; RRD; UUL; DDL; RRD] -> .
  | [UUL; RRD; UUL; DDL; RRD] -> No
  | [DDL; UUL; RRD; UUL; DDL; RRD] -> No
  | [RRD; DDL; UUL; RRD; UUL; DDL; RRD] -> No
  | [UUL; RRD; DDL; UUL; RRD; UUL; DDL; RRD] -> No
  | [DDL; UUL; RRD; DDL; UUL; RRD; UUL; DDL; RRD] -> No
  | [_; DDL; UUL; RRD; DDL; UUL; RRD; UUL; DDL; RRD] -> .
  | [_; UUL; RRD; DDL; UUL; RRD; UUL; DDL; RRD] -> .
  | [_; RRD; DDL; UUL; RRD; UUL; DDL; RRD] -> .
  | [_; DDL; UUL; RRD; UUL; DDL; RRD] -> .
  | [_; UUL; RRD; UUL; DDL; RRD] -> .
  | [_; RRD; UUL; DDL; RRD] -> .
  | [_; UUL; DDL; RRD] -> .
  | [_; DDL; RRD] -> .
  | [_;RRD] -> .
  | [_] -> .
  | _ -> assert false

let tour: tour path -> unit = function
  | [_] -> .
  | [_;_] -> .
  | _ -> ()

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
