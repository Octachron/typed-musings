type 'a line = 'a
  constraint 'a =
    <left:_; mid:_;right:_>




type 'a not_a_king = [ `pawn of 'a | `queen of 'a | `bishop of 'a | `rook of 'a | `knight of 'a ]

type 'a piece = [ `king of 'a | 'a not_a_king ]
type 'a case = [ `border | `free | 'a piece ]


type 'a dup = 'a * ('a * 'a)


type b = [ `border ]
type o = [ `free ]


type 'a hline =
  <left:'a dup; mid:b; right:'a dup>

type halfbline = <left:b; mid:b; right:b * (b * b)>
type bline = b hline
type fline = o hline


type status = [ `win | `white | `black ]

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

type 's turn = [< `white | `black ] as 's

type white = < opp: black; tag: [ `white ] >
and black = < opp: white; tag: [ `black ] >



type 'x up =
  <up: ('ur * 'inf) append;
   mid:'u;
   down: 'm * ('d1 * 'd2);
   border:'inf;
   status:'s turn;
  >
  constraint 'x =
     <up:'u * 'ur; mid:'m; down: 'd1 * ('d2 * _); border:'inf; status:'s turn>

type 'x down =
  <up: 'm * ('u1 * 'u2);
   mid:'d;
   down: ('dr * 'inf) append;
   border:'inf;
   status:'s turn;
  >
  constraint 'x =
    <up:'u1 * ('u2 * _); mid:'m; down: 'd * 'dr; border:'inf; status:'s turn>

type 'x left =
      <up:'up move_left_all;
       mid:'mid move_left;
       down:'down move_left_all;
       border: 'b move_left;
       status: 's turn;
      >
  constraint 'x =
     <up:'up; mid:'mid; down:'down; border:'b; status:'s turn;>

type 'x right =
  <up:'up move_right_all;
   mid:'mid move_right;
   down:'down move_right_all;
   border: 'b move_right;
   status:'s turn;
  >
  constraint 'x =
    <up:'up; mid:'mid; down:'down; border:'b;status:'s turn>


type ('a,'b,'c,'d) case =
  | A1: ('a,'a,'b,'b) case
  | A2: ('a,'a right,'b,'b left) case
  | A3: ('a,'a right right,'b,'b left left) case


type ('st, 'c, 'fst, 'fc, 'all) status_and_case =
    <up:'up;
   mid:<left:'l;mid:'fc;right:'r>;
   down:'down;
   border:'b;
   status:'fst
  >
  constraint
    'all =
  <up:'up;
   mid:<left:'l;mid:'c ;right:'r>;
   down:'down;
   border:'b;
   status:'st
  >

type ('a,'b,'c,'d,'p) piece_move =
     White_pawn: ('a,'a up,'b,'b down, [ `pawn of white ]) piece_move

type ('p, 'me, 'opponent, 'x) move =
  <up:'up;
   mid:<left:'l;mid:'p;right:'r>;
   down:'down;
   border:'b;
   status:'opponent
  >
  constraint
    'x =
  <up:'up;
   mid:<left:'l;mid:[< `free | 'opponent not_a_king] ;right:'r>;
   down:'down;
   border:'b;
   status:'me
  >


type 'x opp = 'o constraint 'x = < opp:'o; ..>

type ('st,'c, 'p, 'fst,'fc) action =
  | Move: ('st, [< `free | 'st opp not_a_king ], 'p,  'st opp, 'p ) action
  | Win: ('st, [ `king of 'st opp ], 'p,  [ `win ], 'p ) action


type ('a,'b) game =
  | [] : ('a,'a) game
  |  (::):
      (('a, 'b, 'e, 'f) case
       * ('b,'c, ('st,'c,'fst,'fc, 'c) status_and_case,'e, 'p) piece_move
       * ('st,'c,'p,'fst,'fc) action) *
      ('start,'a) game  -> ('start, 'f) game


type half = <left:b; mid:o; right:o * (o * b)>
type vhalf = <left:b; mid:x; right:x * (x * b)>



type start =
  <up  : half dup;
   mid : <left:b; mid: [ `pawn of white ] as 'pw; right: 'pw * ('pw * b)>;
   down: half dup;
   border:halfbline;
   status:white;
  >

type 'a test_game = (start, 'a) game

let (+) (l:_ test_game) x = x :: l
