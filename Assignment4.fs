// T-501-FMAL, Spring 2021, Assignment 4

(*
STUDENT NAMES HERE: Eva Sol Petursdottir and Halla Margret Jonsdottir


*)

module Assignment4

// Problem 1

(*
ANSWER 1 HERE:
    (i) g(1, 2) prints 2, h(1, 2) prints 2

   (ii) g(1, 0) prints 0, h(1, 0) prints 0

  (iii) g(0, 0) prints 0, h(0, 0) prints 50
        The function f only changes the first argument t* if the value of the address that t* points to is equal to 0.

        g(0,0) will change the first argument 0 (int x) to 50 since g calls twice on function f.
        But since the function g prints out y in the end, it prints out 0 since y is never changed.

        h(0,0) first initializes a pointer p* that points to the address of x or the first 0.
        Then h calls the function f with the pointer p and the address of y as arguments (address of x and address of y).
        In the function f *t is the first 0 (int x) and since *t is equal to 0 then the function changes the value of *t to 50,
        so now int x has gotten the value 50.
        After f has changed the value, it returns u, which is the second argument, the address of y (the second 0).
        
       The pointer p gets then assigned to the return value from function f, which is the address of y.

       Now we are in the 4th line of the function h and call f again, now with arguments p and &y,
       but since p just got assigned to the address of y, the arguments are the same address.

       Now we are in function f with the arguments address of y (which is of value 0) in both first and second argumnet.
       Since *t, the value of pointer p, is equal to 0, then we change the value *t to equal 50. Then we return u, but nothing get assigned to that value.
       Then at last, we print y, which is new 50 since in the last f function we changed the value of *t, which pointed to y, to 50.
*)


// Abstract syntax
type expr =
    | Access of access            // a
    | Addr of access              // &a
    | Num of int                  // n
    | Op of string * expr * expr  // e1 op e2
and access =
    | AccVar of string            // x
    | AccDeref of expr            // *p
and stmt =
    | Alloc of access * expr        // p = alloc e
    | Print of expr               // print e
    | Assign of access * expr       // p = e
    | TestAndSet of expr * expr   // test_and_set(p, q)
    | Call of string * expr list  // f(e1, ..., en)
    | Block of stmt list          // { stmt1; ...; stmtN }
    | If of expr * stmt * stmt    // if (e) e1 else e2
    | While of expr * stmt        // while (e) stmt
and fundec =
    string *                      // function name
    string list *                 // argument names
    string list *                 // local variable names
    stmt                          // function body
and program =
    | Prog of fundec list

// Examples of concrete and abstract syntax

// void main () {
//   var p;
//   p = alloc(2);
//   *(p + 1) = 11;
//   print(*(p + 1));
// }
let ex =
  ("main", [], ["p"], Block [
    Alloc (AccVar "p", Num 2);
    Assign (AccDeref (Op ("+", Access (AccVar "p"), Num 1)), Num 11);
    Print (Access (AccDeref (Op ("+", Access (AccVar "p"), Num 1))))
  ])

// void make_range(dest_p, lower, upper) {
//   var i;
//   *dest_p = alloc((upper - lower) + 1);
//   while (lower <= upper) {
//     *((*dest_p) + i) = lower;
//     i = i + 1;
//     lower = lower + 1;
//   }
// }
let make_range =
  ("make_range", ["dest_p"; "lower"; "upper"], ["i"], Block [
    Alloc (AccDeref (Access (AccVar "dest_p")), Op ("+", Op ("-", Access (AccVar "upper"), Access (AccVar "lower")), Num 1));
    While (Op ("<=", Access (AccVar "lower"),  Access (AccVar "upper")), Block [
      Assign (AccDeref (Op ("+", Access (AccDeref (Access (AccVar "dest_p"))), Access (AccVar "i"))), Access (AccVar "lower"));
      Assign (AccVar "i", Op ("+", Access (AccVar "i"), Num 1));
      Assign (AccVar "lower", Op ("+", Access (AccVar "lower"), Num 1))
    ])
  ])



// Problem 2

// void print_array(a, length) {
//   var i;
//   while (i < length) {
//     print(*(a + i));
//     i = i + 1;
//   }
// }
let print_array =
  ("print_array", ["a"; "length"], ["i"], Block [
    While (Op ("<", Access (AccVar "i"), Access (AccVar "length")), Block [
      Print(Access (AccDeref (Op ("+", Access (AccVar "a"), Access (AccVar "i")))))
      Assign (AccVar "i", Op ("+", Access (AccVar "i"), Num 1));
    ])
  ])

// void memcpy(dest, src, length) {
//   while (length) {
//     *dest = *src;
//     dest = dest + 1;
//     src = src + 1;
//     length = length - 1;
//   }
// }
let memcpy =
  ("memcpy", ["dest"; "src"; "length"], [], Block [
    While (Op ("!=", Access (AccVar "length"), Num 0), Block[
      Assign (AccDeref (Access (AccVar "dest")), Access (AccDeref (Access (AccVar "src"))));
      Assign (AccVar "dest", Op ("+", Access (AccVar "dest"), Num 1));
      Assign (AccVar "src", Op ("+", Access (AccVar "src"), Num 1));
      Assign (AccVar "length", Op ("-", Access (AccVar "length"), Num 1));
    ])
  ])

// void make_copy(dest_p, src, length) {
//   *dest_p = alloc(length);
//   memcpy(*dest_p, src, length);
// }
let make_copy =
  ("make_copy", ["dest_p"; "src"; "length"], [], Block [
    Alloc (AccDeref (Access (AccVar "dest_p")), Access (AccVar "length"));
    Call ("memcpy", [Access(AccDeref (Access (AccVar "dest_p"))); Access (AccVar "src"); Access (AccVar "length")]);
  ])


// Problem 3


// (i)
// void array_to_list(dest_p, a, length) {
//   var cur;
//   *dest_p = 0;
//    While (length) {
//      length = length - 1;  
//      cur = alloc(2);
//      *cur = *(a + length); 
//      *(cur + 1) = *dest_p;
//      *dest_p = cur;
//     }
// }

let array_to_list =
  ("array_to_list", ["dest_p"; "a"; "length"], ["cur"], Block [
    Assign (AccDeref (Access (AccVar "dest_p")), Num 0);
    While (Access (AccVar "length"), Block [
      Assign (AccVar "length", Op ("-", Access (AccVar "length"), Num 1));
      Alloc (AccVar "cur", Num 2);
      Assign (AccDeref (Access (AccVar "cur")), Access (AccDeref (Op ("+", Access (AccVar "a"), Access (AccVar "length")))));
      Assign (AccDeref (Op ("+", Access (AccVar "cur"), Num 1)), Access (AccDeref (Access (AccVar "dest_p"))));
      Assign (AccDeref (Access (AccVar "dest_p")), Access (AccVar "cur"))
    ])
  ])

// (ii)
let print_list =
  ("print_list", ["l"], ["p"], Block [
    // COMPLETE THIS
    // (You may want to add local variables by changing the empty list
    // above.)
    Assign (AccVar "p", Access (AccVar "l"));
    While (Op ("!=", (Access (AccVar "p")), Num 0), Block[
      Print(Access (AccDeref (Access (AccVar "p"))))
      Assign (AccVar "p", Access (AccDeref(Op ("+", Access (AccVar "p"), Num 1))));
    ])
  ])



// Various definitions used in the interpreter

type 'data envir = (string * 'data) list
let rec lookup env x =
    match env with
    | []          -> failwith (x + " not found")
    | (y, v)::env -> if x = y then v else lookup env x

type locEnv = int envir * int
type funEnv = (string list * string list * stmt) envir

type address = int
type store = Map<address,int> * int
let emptyStore = (Map.empty<address,int>, 1)
let setSto ((map, nextloc) : store) addr value = map.Add (addr, value), nextloc
let getSto ((map, nextloc) : store) addr = map.Item addr
let allocSto ((map, nextloc) : store) length =
  let r = nextloc
  let nextloc' = r + length
  let map' = List.fold (fun (m : Map<address,int>) addr -> m.Add (addr, 0)) map [r..(nextloc' - 1)]
  let sto' = map', nextloc'
  sto', r

let bindVar x v (env, nextloc) sto : locEnv * store =
    let env' = (x, nextloc) :: env
    ((env', nextloc - 1), setSto sto nextloc v)
let rec bindVars xs vs locEnv sto : locEnv * store =
    match xs, vs with
    | [], []       -> locEnv, sto
    | x::xs, v::vs ->
        let locEnv', sto' = bindVar x v locEnv sto
        bindVars xs vs locEnv' sto'
    | _ -> failwith "parameter/argument mismatch"

let initFunEnv (fundecs : fundec list) : funEnv =
    let rec addv decs funEnv =
        match decs with
        | [] -> funEnv
        | (f, parameters, locals, body) :: decr ->
            addv decr ((f, (parameters, locals, body)) :: funEnv)
    addv fundecs []



// Interpreter
let rec eval (e : expr) (locEnv : locEnv) (funEnv : funEnv) (sto : store) : int =
    match e with
    | Access acc      -> getSto sto (access acc locEnv funEnv sto)
    | Num i           -> i
    | Addr acc        -> access acc locEnv funEnv sto
    | Op (op, e1, e2) ->
        let i1 = eval e1 locEnv funEnv sto
        let i2 = eval e2 locEnv funEnv sto
        match op with
        | "*"  -> i1 * i2
        | "+"  -> i1 + i2
        | "-"  -> i1 - i2
        | "/"  -> i1 / i2
        | "%"  -> i1 % i2
        | "==" -> if i1 =  i2 then 1 else 0
        | "!=" -> if i1 <> i2 then 1 else 0
        | "<"  -> if i1 <  i2 then 1 else 0
        | "<=" -> if i1 <= i2 then 1 else 0
        | ">=" -> if i1 >= i2 then 1 else 0
        | ">"  -> if i1 >  i2 then 1 else 0
        | _    -> failwith ("unknown primitive " + op)
and access acc locEnv funEnv (sto : store) : int =
    match acc with
    | AccVar x   -> lookup (fst locEnv) x
    | AccDeref e -> eval e locEnv funEnv sto
and evals es locEnv funEnv (sto : store) : int list =
    List.map (fun e -> eval e locEnv funEnv sto) es
and callfun f es locEnv (funEnv : funEnv) (sto : store) : store =
    let _, nextloc = locEnv
    let paramNames, localNames, fBody = lookup funEnv f
    let arguments = evals es locEnv funEnv sto
    // local variables are initialized to 0
    let localValues = List.map (fun _ -> 0) localNames
    let fBodyEnv, sto' = bindVars (paramNames @ localNames) (arguments @ localValues) ([], nextloc) sto
    exec fBody fBodyEnv funEnv sto'



// Problem 4

and exec stm (locEnv : locEnv) (funEnv : funEnv) (sto : store) : store =
    match stm with
    | Print e ->
        let res = eval e locEnv funEnv sto
        printf "%d " res;
        sto
    | Call (f, es) -> callfun f es locEnv funEnv sto
    | Assign (acc, e) ->
        let loc = access acc locEnv funEnv sto
        let res = eval e locEnv funEnv sto
        setSto sto loc res
    | TestAndSet (p, q) ->
        failwith "not implemented"
        // let *p equal *q
        // let *q equal 1
    | Alloc (acc, e) ->
        let loc = access acc locEnv funEnv sto
        let n = eval e locEnv funEnv sto
        let sto', res = allocSto sto n
        setSto sto' loc res
    | Block stms ->
        List.fold (fun sto' s -> exec s locEnv funEnv sto') sto stms
    | If (e, stm1, stm2) ->
        let v = eval e locEnv funEnv sto
        if v <> 0 then exec stm1 locEnv funEnv sto
                  else exec stm2 locEnv funEnv sto
    | While (e, body) ->
        let rec loop sto =
            let v = eval e locEnv funEnv sto
            if v <> 0 then loop (exec body locEnv funEnv sto)
                    else sto
        loop sto

// Run a complete program
let run (Prog topdecs) vs =
    let funEnv = initFunEnv topdecs
    let locEnv = ([], System.Int32.MaxValue)
    let sto = emptyStore
    callfun "main" [] locEnv funEnv sto



// Problem 5

(* ANSWER 5 HERE
    (i) This prints 10 because
    The memory allocation statement a = alloc(e), allocates space for an array of length e, 
    initialize all of the values in the array to 0, and set a to the address of the first value.

    p and q gets allocated in momeory with the alloc memory statement.
    So p is allocated space for an array of length 1.
    Then after p has been allocated space, q is also allocated space for an array of length 1. 
    Since q is allocated after p, q is straight after p in memory.
    
    In line 4 in the main(), *(q - 1) = 10;, the address (q-1) (which is the same as p) is assigned to 10.
    So the value of the address p is now set to 10.

    Lastly, we print out the value that is stored at location p, which is 10.
   

   (ii) This prints 0 because ...
   In main(), a is assigned to the value 1234.
   Then we call the function f, in which we go through a while loop, that loops through memory addresses
   while the dereference of the address is now equal to 1234. 
   So when, the dereference of the address is equal to 1234, we have found the momory address that stores a.

   Then, the function f changes the value that is located in the address *(&i + i), which dereference is equal to 1234 or a, to 0.
   So the line *(&i + i) = 0; is changing the value of a to 0.

   Lastly, a is printed which has now been assigned to the value 0, so 0 is printed.
  
  *)

// void main() {
//   var p, q;
//   p = alloc(1);
//   q = alloc(1);
//   *(q - 1) = 10;
//   print(*p);
// }
let prog5i =
  Prog (
    [ ("main", [], ["p"; "q"], Block [
        Alloc (AccVar "p", Num 1);
        Alloc (AccVar "q", Num 1);
        Assign (AccDeref (Op ("-", Access (AccVar "q"), Num 1)), Num 10);
        Print (Access (AccDeref (Access (AccVar "p"))))
      ])
    ])

// void f() {
//   var i;
//   while (*(&i + i) != 1234) {
//     i = i + 1;
//   }
//   *(&i + i) = 0;
// }
// void main() {
//   var a, b, c, d;
//   a = 1234;
//   f();
//   print a;
// }
let prog5ii =
  Prog (
    [ ("f", [], ["i"], Block [
        While (Op ("!=", Access (AccDeref (Op ("+", Addr (AccVar "i"), Access (AccVar "i")))), Num 1234), Block [
          Assign (AccVar "i", Op ("+", Access (AccVar "i"), Num 1))
        ]);
        Assign (AccDeref (Op ("+", Addr (AccVar "i"), Access (AccVar "i"))), Num 0)
      ])
    ; ("main", [], ["a"; "b"; "c"; "d"], Block [
        Assign (AccVar "a", Num 1234);
        Call ("f", []);
        Print (Access (AccVar "a"))
      ])
    ])


