
// Problem 1 (30%)
// The five questions of this problem should be solved without the use of functions from List and Seq libraries. 
// If you use such functions in a solution to a question, then you will get no credit for that solution. 

// A menu in a restaurant comprises a list of identified courses, where each course is described by a name and a price. 
// This is modelled by: 

type Price = int
type Name = string
type Course = Name * Price
type Identifier = int
type Menu = (Identifier * Course) list

// It can be assumed that each course has its own identifier. The following is an example of a simple menu:

let m1 = [(1, ("Salad", 35)); (2, ("Soup", 30)); (3, ("Salmon", 120)); (4, ("Chicken", 60)); (5, ("Spicy Beans", 70)); (6,("Lamb", 115))];;

// 1.
// Declare a function findCourse: Identifier -> Menu -> Course, so that the value of findCourse i m is the course identified by i in the menu m. 
// If no course is identified by i in m, then the function should terminate with an exception mentioning i. 

let rec findCourse i m = 
    match m with
    | [] -> failwithf "Item %i not found in menu" i
            None
    | (id, (n, p ))::xs -> if (id = i) then Some(n, p) else findCourse i xs 


let c = findCourse 3 m1 ;; // expected result ("Salmon", 120)



// An order is given by a list of identifiers: 

type Order = Identifier list

// For the above menu, the list 
let l = [1;4;2;1;6] 
// is an order o of two salads, one chicken, one soup and one lamb, 

// 2.
// Declare a function priceOf: Order*Menu -> int, which gives the total price of an order for a given menu. 

// helper function for priceOf
let rec findPrice i m = 
    match m with
    | [] -> None // item does not exist
    | (id, (n, p ))::xs -> if (id = i) then Some p else (findPrice i xs) 

let rec priceOf ord men  = 
    match ord with
    | [] -> 0
    | x::xs -> match (findPrice x men) with
                    | None -> 0 + priceOf xs men
                    | Some p -> p + (priceOf xs men)

let p = priceOf l m1 // expected result: 275


// By a counting we understand a list of the form [(i1,c1);,(i2,c2);...;(ik,ck)] where ij is an identifier for a course and cj is a count for the number of 
// occurrences (orders) of course ij, for 1<= j <= k. 
// The identifiers occurring in a counting are all different and the order of the pairs in a counting is of no importance. 
// The counting [(1,2);(4,1);(2,1);(6,1)] corresponds to the above mentioned order o. 

type Count = int
type Counting = (Identifier * Count) list


// 3.
// Declare a function increment(i,cnt) where i is an identifier and cnt is a counting. The value of increment(i,cnt) is the counting obtained from cnt
// by incrementing the count for i by one. 
// For example, increment (6,[(1,2);(4,1);(2,1);(6,1)]) is a counting containing four pairs: (1,2), (4,1), (2,1), (6,2), and 
// increment (3,[(1,2);(4,1);(2,1);(6,1)]) is a counting containing five pairs: (1,2), (4,1), (2,1), (6,2), and (3,1). 

let rec increment (i,cnt) = 
    match cnt with
    | [] -> [(i,1)]
    | (idn,c)::xs when (i <> idn) -> (idn,c)::(increment (i,xs))
    | (idn,c)::xs -> (idn, c+1)::xs

let incr1 = increment (6,[(1,2);(4,1);(2,1);(6,1)]) // expected result: [(1, 2); (4, 1); (2, 1); (6, 2)]
let incr2 = increment (6,[])                        // expected result: [(6, 1)] 
let incr3 = increment (3,[(1,2);(4,1);(2,1);(6,1)]);; // expected result: [(1, 2); (4, 1); (2, 1); (6, 1); (3, 1)]


// 4. 
// Declare a function toCounting: Order -> Counting, that makes a counting from an order. 
// uses calls to increment (i,cnt) as accumulating list parameter to make the function tail recursive. 
// the match case
// | [], y::ys -> y::ys
// ensures that the function will return a specific value when the end of order list is reached, returning the accumulated values. 


let toCounting order =
    let rec recToCounting order acc =
        match order, acc with
        | [], [] -> []
        | [], y::ys -> y::ys
        | x::xs, _ -> recToCounting xs (increment (x, acc))
    recToCounting order [];;


let tc1 = toCounting [1;4;2;1;6]                        // Expected result: [(1, 2); (4, 1); (2, 1); (6, 2)]
let tc2 = toCounting [42;34;1;1;1;1;2;1;5;5;3;3;42;34]  // Expected result: [(42, 2); (34, 2); (1, 5); (2, 1); (5, 2); (3, 2)]


// An overview is a list of tuples: (i,n,c,p) where i is a course identifier, n is a course name, c is the count of course i,
// and p is the total price for the ordered number of course i. 
// Overviews are modelled by: 

type Overview = (Identifier * Name * Count * Price) list;;


// 5.
// Declare a function makeOverview: Counting*Menu -> Overview,
// which gives an overview for the given counting and menu. 


let rec makeOverview (cnt,men) =
    match cnt with
    | [] -> []
    | (idn,num)::xs -> match (findCourse idn men) with 
                       | None -> (idn, "", num, 0)::makeOverview(xs, men)
                       | Some (n,p) -> (idn, n, num, num*p)::makeOverview(xs, men)

let over1 = makeOverview ([(1,2);(4,1);(2,1);(6,1)], m1) // expected result: [(1, "Salad", 2, 70); (4, "Chicken", 1, 60); (2, "Soup", 1, 30); (6, "Lamb", 1, 115)]
let over2 = makeOverview ([(1,2);(4,1);(2,1);(6,2)], m1) // expected result: [(1, "Salad", 2, 70); (4, "Chicken", 1, 60); (2, "Soup", 1, 30); (6, "Lamb", 1, 230)]
let over3 = makeOverview ([(4,10);(2,10)], m1)           // expected result: [(4, "Chicken", 10, 600); (2, "Soup", 10, 300)]




// Problem 2 (15%) 
// Conside the following F# declaration: 

let rec f a xs g =
    match xs with
    | [] -> a
    | x::xr -> f (g a x) xr g;;

// 1.
// Give the type of f.

(* 
    val f: 'a -> 'b list -> ('a -> 'b -> 'a) -> 'a 
*)

// 2.
// Give a step by step evaluation (using ~>) for the expression f a [x0; x1; x2] g, where:
// - there should be at least one step for every recursive call of f and
// - the last expression of the evaluation does not contain f. 

(*
    f a [x0; x1; x2] g ~>  f (g a x0) [x1; x2] g

    f (g a x0) [x1; x2] g ~>  f (g (g a x0) x1) [x2] g

    f (g (g a x0) x1) [x2] g ~>  f (g (g (g a x0) x1) x2) [] g 

    f (g (g (g a x0) x1) x2) [] g ~>  (g (g (g a x0) x1) x2)

*)

// 3. 
// Give three values v1, v2 and v3 so that the expression f v1 v2 v3 has type int*string.

let v1 = (0,"a")
let v2 = [1;2;1;2]
let v3 = (fun (x,y) z -> (x+z,y));;

let ftest = f v1 v2 v3;; // expected result: (6,"a")


// 4.
// Declare a function h x y that gives the infinite sequence with elements x*y - i*(x-y),
// where i = 0,1,2,3,...  . The identifier i is the index of the elements of the sequence. 
// Give the type of h. 

let h x y = seq{ yield! Seq.initInfinite(fun i -> x*y - i*(x-y))}
let htest = h 3 5
Seq.item 100 htest;;

(*
    val h: int -> int -> seq<int> 
*)




// Problem 3 (30%) 
// We now consider the types Exp and Pat:

type Exp = | Const of int  
           | Var of string
           | Add of Exp * Exp;;

type Pat = | PConst of int
           | PVar of string 
           | PAdd of Pat * Pat;;

// Values of type Exp are called expressions and values of type Pat are called patterns. 
// Expressions are constructed from constants (constructor Const) and variables (constructor Var) 
// using a constructor Add to form additions of two expressions. Patterns are constructed in a similar fashion. 

// 1.
// Declare a function vars: Pat -> string list that gives the list of those strings s that appear as PVar s in a pattern. 
let exp1 = Add(Add(Const 2, Var "x"),Add(Var "z", Const 5))

let pat1 = PAdd(PAdd(PConst 2, PVar "x"),PVar "y")
let pat2 = PAdd(PAdd(PConst 2, PVar "x"),PAdd(PVar "z", PVar "x"))

let rec vars pat = 
    match pat with
    | PConst i -> []
    | PVar s -> [s]
    | PAdd (pat1,pat2) -> vars(pat1)@vars(pat2)

let vars1 = vars pat1   // expected result: ["x"; "y"]
let vars2 = vars pat2   // expected result: ["x"; "z"; "x"]

// 2.
// A pattern p is illegal if the same string occurs multiple times as PVar s in p. Declare a function
// legal: Pat -> bool
// that gives value false for illegal patterns and otherwise the value true. 

let legal pat =
    let rec legal_helper strlist = 
        match strlist with
        | [] -> true
        | x::xs -> if not (List.contains x xs) then (legal_helper xs) else false
    legal_helper (vars pat)

let legal1 = legal pat1     // expected result: true
let legal2 = legal pat2     // expected result: false
        

// The types Binding and BindingList are declared as follows: 

type Binding = string*Exp
type BindingList = Binding list

// A pattern p can match an expression e, and, if this is the case, a list of bindings is formed according to the following rules: 
    // a. Pattern PConst n1 matches expression Const n2 when n1 = n2 and no binding is formed. 
    // b. Pattern PVar s matches any expression e forming the list [(s,e)] containing one binding. 
    // c. Pattern PAdd(p1, p2) matches expression Add(e1, e2) when p1 matches e1 and p2 matches e2,
    // and the list of bindings formed consists of all bindings formed by matching p1 with e1 and p2 with e2. 
    // d. Any matching of a pattern with an expression can be obtained by repeated use of the above rules a, b and c. 

 // 3. 
 // Declare a function
 // patMatch: Pat*Exp -> BindingList option 
 // where patMatch(p,e) = None if p does not match e and patMatch(p,e) = Some bs if 
 // p matches e and bs is a list of bindings formed when matching p with e. 



let rec patMatch (pat,exp) = 
    let rec patMatchBool (pat,exp) = 
        match (pat,exp) with
        | PAdd(p1, p2) , Const n -> false
        | PConst n, Var s -> false
        | PAdd(p1, p2), Var s -> false
        | PConst n, Add(e1, e2) -> false
        | PConst n1, Const n2 -> (n1 = n2)
        | PVar s, _ -> true
        | PAdd(p1,p2), Add(e1,e2) -> (patMatchBool(p1,e1)&&patMatchBool(p2,e2))
    let rec getPatMatch (pat,exp) = 
        match (pat,exp) with
        | PAdd(p1, p2) , Const n -> []
        | PConst n, Var s -> []
        | PAdd(p1, p2), Var s -> []
        | PConst n, Add(e1, e2) -> []
        | PConst n1, Const n2 -> []
        | PVar s, e -> [(s, e)]
        | PAdd(p1,p2), Add(e1,e2) -> (getPatMatch(p1,e1)@getPatMatch(p2,e2))
    if (patMatchBool(pat,exp)) then Some (getPatMatch (pat, exp)) else None

let bindinglist1 = patMatch (pat2, exp1);;    // expected result: Some [("x", Var "x"); ("z", Var "z"); ("x", Const 5)]

 // We shall now consider a revision of the types Pat and Exp in order to support infix operators in general and not just addition. 
 // In particular, we'd like to replace Add by a new constructor InfixOp so that 
 // InfixOp(InfixOp(Const 2, "+", Var "x"), "-", Var "y"), for example, becomes a value of the revised expression type.

 // Similarly, PAdd should be replaced by a new constructor PInfixOp so that, for example, 
 // PInfixOp(PInfixOp(PConst 2, "+", PVar "x"),"-",PVar "y") becomes a value of the revised pattern type.
 // Furthermore, the matching rule relating to the new constructors is: 

 // c'. Pattern PInfixOp(p1, o, p2) matches expression InfixOp(e1, o', e2) when o = o', p1 matches e1 and p2 matches e2,
 // and the list of bindings formed consists of all bindings formed by matching p1 with e1 and p2 with e2. 

 // The following two questions concern some of the revisions needed to accommodate infix operators: 
 // 4. 
 // Show the revised type declarations for Pat and Exp

type Exp' = | Const of int  
            | Var of string
            | InfixOp of Exp' * string * Exp'

type Pat' = | PConst of int
            | PVar of string 
            | PInfixOp of Pat' * string * Pat'

let exp'_1 = InfixOp(InfixOp(Const 2, "+", Var "x"), "-", Var "y")
let pat'_1 = PInfixOp(PInfixOp(PConst 2, "+", PVar "x"),"-",PVar "y")


 // 5.
 // Show your revised declaration for patMatch. 

let rec newPatMatch (pat,exp) = 
    let rec patMatchBool (pat,exp) = 
        match (pat,exp) with
        | PInfixOp(p1,o,p2) , Const n -> false
        | PConst n, Var s -> false
        | PInfixOp(p1,o,p2), Var s -> false
        | PConst n, InfixOp(e1,o',e2) -> false
        | PConst n1, Const n2 -> (n1 = n2)
        | PVar s, _ -> true
        | PInfixOp(p1,o,p2), InfixOp(e1,o',e2) -> ((o=o')&&(patMatchBool(p1,e1)&&patMatchBool(p2,e2)))
    let rec getPatMatch (pat,exp) = 
        match (pat,exp) with
        | PInfixOp(p1,o,p2) , Const n -> []
        | PConst n, Var s -> []
        | PInfixOp(p1,o,p2), Var s -> []
        | PConst n, InfixOp(e1,o',e2) -> []
        | PConst n1, Const n2 -> []
        | PVar s, e -> [(s,e)]
        | PInfixOp(p1,o,p2), InfixOp(e1,o',e2) -> (getPatMatch(p1,e1)@getPatMatch(p2,e2))
    if (patMatchBool(pat,exp)) then Some (getPatMatch (pat, exp)) else None

let bindinglist2 = newPatMatch (pat'_1, exp'_1);;   // expected result: Some [("x", Var "x"); ("y", Var "y")]



// Problem 4
// Consider the following F# declaration: 
