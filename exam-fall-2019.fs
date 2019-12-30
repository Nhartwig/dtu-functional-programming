
// Problem 1 
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
    | (idn,c)::xs when (i = idn) -> (idn, c+1)::xs

let incr1 = increment (6,[(1,2);(4,1);(2,1);(6,1)]) // expected result: [(1, 2); (4, 1); (2, 1); (6, 2)]
let incr2 = increment (6,[])                        // expected result: [(6, 1)] 
let incr3 = increment (3,[(1,2);(4,1);(2,1);(6,1)]) // expected result: [(1, 2); (4, 1); (2, 1); (6, 1); (3, 1)]




