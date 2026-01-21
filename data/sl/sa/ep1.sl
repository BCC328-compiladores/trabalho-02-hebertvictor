/*  -----------------------------
    @file       data/sl/sa/ep1.sl
    @brief      Semantical Analysis sample error program.
    @details    Invalid structs.
*/

struct A { 
    // empty struct.
} 

struct camarada { 
    x: void; 
    y: A; 
    z1: A[1]; 
    a: A[][-1][5]["ok"]; 
    b: A[];
    c: A[-1];
    d: A[5];
    e: A["ok"];
}

struct asd {
    // should this error? should it not? it is, for instance...
    z : (int, int) -> int;
}
