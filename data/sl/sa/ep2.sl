/*  -----------------------------
    @file       data/sl/sa/ep2.sl
    @brief      Semantical Analysis sample error program.
    @details    Multiple symbol definition.
*/

func asd(void) { 
    let x : int;
}

func asd(void) {
    let y: int;
}
 
struct A {
    campo_interessante : int;
}

struct A {
    campo_interessante2 : int;
}

func A(void) {
    
}

func main(argsc: int, argsv : string[]) : int { 
    let x : int = 1; 
    
    while (x < 5) { 
        ++ x; 
    }
    
    return x ++; 
}

