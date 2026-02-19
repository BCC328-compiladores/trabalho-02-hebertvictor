/*  --------------------------
    @file       data/sl/ex6.sl
    @details    lambda~
*/



func main(void) {

    let x1: int = 7;
    let x2: int = 27;
    let x3: int = 92;
    
    let f = lambda x: int, y, z captures x1, x2, x3 returns int {
        return x1 + x2 + x3 + x + y + z;
    };
    
    print(
        eval (lambda x, y, z : int returns int { return x + y + z + 1; }) <- (1, 2, 3)
    );

    print(
        f(78, 85, 37)
    ); // 326
}
