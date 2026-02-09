/*  -----------------------------
    @file       data/sl/sa/ep9.sl
    @brief      Semantical Analysis sample error program.
    @details    lambdas...
*/

func ok_behavior(void) {

    let x1: int = 7;
    let x2: int = 27;
    let x3: int = 92;
    
    let f = lambda x: int, y, z captures x1, x2, x3 returns int {
        return x1 + x2 + x3 + x + y + z;
    };
    
    print(
        eval (lambda x : int returns int { return x + 1; }) <- (1, 2, 3)
    );

    print(
        f(78, 85, 37)
    ); // 326


    let g = lambda x, y returns string {
        return x + y;
    };

    print(g("beleza; ", "tudo certo"));
}

func not_so_ok_behavior(void) {

    let x1: int = 7;
    let x2: int = 27;
    let x3: int = 92;
    
    let f = lambda x: int, y, z captures x1, x2, x3 returns int {
        return x1 + x2 + x3 + x + y + z;
    };
    
    print(
        eval (lambda x : int returns int { return x + 1; }) <- (1, 2, 3)
    );


    // invalid args, by cardinality...
    print(f());
    print(f(1));
    print(f(1, 2));
    print(f(78, 85, 37, 400));

    // invalid args, by type...
    print(f(1.0, "tudo certo", 2));
}

