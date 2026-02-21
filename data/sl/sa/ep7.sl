/*  -----------------------------
    @file       data/sl/sa/ep7.sl
    @brief      Semantical Analysis sample error program.
    @details    Invalid accesses.
*/

struct A { 
    x: int[2][2]; 
} 

struct B { 
    x: int;
    y : A; 
} 


func vo_accessar_tudo_certo(void) : void {
    let h : B;

    // ok.
    print(h.x);

    // ok.
    print(h.y);

    // ok.
    print(h.y.x);

    // ok.
    print(h.y.x[0]);

    // ok.
    print(h.y.x[0].size);

    // ok.
    print(h.y.x[0][0]);
}

func vo_accessar_tudo_errado(void) : void {
    let h : B;
    
    // invalid field access.
    print(h.campo);
    print(h.campo_todo);
    print(h.campo_todo_errado);
    print(h.size);

    // undefined variables.
    H = 1;
    this_one = is_also + undefined;

    //
    B = B + 1;


    h.y.x.camarada              = 1;
    h.y.x.ok.ok                 = 1;
    h.y.x[0][0].coisa_doida     = 1;
}

