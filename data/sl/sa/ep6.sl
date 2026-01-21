/*  -----------------------------
    @file       data/sl/sa/ep6.sl
    @brief      Semantical Analysis sample error program.
    @details    Wrong type attributions.
*/

struct A { 
    x: int[2][2]; 
} 

struct B { 
    x: int;
    y : A; 
} 


func atribuicao_certa(void) : void {
    let h : B;

    h.x         = 1;
    h.y         = A { [[1, 2], [3, 4]] };
    h.y.x       = [[5, 6], [7, 8]];
    h.y.x[0]    = [1, 2];
    h.y.x[0][0] = 1;
}


func atribuicao_errada_1(void) : void { 
    let x : int = 1; 
    let y : float = x; 

    x = "chupando" + " uma " + "manga";
    y = 1.57 * "INFINITO";
}


func atribuicao_errada_2(void) : void { 
    let h : B;

    h.x = 1.0;

    h.y = [1, 4];
    h.y = "deixa atribuir errado vai";
    h.y = 1.0;

    h.y.x[0][0][0] = 1;
}
