/*  --------------------------
    @file       data/sl/ex8.sl
    @details    sla
*/

struct doideira {
    negocio  : float;
    negocio2 : int;
}

func coisa_doida1(x, y, z) : int {
    return x + y + z;
}

forall a b c . func coisa_doida2(x: a, y: b, z: c) : int {
    return x + y + z;
}

func main(void) : void {
    print(23);
    print(coisa_doida1(1, 2, 3));
    print(coisa_doida2(1, 2, 3));
}
