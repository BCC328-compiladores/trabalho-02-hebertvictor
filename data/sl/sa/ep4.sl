/*  -----------------------------
    @file       data/sl/sa/ep4.sl
    @brief      Semantical Analysis sample error program.
    @details    Unexpected return type.
*/

func coisa_doida(void) {
    return 1; // esse ainda é argumentável com inferência de tipos...
}

func negocio_maluco(void) : int {
    return "claramente errado";
}

func objeto_delirado(void) : void {
    return 1;
}

func nothing(void) : void {
    // this one is ok.
}

func nothing2(void) {
    // this is as well.
}

func nothing3(void) {
    // so is this.
    return;
}
