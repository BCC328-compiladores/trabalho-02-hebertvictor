/*  -----------------------------
    @file       data/sl/sa/ep3.sl
    @brief      Semantical Analysis sample error program.
    @details    No-explicit return.
*/

func doidera(void) : float {
    let x: int = print(1.0); x = x + 1;
    // no return.
}

// main expects an int return.
func main(void) : int { 
    let x : int = 1; 
    let y : int = 2; 
    let z : int = SOMA(x, y) ** 5; 
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

func SOMA(a : int, b : int) : int {
    return a + b;
}