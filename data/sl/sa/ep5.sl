/*  -----------------------------
    @file       data/sl/sa/ep5.sl
    @brief      Semantical Analysis sample error program.
    @details    Variable re-definition.
*/

func tudo_certo(x : int, x: int) : int { 
    let x = 1; 
    let x : int = 2; 
    let x : float = 3.0;
    
    x = x + 1;
    return "ok"; 
}

// @TODO adicionar mais.
