/*  --------------------------
    @file       data/sl/print-def.sl
    @details    Defines the IO functions.
*/


forall a . func print(arg: a) : int { 
    @print<<arg>>; 
    return @rc;
}

forall a . func scan(arg: a) : int { 
    @scan<<arg>>; 
    return @rc;
}
