/*  --------------------------
    @file       data/sl/ex5.sl
    @details    type inference & template~
*/


func id(x) {
    return x;
}


forall a b . func map (f: (a) -> b, v: a[]) : b[] {
    let result = new b[v.size];
    
    for (let i : int = 0; i < v.size; ++ i) {
        result[i] = f(v[i]);
    }

    return result;
}


func a_real_nothing(x : int) : float {
    if (x >= 5) {
        return 1.0;
    }

    return 0.0;
}


func main(void) : void {
    
    // identity~
    print(id(5.0));
    print(id(5));
    print(id("olá"));

    // map~
    let asd : int[] = [ 3, 1, 4, 1, 5 ];
    let new_asd : float[] = map(a_real_nothing, asd);
    
    for (let j : int = 0; j < new_asd.size; ++ j) {
        print(new_asd[j]);
    }
}
