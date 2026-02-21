/*  -----------------------------
    @file       data/sl/sa/ep8.sl
    @brief      Semantical Analysis sample error program.
    @details    More on types.
*/

struct Aluno {
    idade: int;
}


func A(idade : int) : Aluno {
    return Aluno { idade };
}


// higher-order function gotta pass the semantical analysis.
forall a b . func map(f: (a) -> b, l : a[]) : b[] 
{
    let i : int = 0;
    let n : b[] = new b[l.size];

    while (i < l.size)
    {
        n[i] = f(l[i]);
    }
    
    return n;
}


func SOMA(x: int, y: int) : int {
    return x + y;
}


// tests~
func arrays(void)
{
    // ok
    let w : int[4] = [1, 2, 3, 4]; // static instance.
    let x : int[] = new int[4 + 1]; // dynamic instance.
    let y : int[] = [1, 2, 3]; // @TODO This will infer x's capacity.

    // errors
    let z : int[]   = [];
    let z2 : int[]  = [1, 1.0, "banana"]; // non-homogenous.
    let z3 : int[]  = [1.0, 2.0, 3.0];

    let z4 : int[]  = new int[];
    let z5 : int[]  = new int;
}


func structs(void)
{
    let a : Aluno = Aluno{ 5 };
    let b : Aluno = A(5);
    
    Aluno(47, 55, 44);
}


func functions(void)
{
    // @TODO move this to a more contextual src file.
    function_not_defined(5, 5, 5);

    // wrong number of arguments.
    let x : int = SOMA(1 + 1, 2 + 5 * 6 ** 2, 3);
    
    // wrong types on fcall.
    let x : int = SOMA(1.0, 2.0);
}


func main(void) : void
{

}
