/*  --------------------------
    @file       data/sl/ex1.sl
    @details    Function def. & call example.
*/


func factorial(n: int) : int {
    if (n <= 1) {
        return 1;
    } else {
        return n * factorial(n - 1);
    }
}


func main() : int {
    let result : int = factorial(5);
    print(result); // deve imprimir 120
    return 0;
}
