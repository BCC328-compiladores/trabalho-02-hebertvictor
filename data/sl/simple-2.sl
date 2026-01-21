/*  -------------------------------
    @file       data/sl/simple-2.sl
    @details    structure and array accessing...
*/

struct doidera {
    x : int;
    y : int;
    z : int[3];
}

func main(void) : void 
{
    let d = doidera { 1, 1, [ 1, 2, 3 ] };

    print(d.x);
    print(d.y);
    print(d.z[0]);
}
