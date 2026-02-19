/*  --------------------------
    @file       data/sl/ex7.sl
    @details    a very normal program...
*/


struct bs_return {
    m: int;
    it: int;
}


func binary_search(a: int[], k: int) : bs_return {
    let l: int = 0;
    let r: int = a.size - 1;
    let m: int = 0;
    let i: int = 0;

    m = (l + r) / 2;
    
    while (l <= r)
    {
        m = (l + r) / 2;

        if (a[i] < k) {
            r = m + 1;

        } elif (a[i] > k) {
            l = m - 1;

        } else {
            l = a.size + 1;
        }

        i = i + 1;
    }

    let rs: bs_return = bs_return { m, i };
    return rs;
}


func main(void) : int {

    let a: int[] = [0 + 0, 23, 52, 56, 63, 66, 67, 84, 94, 97];
    let r: bs_return = binary_search(a, 56);

    print(r.m);
    print(r.it);

    return 0;
}
