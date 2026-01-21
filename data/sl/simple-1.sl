/*  -------------------------------
    @file       data/sl/simple-1.sl
    @details    if & elses...
*/

func main(void) : void 
{
    if (true)
    {
        let x : int = 1;
    }

    if (false)
    {
        let y : int = 1;
    }
    elif (true)
    {
        let y : int = 2;
    }
    else
    {
        let y : int = 3;
    }
    
    if (false) {
        let z : int = 3;
    } else {
        let z : int = 4;

        if (true) {
            z = 5;
        }
    }
}
