/*  -----------------------------
    @file       data/sl/sa/ep10.sl
    @brief      Semantical Analysis sample error program.
    @details    ~
*/

func sla(void) : void {

    for (let i : int = 0; i < 100; i = i + 1) {
        let k : int = 0;
        print(i * 2 + 17 + 8);

        if (false) 
        {
            //let victor_xaviver_costa1 : int = 0;
            print("tomar no cú");
        } 
        else 
        {
            //let victor_xaviver_costa2 : int = 0;
            print("tomar na bunda");
        }
    }
    

    let y: int = 5;
    let z: int = 8;

    let f = lambda x captures y, z returns int { 
        return x + y + z; 
    };

    print(f(1));
}