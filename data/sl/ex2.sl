/*  --------------------------
    @file       data/sl/ex2.sl
    @details    Structures and arrays.
*/


struct Person {
    name    : string;
    age     : int;
    height  : float;
}


func main() : void {
    // arranjo
    let people : Person[3];
    people[0] = Person{ "Alice", 25, 1.65 };
    people[1] = Person{ "Bob", 30, 1.80 };
    people[2] = Person{ "Charlie", 35, 1.75 };

    // iteração sobre arranjo
    let i : int = 0;
    while (i < 3) {
        print(people[i].name);
        print(people[i].age);
        print(people[i].height);

        i = i + 1;
    }
}
