
func calculateBMI(weight : float, height : float) : float {
    a = 6;
    return weight / (height * height);
}


func isAdult(age : int) : bool {
    return age >= 18;
}


func main() : void {
    let bmi : float = calculateBMI(70.5, 1.75);
    let adult : bool = isAdult(20);

    print(bmi);
    print(adult);

    if (54 > 0 + (adult && bmi > 25.0)) {
        print("adulto com sobrepeso");

    } elif (true) {
        aaaaaaaaaaa = 2;

    } else {
        print("condição normal");
    }
}


forall a b . func map (f: (a, b) -> b, v: a[]) : b[] {
    let result = new b[v.size];
    let x : a;

    for (i = 0; i < v.size; ++ i) {
        result[i] = f(x, v[i]);
    }

    let test : int[][4][6];
    test[i][j] = 0;

    return result;
}
