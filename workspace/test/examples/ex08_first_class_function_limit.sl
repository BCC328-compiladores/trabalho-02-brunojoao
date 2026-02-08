forall a b . func apply(f : (a) -> b, x : a) : b {
    return f(x);
}

func inc(x : int) : int {
    return x + 1;
}

func main() : void {
    let y : int = apply(inc, 10);
    print(y);
}
