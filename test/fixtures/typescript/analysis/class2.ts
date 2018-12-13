class Adder<A> {
    summand: number;
    constructor(summand: number) {
        this.summand = summand;
    }
    add<A>(foo: A) {
        return foo
    }
}

export { Adder }
