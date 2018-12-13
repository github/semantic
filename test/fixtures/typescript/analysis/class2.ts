class Adder {
    summand: number;
    constructor(summand: number) {
        this.summand = summand;
    }
    add() {
        return 4 + this.summand;
    }
}

export { Adder }
