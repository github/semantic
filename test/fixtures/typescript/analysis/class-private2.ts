class Adder {
    summand: number;
    constructor(summand: number) {
        this.summand = summand;
    }
    private private_add() {
      return 0 + this.summand;
    }
}

export { Adder }
