class Adder {
    summand: number;

    constructor(summand: number) {
        this.summand = summand;
    }

    addOne() {
      this.summand += 1;
      return this.summand;
    }
}

var foo = new Adder(5)
foo.addOne()
