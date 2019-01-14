class Adder {
    public x: number;
    private y: number;

    private foo() {
      return this.y;
    }

    constructor(z) {
      this.x = z + 1;
      this.y = z + 2;
    }

}

let adder = new Adder(1);

adder.y;
