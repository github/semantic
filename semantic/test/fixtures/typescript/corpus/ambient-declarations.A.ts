declare class Error {
  constructor: Function
}

declare var foo: number;

declare function greet(greeting: string): void;

declare namespace myLib {
    function makeGreeting(s: string): string;
    let numberOfGreetings: number;

    interface LogOptions {
      verbose?: boolean;
    }
    interface AlertOptions {
      modal: boolean;
      title?: string;
      color?: string;
    }
}

declare class Greeter {
  constructor(greeting: string);

  greeting: string;
  showGreeting(): void;
}
