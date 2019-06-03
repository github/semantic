# Program analysis

Program analysis allows us to ask questions about and analyze the behavior of computer programs. Analyzing this behavior allows us to (eventually) answer subtle but powerful questions such as, will this use more than 8 GB of RAM? Does this present a user interface? We perform program analysis statically—that is, without executing the program.

We’re able to compute the following end results using evaluation:
1. **Import graph:** graph representing all dependencies (`import`s, `require`s, etc.)
2. **Call graph:** a control flow graph that represents calling relationships (ie., how one particular function calls other functions). This information is often vital for debugging purposes and determining where code is failing.
3. **Control flow graph:** representation of _all_ paths that might be traversed through a program during its execution.

### Abstract interpretation
To do program analysis, we implement an approach based on the paper [Abstracting Definitional Interpreters](https://plum-umd.github.io/abstracting-definitional-interpreters/), which we've extended to work with our [à la carte representation of syntaxes](http://www.cs.ru.nl/~W.Swierstra/Publications/DataTypesALaCarte.pdf). This allows us to build a library of interpreters that do different things, but are written with the _same_ evaluation semantics. This approach offers several advantages; we can define _one_ evaluator and get different behaviors out of it (via type-directed polymorphism).

We employ three types of interpretation: *concrete semantics*, *abstract semantics* and *type-checking*.

1. Under **concrete semantics**, we are precise; we only compute the result of code that is called. This allows us to see exactly what happens when we run our program. For example, if we expect to return a boolean value and our results differ, we’ll throw an error (which is sub-optimal because in a language like Ruby, a lot of objects that are not booleans could be used as booleans).
2. Under **abstract semantics**, we are exhaustive; we compute the result of all possible permutations. This is how we compute call graphs. Under abstract semantics, we don’t know if something is going to be `true` or `false`, so we take both branches—non-deterministically producing both using the `<|>` operator which represents choice, building a union of possibilities.
3. Under **type-checking semantics**, we verify that the type of a syntactic construct (ex., an object of type `Int`) matches what is expected when it is used. This helps us check type errors, emulating compile-time static type checking.

### Evaluation
The [`Evaluatable`](https://github.com/github/semantic/blob/master/src/Data/Abstract/Evaluatable.hs) class defines the necessary interface for a term to be evaluated. While a default definition of `eval` is given, instances with computational content must implement `eval` to perform their small-step operational semantics. Evaluation gives us a way to capture what it means to interpret the syntax data types we create using the [Assignment](https://github.com/github/semantic/blob/master/docs/assignment.md) stage. The evaluation algebra also handles each syntax without caring about any language-specific implementation. We do this by cascading polymorphic functions using the `Evaluatable` type class.

We have yet to finish implementing `Evaluatable` instances for the various à la carte syntaxes. Doing so requires knowledge of the type and value evaluation semantics of a particular syntax and familiarity with the functions for interacting with the environment and store.

#### Implementing `Evaluatable` instances
The following is a brief guide to working with the definitional interpreters and implementing instances of `Evaluatable` for the various pieces of syntax. `Semantil.Util` defines a series of language-specific wrapper functions for working in ghci to do evaluation.

_Helpers:_
- `parseFile`: parses one file.
- `evaluateLanguageProject`: takes a list of files and evaluates them usually under concrete semantics.
- `callGraphLanguageProject`: uses the same mechanism for evaluating, but uses abstract semantics.
- `typeCheckLanguageFile`: allows us to evaluate under type checking semantics.

#### Creating good abstractions
When adding `Evaluatable` instances, we may notice that certain language-specific syntaxes share semantics sufficiently enough to be consolidated into a language-agnostic data type (and resultantly, have one `Evaluatable` instance). Other times—it may be the opposite case where there is not enough overlap in evaluation semantics and therefore requires decoupling. Reasoning through the right abstractions is a big part of determining how to write these `Evaluatable` instances.

### Effects
To perform these computations, we need effects. An effect is something a piece of code does which isn’t strictly encapsulated in its return value. Outside of taking inputs and returning outputs, programs must capture state in memory by read or write, throw exceptions, fail to terminate, or terminate non-deterministically, etc. These outcomes are known as _effects_. An an example, consider the JS function:

```
function square(x) {
  return x * x;
}
```

This is _pure_ because it performs no effects, whereas the similar function:

```function square(x) {
  console.log("squaring x: " + x);
  return x * x;
}
```
computes the same result value but additionally performs an effect (logging). Effects provide convenient access to powerful and efficient capabilities of the machine such as interrupts, stateful memory, the file system, and the monitor.

We compute effects non-deterministically.

<!--- WIP: come back and briefly talk about why this is useful for program analysis --->

<!--- WIP: come back and briefly talk about why this is useful for runEvaluator --->

### Potential use-cases

- *Dead code* analysis: reduce potential surface area (security vulnerabilities). Less code to maintain is always a good thing. Good examples in most IDEs.
- *Symbolic* - allows us to do symbolic execution. https://prepack.io/
- *Caching* - a way to guarantee that an analysis will terminate. allows us to write a type checker. abstracting variables to their types. Instead of potentially infinite series of integers, you can represent as Int (finitization of values)
- *Collecting* - allows us to have greater precision in other analyses (more useful internally)
- *Tracing and reachable* state - useful for debugging (it's verbose).
