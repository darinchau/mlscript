# MLscript

What would TypeScript look like if it had been designed with type inference and soundness in mind?

We provide one possible answer in MLscript, an object-oriented and functional programming language with records, generic classes, mix-in traits, first-class unions and intersections, instance matching, and ML-style principal type inference.
These features can be used to implement expressive class hierarchies as well as extensible sums and products.

MLscript supports union, intersection, and complement (or negation) connectives, making sure they form a Boolean algebra, and add enough structure to derive a sound and complete type inference algorithm.

## Getting Started

### Project Structure

#### Sub-Projects

- The ts2mls sub-project allows you to use TypeScript libraries in MLscript. It can generate libraries' declaration information in MLscript by parsing TypeScript AST, which can be used in MLscript type checking.

#### Directories

- The `shared/src/main/scala/mlscript` directory contains the sources of the MLscript compiler.

- The `shared/src/test/scala/mlscript` directory contains the sources of the testing infrastructure.

- The `shared/src/test/diff` directory contains the actual tests.

- The `ts2mls/js/src/main/scala/ts2mls` directory contains the sources of the ts2mls module.

- The `ts2mls/js/src/test/scala/ts2mls` directory contains the sources of the ts2mls declaration generation test code.

- The `ts2mls/jvm/src/test/scala/ts2mls` directory contains the sources of the ts2mls diff test code.

- The `ts2mls/js/src/test/typescript` directory contains the TypeScript test code.

- The `ts2mls/js/src/test/diff` directory contains the declarations generated by ts2mls.

### Prerequisites

You need [JDK supported by Scala][supported-jdk-versions], [sbt][sbt], [Node.js][node.js], and TypeScript to compile the project and run the tests.

We recommend you to install JDK and sbt via [coursier][coursier]. The versions of Node.js that passed our tests are from v16.14 to v16.17, v17 and v18. Run `npm install` to install TypeScript. **Note that ScalaJS cannot find the global installed TypeScript.** We explicitly support TypeScript v4.7.4.

[supported-jdk-versions]: https://docs.scala-lang.org/overviews/jdk-compatibility/overview.html
[sbt]: https://www.scala-sbt.org/
[node.js]: https://nodejs.org/
[coursier]: https://get-coursier.io/

### Running the tests

Running the main MLscript tests only requires the Scala Build Tool installed.
In the terminal, run `sbt mlscriptJVM/test`.

Running the ts2mls MLscript tests requires NodeJS, and TypeScript in addition.
In the terminal, run `sbt ts2mlsJS/test`.

You can also run all tests simultaneously.
In the terminal, run `sbt test`.

### Running tests individually

Individual tests can be run with `-z`.
For example, `~mlscriptJVM/testOnly mlscript.DiffTests -- -z parser` will watch for file changes and continuously run all parser tests (those that have "parser" in their name).

You can also indicate the test you want in `shared/src/test/scala/mlscript/DiffTests.scala`:

```scala
  // Allow overriding which specific tests to run, sometimes easier for development:
  private val focused = Set[Str](
    // Add the test file path here like this:
    "shared/src/test/diff/mlscript/Methods.mls"
  ).map(os.RelPath(_))
```

To run the tests in ts2mls sub-project individually,
you can indicate the test you want in `ts2mls/js/src/test/scala/ts2mls/TSTypeGenerationTests.scala`:

```scala
private val testsData = List(
    // Put all input files in the `Seq`
    // Then indicate the output file's name
    (Seq("Array.ts"), "Array.d.mls")
  )
```

### Running the web demo locally

To run the demo on your computer, compile the project with `sbt fastOptJS`, then open the `local_testing.html` file in your browser.

You can make changes to the type inference code
in `shared/src/main/scala/mlscript`,
have it compile to JavaScript on file change with command
`sbt ~fastOptJS`,
and immediately see the results in your browser by refreshing the page with `F5`.
