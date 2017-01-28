# haskell-cabal-starter
A boilerplate for a haskell-cabal library with tests included

## Getting Started
This is a project in which there is the library `Haq.hs`. We are interested in building and testing this library.

After cloning the repository, run these commands to get started:

```
# Create `cabal.sandbox.config` and actual sandbox in `.cabal-sandbox/`
cabal sandbox init
# Compile and build the application with tests included. Build can be found in `dist/` directory.
cabal install --enable-tests
# Configure the library
cabal configure
# Run the tests
cabal test
```

After you successfully completed the steps above, you should see something like this:

```
Running 1 test suites...
Test suite tests: RUNNING...

Validate haqify function
  haqify is supposed to prefix Haq! to things
Also validate haqify function
  haqify is supposed to prefix NonHaq! to things FAILED [1]

Failures:

  tests/HSpecTests.hs:13:
  1) Also validate haqify function haqify is supposed to prefix NonHaq! to things
       expected: "NonHaq! me"
        but got: "Haq! me"

Randomized with seed 501962637

Finished in 0.0089 seconds
2 examples, 1 failure
Test suite tests: FAIL
Test suite logged to: dist/test/haq-0.1.0.0-tests.log
```

Now you can start with fixing the wrong spec. Happy hacking!

## LICENSE
This project is protected(!) under [WTFPL LICENSE](LICENSE).
