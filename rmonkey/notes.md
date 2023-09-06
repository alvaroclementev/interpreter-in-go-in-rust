# Notes

## Next Steps

- Builtins
    - range
- Module system (import)


## Improvements

Things that should probably be changed

- Structure of Expression and Statement
    - A lot of duplication 
        - pointless trait impls
    - Recursive references
        - Rc and clone
    - Invalid states are representable
        - For instance, an if statement MUST contain a BLOCK as consequence and alternative (optional), not any kind of statement
        - I am testing with the FunctionLiteral to see what if we use the underlying structs directly
            - It's a bit weird for testing when we want to have the generic version (the enum)
                - Maybe it would be solved with simple `From` implementation and a call to `into()`
    - For error handling maybe using Result<> is better, so that we can use the `?` operator
    - If we make `eval` consume the object being evaluated we can avoid a lot of cloning
        - Not sure if this couild impact things like error reporting

- Tests
    - For table driven tests we could use something like [test_case](https://docs.rs/test-case/latest/test_case/)

- CLI
    - Use a `readline` implementation, for instance [rustyline](https://github.com/kkawakam/rustyline)
