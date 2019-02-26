# Causal Cop

Install cargo using `rustup`.

Then `cargo build --release` to build the binary.

The binary will be available at `target/release/causalcop`.

##### Usage:

    > causalcop -h
    Usage:
        copcc [file]       read specified file
    or: copcc -            read from stdin
    > cat cc-history.txt | causalcop
    true
    > echo $?
    0
    > causalcop non-cc-history.txt
    false
    > echo $?
    1
