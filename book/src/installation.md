# Installation

Aerology is written in Rust, and uses the capstone library for disassembly.
Since capstone is written in C++, you will need to ensure that you have a
suitable compiler instaled.

If a recent version of Rust in not provided by your OS or it's package
manager you may find [rustup.rs](https://rustup.rs) helpful.

Like many Rust applications, Aerology is built with Rust's build tool and
package manager Cargo.
Installation is also handled by Cargo, and 
```bash
$ cargo install --path .
```
should be sufficient to install Aerology.
