# brainfuck

[Brainfuck](https://en.wikipedia.org/wiki/Brainfuck) is very well-named esoteric programming language. At first I thought the name was a joke, but it's really a warning.

The second I started playing with Brainfuck, I knew I couldn't stop until I was **certain** that I could write anything in it. Thus, to free myself from my self-imposed Brainfuck dungeon, I created a high-level programming language that compiles to Brainfuck. For a sample program in this language, checkout [is_power_of_two.clj](examples/is_power_of_two.clj).

## Usage

This includes both a Brainfuck interpreter and a compiler for my programming language. To run an existing piece of brainfuck code, do this:

    $ lein run interpret <file.bf> [stdin]

Notice that you can pass in standard input data as a command-line argument.

To compile a program down to Brainfuck, such as *is_power_of_two.clj*, simply do this:

    $ lein run compile <file.clj> >output.bf

In order to actually run a program written in my language, you must first compile it to Brainfuck and then run the Brainfuck code.

## Examples

I wrote a program which prints powers of two in decimal. To run this, do the following:

    $ lein run interpret examples/powers_of_two.bf
    2^n:
    2
    4
    8
    16
    32
    64
    128
    256
    512
    1024
    2048
    4096
    8192
    ...

Notice that this program is not constrained to any particular bit size. It is operating directly on decimal "bignums", so it can go on forever.

I also created a program which tells you if a decimal number is a power of two:

    $ lein run interpret examples/is_power_of_two.bf 8589934592
    Processing...
    Power of 2
    $ lein run interpret examples/is_power_of_two.bf 1025
    Processing...
    Not power of 2

Just like the other program, this program uses bignums. In other words, the size of the supplied number is unbounded.
