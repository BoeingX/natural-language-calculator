# Natural language calculator
[![Build Status](https://travis-ci.org/BoeingX/natural-language-calculator.svg?branch=master)](https://travis-ci.org/BoeingX/natural-language-calculator)
[![License](https://img.shields.io/badge/License-BSD%203--Clause-blue.svg)](https://opensource.org/licenses/BSD-3-Clause)

This project is a simple calculator which understands arithmetic expressions in natural language.

```
>> 1 + (2 * 3) - 4
3
>> one plus two
3
>> what is three plus four?
7
>> calculate one plus two times three minus four
3
```

## Requirements

A Haskell environment is required.
[Stack](https://docs.haskellstack.org/en/stable/README/) is highly recommended.

## Get started

To compile and run the binary

```bash
$ stack build
$ stack exec natural-language-calculator-exe
```

If you have [rlwrap](https://github.com/hanslub42/rlwrap)
installed, run instead

```bash
$ rlwrap stack exec natural-language-calculator-exe
```

to get readline support (like using arrow key).

## Limitation

- Only `one`, `two`, ..., `nine` are currently supported (but arbitrary numerical numbers and parentheses are supported). It is possible to change this by modifying the `word` function in [src/Lib.hs](src/Lib.hs).
- It only understands `what is` and `calculate` (case insensitive), other text will result a parse error. While we can add many more, this should ideally be learnt with machine learning as in the [Sempre](https://github.com/percyliang/sempre) project.

## Contribution

Any contribution is appreciated.
