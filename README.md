# textery
### _ava fox_

reimplementation of tracery in lisp

## Installation

```shell
$ git clone https://github.com/compufox/textery ~/common-lisp/textery
* (ql:quickload :textery)
```

## Usage

Load a rule file (a normal tracery JSON file) using `(load-rules "rule.file")`

If this is the first rule file loaded, it is set as the current ruleset. 

To expand text, use `(expand-text "#expand# this!")` to use a different ruleset `(expand-text "#expand# text" :ruleset "rule2")`

The argument to be passed as `:ruleset` is the name of the tracery file, minus the extension

You can pass arguments to the text to be expanded by using pipes: `(expand-text "#test|c#")`, these can be chained together by doing `(expand-text "#test|c|s#")`

Each argument will be detailed below:

## Arguments

`c`: capitalize

`s`: pluralize

## License

BSD 3-Clause

