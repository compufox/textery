# textery
### _ava fox_

reimplementation of tracery in lisp

## Installation

```shell
$ git clone https://github.com/compufox/textery ~/common-lisp/textery
* (ql:quickload :textery)
```

## Usage

Load a rule file (a normal tracery JSON file) using `(load-grammar "rule.file")`

If this is the first rule file loaded, it is set as the current grammar. 

To expand text, use `(expand "#expand# this!")`

If you have multiple grammars loaded use the `(with-grammar)` macro to select which grammar to use:

```lisp
(with-grammar "grammar2"
  (expand "#test#"))
```

You can pass arguments to the text to be expanded by using pipes: `(expand-text "#test|string-upcase#")`, these can be chained together by doing `(expand-text "#test|string-upcase|reverse#")`

Arguments are any lisp function that can handle at least one string parameter. You can even provide your own!

If you want to use functions that require more than one parameter do as follows:

```lisp
(defun example-argument (text param param2)
  (concatenate 'string
			   text
			   param
			   param2))

(expand "#test|example-argument,5,test#")
```

## API

`(load-grammar file)`

loads grammar from FILE

stores the contents internally under the FILE's name minus the extension

if this is the first grammar thats loaded, set our current grammar to it

---

`(with-grammar grammar &body body)`

executes BODY with GRAMMAR set to be the current grammar

---

`(expand text)`

expands TEXT using the current grammar (first loaded grammar file)

---


## License

BSD 3-Clause

