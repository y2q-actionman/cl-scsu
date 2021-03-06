# Abstract

[Standard Compression Scheme for Unicode](http://unicode.org/reports/tr6/)
の Common Lisp 実装。

*cl-scsu* is an implementation of
[Standard Compression Scheme for Unicode](http://unicode.org/reports/tr6/)
by Common Lisp.

# License

The MIT License. See LICENSE file.

# Loading

## Libraries depending on

* asdf
* alexandria
* 1am (only by test codes)

## Load by quicklisp

[![Quicklisp](http://quickdocs.org/badge/cl-scsu.svg)](http://quickdocs.org/cl-scsu/)

```lisp
(ql:quickload "cl-scsu")
```

## or, Load manually

```lisp
(asdf:load-asd "cl-scsu.asd")
(asdf:load-system :cl-scsu)
```

For running tests, do below additionally.

```lisp
(asdf:load-asd "cl-scsu-test.asd")
(asdf:test-system :cl-scsu)
```

# Examples

Makes a test string, estimated 38 bytes long by counting Japanese characters as 2 bytes.

```lisp
CL-USER> (defvar *test-string* "abcde あいうえお アイウエオ 阿伊宇江於")
*TEST-STRING*
```

Let's compress it. The result is only 30 bytes long.

```lisp
CL-USER> (cl-scsu:encode-from-string *test-string*)
#(97 98 99 100 101 32 21 130 132 134 136 138 32 226 228 230 232 234 32
  15 150 63 79 10 91 135 108 95 101 188) ; compressin result
30                                       ; number of result bytes
23                                       ; number of read characters
#<CL-SCSU:SCSU-STATE @ #x10002ed37c2>    ; scsu state object
```

To restore the original string, decompress the bytes.

```lisp
CL-USER> (cl-scsu:decode-to-string *)
"abcde あいうえお アイウエオ 阿伊宇江於" ; decompression result
23                                       ; number of result characters
30                                       ; number of read bytes
#<CL-SCSU:SCSU-STATE @ #x10002f469f2>    ; scsu state object
```

# API

Please see docstrings of symbols exported from `cl-scsu` package:

- [Function] `decode-to-string`
- [Function] `encode-from-string`
- [Function] `encode-reset-sequence`
- [Class] `scsu-state`
- [Condition] `scsu-error`
- [Restart] `restore-state`
