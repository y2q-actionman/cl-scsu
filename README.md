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

## Loading

```lisp
(load "cl-scsu.asd")
(asdf:load-system :cl-scsu)
```

For running tests, do below additionally.

```lisp
(load "cl-scsu-test.asd")
(asdf:test-system :cl-scsu)
```

# Examples

Makes a test string, estimated 38 bytes long by counting Japanese characters as 2 bytes.

```
CL-USER> (defvar *test-string* "abcde あいうえお アイウエオ 阿伊宇江於")
*TEST-STRING*
```

Let's compress it. The result is only 30 bytes long.

```
CL-USER> (cl-scsu:encode-from-string *test-string*)
#(97 98 99 100 101 32 21 130 132 134 136 138 32 226 228 230 232 234 32
  15 150 63 79 10 91 135 108 95 101 188)
30
23
#<CL-SCSU:SCSU-STATE @ #x10002ed37c2>
```

To restore the original string, decompress the bytes.

```
CL-USER> (cl-scsu:decode-to-string *)
"abcde あいうえお アイウエオ 阿伊宇江於"
23
30
#<CL-SCSU:SCSU-STATE @ #x10002f469f2>
CL-USER>
```

# API

see [API.md](API.md).
