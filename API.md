# API

## [Function] `decode-to-string`

### 書式 / Syntax

```lisp
(decode-to-string bytes
	&key start1 end1 string start2 end2
	     state)
```

### 説明 / Description

`bytes` に渡された SCSU で圧縮されたバイト列を文字列に変換して返す。
戻値は以下の通り:

1. SCSU解凍結果の文字列
2. 1. で返された文字列に、最後に書き出した文字の次の位置
3. `bytes` で渡したバイト列で、最後に読み込んだバイトの次の位置
4. `scsu-state` オブジェクト

This function decompresses `bytes` compressed by SCSU and returns the result as a string.
Returned values are following:

1. The result string decompressed by SCSU.
2. The position of the next of the last character written to the result string.
3. The position of the next of the last byte read from `bytes`
4. `scsu-state` object.

### 引数 / Arguments

- `bytes`

  SCSU圧縮されたバイト列。

  A byte sequence compressed by SCSU.
	
- `start1`, `end1`

  `bytes` 引数に渡したバイト列の使用範囲を指定する。初期値は、それぞれ `0` と `bytes` の長さ.
	
  Specifies the range of `bytes` argument should be used.
  Initial value is `0` and the length of `bytes` respectively.
	
- `string`

  SCSU解凍結果を格納する先の文字列。ここで指定された文字列が返される。
  未指定の場合、新しい文字列を割り当てて返す。

  Specifies an output storage filled by the decompression result.
  This specified string is returned by `decode-to-string`.
  If not specified, a new string is allocated and returned.

- `start2`, `end2`

  `string` 引数に渡した文字列の使用範囲を指定する。
  初期値は、それぞれ `0` と `string` の長さ.
  `string` を指定しなかった場合、無視される。
	
  Specifies the range of `string` argument used.
  Initial value is `0` and the length of `string` respectively.
  If `string` is not specified, these arguments are ignored.
	
- `state`

  他の `decode-to-string` 呼び出しで返された `scsu-state` オブジェクトを
  渡すことにより、シフト状態などを引き継いで使用できる。
  指定しない場合、 (初期シフト状態)[http://unicode.org/reports/tr6/#Initial_State] を使用する。

  If specifies a `scsu-state` object returned by previous `decode-to-string` calls,
  the shift state included the object is used.
  If not specified, (the initial state)[http://unicode.org/reports/tr6/#Initial_State] is used.

## [Function] `encode-from-string`

### 書式 / Syntax

```lisp
(encode-from-string string
	&key start1 end1 bytes start2 end2 initial-priority
	     fix-dynamic-window state)
```

### 説明 / Description

`string` を SCSU で圧縮したバイト列に変換して返す。
戻値は以下の通り:

1. SCSU圧縮結果のバイト列
2. 1. で返されたバイト列に、最後に書き出したバイトの次の位置
3. `bytes` で渡した文字列で、最後に読み込んだ文字の次の位置
4. `scsu-state` オブジェクト

This function compresses `string` by SCSU and returns the result as a byte sequence.
Returned values are following:

1. The result byte sequence compressed by SCSU.
2. The position of the next of the last byte written to the result byte sequence.
3. The position of the next of the last characted read from `string`
4. `scsu-state` object.

### 引数 / Arguments

- `string`

  SCSU圧縮する文字列。

  A string to be compressed by SCSU.
	
- `start1`, `end1`
	
  `string` 引数に渡した文字列の使用範囲を指定する。初期値は、それぞれ `0` と `string` の長さ.
	
  Specifies the range of `string` argument should be used.
  Initial value is `0` and the length of `string` respectively.
	
- `bytes`
	
  SCSU圧縮結果を格納する先のバイト列。ここで指定されたバイト列が返される。
  未指定の場合、新しいバイト列を割り当てて返す。

  Specifies an output storage filled by the compression result.
  This specified byte sequence is returned by `encode-from-string`.
  If not specified, a new byte sequence is allocated and returned.

- `start2`, `end2`

  `bytes` 引数に渡したバイト列の使用範囲を指定する。
  初期値は、それぞれ `0` と `bytes` の長さ.
  `bytes` を指定しなかった場合、無視される。
	
  Specifies the range of `bytes` argument used.
  Initial value is `0` and the length of `bytes` respectively.
  If `bytes` is not specified, these arguments are ignored.

- `initial-priority`

  SCSUの (初期 dynamic window)[http://unicode.org/reports/tr6/#Initial_Window] の優先順序を指定する。指定可能な値は以下の通り:
  - `:lookahead` :: `string` 引数の内容を先読みして決定する。
  - `:random` :: 乱数で適当に決める。
  - 数値配列 :: 渡した数列を初期 dynamic window の優先度として使用する。大きい値が優先される。

  Specifies how to determine the priority of (the initial dynamic window)[http://unicode.org/reports/tr6/#Initial_Window] of SCSU as following:
  - `:lookahead` :: Determines by lookaheading `string` argument.
  - `:random` :: determines randomly.
  - an interger array :: Uses the array as a priority. Bigger is prior.

- `fix-dynamic-window`

  (stub)

- `state`

  他の `encode-from-string` 呼び出しで返された `scsu-state` オブジェクトを
  渡すことにより、シフト状態などを引き継いで使用できる。
  指定しない場合、 (初期シフト状態)[http://unicode.org/reports/tr6/#Initial_State] を使用する。

  If specifies a `scsu-state` object returned by previous `encode-from-string` calls,
  the shift state included the object is used.
  If not specified, (the initial state)[http://unicode.org/reports/tr6/#Initial_State] is used.


## [Function] `encode-reset-sequence`

### 書式 / Syntax

```lisp
(encode-reset-sequence state
	&key bytes start end)
```

### 説明 / Description

`state` の内部状態を SCSU の初期状態に戻すためのバイト列を返す。
戻値は以下の通り:

1. バイト列
2. 1. で返されたバイト列に、最後に書き出したバイトの次の位置
3. `scsu-state` オブジェクト

This function returns a byte sequence to change the SCSU state to the initial state.
Returned values are following:

1. A byte sequence.
2. The position of the next of the last byte written to the result byte sequence.
3. `scsu-state` object.

### 引数 / Arguments

- `state`

  `scsu-state` オブジェクト

  A `scsu-state` object.

- `bytes`
	
  格納先のバイト列。ここで指定されたバイト列が返される。
  未指定の場合、新しいバイト列を割り当てて返す。

  Specifies an output storage filled by the result.
  This specified byte sequence is returned by `encode-reset-sequence`.
  If not specified, a new byte sequence is allocated and returned.

- `start`, `end`

  `bytes` 引数に渡したバイト列の使用範囲を指定する。
  初期値は、それぞれ `0` と `bytes` の長さ.
  `bytes` を指定しなかった場合、無視される。
	
  Specifies the range of `bytes` argument used.
  Initial value is `0` and the length of `bytes` respectively.
  If `bytes` is not specified, these arguments are ignored.


## [Class] `scsu-state`

SCSUの内部状態を保持するオブジェクトのクラス。

The class of an object holding the internal state of SCSU.


## [Condition] `scsu-error`

`decode-to-string`, `encode-from-string`, `encode-reset-sequence` でエ
ラーが発生した場合、この condition が報告される。

If some error occurs in `decode-to-string`, `encode-from-string`, or `encode-reset-sequence`,
this condition is reported.


## [Restart] `restore-state`

`scsu-error` が発生した時、この restart が提供される。

この restart を使用すると、各関数は、最後に正常に処理することのできた文字、もしくはバイトまで
処理を巻き戻し、その時点での値を返す。

If `scsu-error` is raised, this restart is established.
If this restart is invoked, each function rewinds precessing to the
last character or bytes collectly processed, and returns values at
that time.
