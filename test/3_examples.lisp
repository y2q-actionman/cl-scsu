(in-package :cl-scsu.test)

;;; From:
;;;  http://unicode.org/reports/tr6/#Examples

(defun make-string-from-codepoints (codepoints)
  (loop with ret = (make-array (length codepoints) :element-type 'character
			       :adjustable t :fill-pointer 0)
     for cp across codepoints
     if (>= cp char-code-limit)
     do (multiple-value-bind (high low)
     	    (cl-scsu::encode-to-surrogate-pair cp)
     	  (vector-push-extend (code-char high) ret)
     	  (vector-push-extend (code-char low) ret))
     else
     do (vector-push-extend (code-char cp) ret)
     finally (return ret)))

(defun test-example* (codepoints expected-compression)
  (let* ((expected-compression
	  (make-array (length expected-compression) :element-type '(unsigned-byte 8)
					   :initial-contents expected-compression))
	 (string (make-string-from-codepoints codepoints))
	 (expected-decompression (decode-to-string expected-compression)))
    (assert-equal string expected-decompression)
    (let* ((compressed (encode-from-string string))
	   (decompressed (decode-to-string compressed))
	   (compressed-len (length compressed))
	   (expected-compression-len (length expected-compression)))
      #+()
      (format t "~&Codepoints: ~A~%String: ~A~%Expected Compression: (~D)~%~A~%Our Compression: (~D)~%~A
Our Decompression: ~A~%Decompression from Expected: ~A~2%"
	      codepoints string
	      expected-compression-len expected-compression
	      compressed-len compressed
	      decompressed expected-decompression)
      (assert-equal decompressed string)
      (unless (equalp compressed expected-decompression)
	(when (> compressed-len expected-compression-len)
	  #+()
	  (warn "Our compressed length ~D is longer than expected compression length ~D~%"
		compressed-len expected-compression-len)))))
  t)
  

(defun test-9.1 () 			; German
  (test-example*
   #(#x00D6 #x006C #x0020 #x0066 #x006C #x0069 #x0065 #x00DF #x0074)
   '(#xD6 #x6C #x20 #x66 #x6C #x69 #x65 #xDF #x74)))

(defun test-9.2 () 			; Russian
  (test-example*
   #(#x041C #x043E #x0441 #x043A #x0432 #x0430)
   '(#x12 #x9C #xBE #xC1 #xBA #xB2 #xB0)))

(defun test-9.3 () 			; Japanese
  (test-example*
   #(#x3000 #x266A #x30EA #x30F3 #x30B4 #x53EF #x611B ;　♪リンゴ可愛
     #x3044 #x3084 #x53EF #x611B #x3044 #x3084 #x30EA #x30F3 ;いや可愛いやリン
     #x30B4 #x3002 #x534A #x4E16 #x7D00 #x3082 #x524D #x306B ;ゴ。半世紀も前に
     #x6D41 #x884C #x3057 #x305F #x300C #x30EA #x30F3 #x30B4 ;流行した「リンゴ
     #x306E #x6B4C #x300D #x304C #x3074 #x3063 #x305F #x308A ;の歌」がぴったり
     #x3059 #x308B #x304B #x3082 #x3057 #x308C #x306A #x3044 ;するかもしれない
     #x3002 #x7C73 #x30A2 #x30C3 #x30D7 #x30EB #x30B3 #x30F3 ;。米アップルコン
     #x30D4 #x30E5 #x30FC #x30BF #x793E #x306E #x30D1 #x30BD ;ピュータ社のパソ
     #x30B3 #x30F3 #x300C #x30DE #x30C3 #x30AF #xFF08 #x30DE ;コン「マック（マ
     #x30C3 #x30AD #x30F3 #x30C8 #x30C3 #x30B7 #x30E5 #xFF09 ;ッキントッシュ）
     #x300D #x3092 #x3001 #x3053 #x3088 #x306A #x304F #x611B ;」を、こよなく愛
     #x3059 #x308B #x4EBA #x305F #x3061 #x306E #x3053 #x3068 ;する人たちのこと
     #x3060 #x3002 #x300C #x30A2 #x30C3 #x30D7 #x30EB #x4FE1 ;だ。「アップル信
     #x8005 #x300D #x306A #x3093 #x3066 #x8A00 #x3044 #x65B9 ;者」なんて言い方
     #x307E #x3067 #x3042 #x308B #x3002) ;まである。
   '(#x08 #x00 #x1B #x4C #xEA #x16 #xCA #xD3 #x94 #x0F #x53 #xEF #x61 #x1B #xE5 #x84
     #xC4 #x0F #x53 #xEF #x61 #x1B #xE5 #x84 #xC4 #x16 #xCA #xD3 #x94 #x08 #x02 #x0F
     #x53 #x4A #x4E #x16 #x7D #x00 #x30 #x82 #x52 #x4D #x30 #x6B #x6D #x41 #x88 #x4C
     #xE5 #x97 #x9F #x08 #x0C #x16 #xCA #xD3 #x94 #x15 #xAE #x0E #x6B #x4C #x08 #x0D
     #x8C #xB4 #xA3 #x9F #xCA #x99 #xCB #x8B #xC2 #x97 #xCC #xAA #x84 #x08 #x02 #x0E
     #x7C #x73 #xE2 #x16 #xA3 #xB7 #xCB #x93 #xD3 #xB4 #xC5 #xDC #x9F #x0E #x79 #x3E
     #x06 #xAE #xB1 #x9D #x93 #xD3 #x08 #x0C #xBE #xA3 #x8F #x08 #x88 #xBE #xA3 #x8D
     #xD3 #xA8 #xA3 #x97 #xC5 #x17 #x89 #x08 #x0D #x15 #xD2 #x08 #x01 #x93 #xC8 #xAA
     #x8F #x0E #x61 #x1B #x99 #xCB #x0E #x4E #xBA #x9F #xA1 #xAE #x93 #xA8 #xA0 #x08
     #x02 #x08 #x0C #xE2 #x16 #xA3 #xB7 #xCB #x0F #x4F #xE1 #x80 #x05 #xEC #x60 #x8D
     #xEA #x06 #xD3 #xE6 #x0F #x8A #x00 #x30 #x44 #x65 #xB9 #xE4 #xFE #xE7 #xC2 #x06
     #xCB #x82)))
;;; To make optimal compression, we must lookahead 3 chars (at "の歌」がぴったり" Hiraganas)

(defun test-9.4 ()			; all features
  (let ((utf-32-seq
	 #(#x0041 #x00DF #x0401 #x015F #x00DF #x01DF #xF000
	   #x10FFFF
	   #x000D #x000A
	   #x0041 #x00DF #x0401 #x015F #x00DF #x01DF #xF000
	   #x10FFFF))
	(utf-16-seq
	 #(#x0041 #x00DF #x0401 #x015F #x00DF #x01DF #xF000
	   #xDBFF #xDFFF
	   #x000D #x000A
	   #x0041 #x00DF #x0401 #x015F #x00DF #x01DF #xF000
	   #xDBFF #xDFFF)))
    #+allegro
    (assert-equalp (make-string-from-codepoints utf-32-seq)
    		   (make-string-from-codepoints utf-16-seq))
    (test-example*
     (if (> char-code-limit #xFFFF) utf-32-seq utf-16-seq)
     '(#x41 #xDF #x12 #x81 #x03 #x5F #x10 #xDF #x1B #x03 #xDF #x1C #x88 #x80
       #x0B #xBF #xFF #xFF
       #x0D #x0A
       #x41 #x10 #xDF #x12 #x81 #x03 #x5F #x10 #xDF #x13 #xDF #x14 #x80
       #x15 #xFF))))
;;; This example uses SDX for U+10FFFF. cl-scsu don't do so because U+10FFFF is a non-character.

(defun test-examples ()
  (and (test-9.1)
       (test-9.2)
       (test-9.3)
       (test-9.4)
       t))
