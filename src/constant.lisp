(in-package :scsu)

(defconstant +window-count+ 8 "number of static or dynamic windows")

(defmacro define-window-find-function (name lower)
  `(defun ,name (byte)
     (declare (type (unsigned-byte 8) byte))
     (assert (<= ,lower byte (+ ,lower +window-count+))) ; TODO: move to type decl.
     (- byte ,lower)))

;;; Single-Byte Mode Tag Values
(defconstant +SQ0+ #x1 "Quote from Window 0")
(defconstant +SQ1+ #x2 "Quote from Window 1")
(defconstant +SQ2+ #x3 "Quote from Window 2")
(defconstant +SQ3+ #x4 "Quote from Window 3")
(defconstant +SQ4+ #x5 "Quote from Window 4")
(defconstant +SQ5+ #x6 "Quote from Window 5")
(defconstant +SQ6+ #x7 "Quote from Window 6")
(defconstant +SQ7+ #x8 "Quote from Window 7")
(defconstant +SDX+ #xB "Define Extended")
(defconstant +SQU+ #xE "Quote Unicode")
(defconstant +SCU+ #xF "Change to Unicode")
(defconstant +SC0+ #x10 "Change to Window 0")
(defconstant +SC1+ #x11 "Change to Window 1")
(defconstant +SC2+ #x12 "Change to Window 2")
(defconstant +SC3+ #x13 "Change to Window 3")
(defconstant +SC4+ #x14 "Change to Window 4")
(defconstant +SC5+ #x15 "Change to Window 5")
(defconstant +SC6+ #x16 "Change to Window 6")
(defconstant +SC7+ #x17 "Change to Window 7")
(defconstant +SD0+ #x18 "Define Window 0")
(defconstant +SD1+ #x19 "Define Window 1")
(defconstant +SD2+ #x1A "Define Window 2")
(defconstant +SD3+ #x1B "Define Window 3")
(defconstant +SD4+ #x1C "Define Window 4")
(defconstant +SD5+ #x1D "Define Window 5")
(defconstant +SD6+ #x1E "Define Window 6")
(defconstant +SD7+ #x1F "Define Window 7")

(define-window-find-function find-SQn-window +SQ0+)
(define-window-find-function find-SCn-window +SC0+)
(define-window-find-function find-SDn-window +SD0+)

;;; Unicode Mode Tag Values
(defconstant +UC0+ #xE0 "Change to Window 0")
(defconstant +UC1+ #xE1 "Change to Window 1")
(defconstant +UC2+ #xE2 "Change to Window 2")
(defconstant +UC3+ #xE3 "Change to Window 3")
(defconstant +UC4+ #xE4 "Change to Window 4")
(defconstant +UC5+ #xE5 "Change to Window 5")
(defconstant +UC6+ #xE6 "Change to Window 6")
(defconstant +UC7+ #xE7 "Change to Window 7")
(defconstant +UD0+ #xE8 "Define Window 0")
(defconstant +UD1+ #xE9 "Define Window 1")
(defconstant +UD2+ #xEA "Define Window 2")
(defconstant +UD3+ #xEB "Define Window 3")
(defconstant +UD4+ #xEC "Define Window 4")
(defconstant +UD5+ #xED "Define Window 5")
(defconstant +UD6+ #xEE "Define Window 6")
(defconstant +UD7+ #xEF "Define Window 7")
(defconstant +UQU+ #xF0 "Quote Unicode")
(defconstant +UDX+ #xF1 "Define Extended")

(define-window-find-function find-UCn-window +UC0+)
(define-window-find-function find-UDn-window +UD0+)

;;; Tables
(defun lookup-window-offset-table (byte)
  (declare (type (unsigned-byte 8) byte))
  (cond ((= byte #x0)
	 (error "reserved"))		; reserved for internal use
	((<= #x1 byte #x67)
	 (* byte #x80))		   ; half-blocks from U+0080 to U+3380
	((<= #x68 byte #xA7)
	 (+ (* byte #x80) #xAC00)) ; half-blocks from U+E000 to U+FF80
	((<= #xA8 byte #xF8)
	 (error "reserved"))		; reserved for future use
	((= byte #xF9) #x00C0) ; Latin-1 letters + half of Latin Extended-A
	((= byte #xFA) #x0250) ; IPA Extensions
	((= byte #xFB) #x0370) ; Greek
	((= byte #xFC) #x0530) ; Armenian
	((= byte #xFD) #x3040) ; Hiragana
	((= byte #xFE) #x30A0) ; Katakana
	((= byte #xFF) #xFF60) ; Halfwidth Katakana
	(t (assert nil))))

(defun compressible-code-point-p (x)
  (declare (type (integer #x0 #xFFFF) x))
  (or (<= #x0 x (1- #x80))
      (<= (+ #x3380 #x7F) x (1- #xE000))))

(defun lookup-static-window-position (x)
  (declare (type (integer 0 7) x))
  (ecase x
    (0 #x0000)	      ; (for quoting of tags used in single-byte mode)
    (1 #x0080)	      ; Latin-1 Supplement
    (2 #x0100)	      ; Latin Extended-A
    (3 #x0300)	      ; Combining Diacritical Marks
    (4 #x2000)	      ; General Punctuation
    (5 #x2080)	      ; Currency Symbols
    (6 #x2100)	      ; Letterlike Symbols and Number Forms
    (7 #x3000)))      ; CJK Symbols & Punctuation

(alexandria:define-constant +default-positions-for-dynamically-positioned-windows+
    #(#x0080  ; Latin-1 Supplement
      #x00C0  ; (combined partial Latin-1 Supplement/Latin Extended-A)
      #x0400  ; Cyrillic
      #x0600  ; Arabic
      #x0900  ; Devanagari
      #x3040  ; Hiragana
      #x30A0  ; Katakana
      #xFF00) ; Fullwidth ASCII
  :test 'equalp)
(declaim (type (array fixnum (8)) +default-positions-for-dynamically-positioned-windows+))

;;; Utils
(defun split-extended-window-tag (hbyte lbyte)
  (declare (type (unsigned-byte 8) hbyte lbyte))
  (values
   (ldb (byte 3 5) hbyte)		; window
   (+ #x10000 (* #x80			; offset
		 (+ (* (logand hbyte #x1f) #x100)
		    lbyte)))))
