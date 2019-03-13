;;; -*- coding: utf-8; -*-

(in-package :cl-scsu)

;;; Types
(deftype window-index ()
  `(integer 0 7))

(deftype unicode-code-point ()
  `(integer 0 #x10FFFF))

;;; Condition
(define-condition scsu-error (error)
  ((src-error-position :initform nil :accessor scsu-error-src-error-position)
   (dst-error-position :initform nil :accessor scsu-error-dst-error-position)
   (parental-condition :initarg parental-condition :initform nil
		       :accessor scsu-error-parental-condition))
  (:report (lambda (condition stream)
	     (with-accessors ((original-format simple-condition-format-control)
			      (original-args simple-condition-format-arguments)
			      (src-pos scsu-error-src-error-position)
			      (dst-pos scsu-error-dst-error-position))
		 condition
	       (format stream "~? [at SRC ~A, DST ~A] "
		       original-format original-args src-pos dst-pos))))
  (:documentation "If some error occurs in `decode-to-string',
`encode-from-string', or `encode-reset-sequence', this condition is reported.
====
`decode-to-string', `encode-from-string', `encode-reset-sequence' で
エラーが発生した場合、この condition が報告される。"))


;;; Tags
(defconstant +window-count+ 8 "number of static or dynamic windows")
(declaim (type window-index +window-count+))

(defmacro define-window-find-function (name lower)
  `(defun ,name (byte)
     (declare (type (unsigned-byte 8) byte))
     ;; (assert (<= ,lower byte (+ ,lower +window-count+)))
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
  (flet ((raise-error-on-reserved-byte ()
	   (error 'scsu-error :format-control "reserved offset-table index ~X was used"
		  :format-arguments (list byte))))
    (cond ((= byte #x0)
	   (raise-error-on-reserved-byte)) ; reserved for internal use
	  ((<= #x1 byte #x67)
	   (* byte #x80))	   ; half-blocks from U+0080 to U+3380
	  ((<= #x68 byte #xA7)
	   (+ (* byte #x80) #xAC00)) ; half-blocks from U+E000 to U+FF80
	  ((<= #xA8 byte #xF8)
	   (raise-error-on-reserved-byte)) ; reserved for future use
	  (t (ecase byte
	       (#xF9 #x00C0) ; Latin-1 letters + half of Latin Extended-A
	       (#xFA #x0250) ; IPA Extensions
	       (#xFB #x0370) ; Greek
	       (#xFC #x0530) ; Armenian
	       (#xFD #x3040) ; Hiragana
	       (#xFE #x30A0) ; Katakana
	       (#xFF #xFF60))))))	; Halfwidth Katakana

(defun window-offset-to-table-index (offset)
  (declare (type unicode-code-point offset))
  (case offset
    (#x00C0 #xF9)	  ; Latin-1 letters + half of Latin Extended-A
    (#x0250 #xFA)	  ; IPA Extensions
    (#x0370 #xFB)	  ; Greek
    (#x0530 #xFC)	  ; Armenian
    (#x3040 #xFD)	  ; Hiragana
    (#x30A0 #xFE)	  ; Katakana
    (#xFF60 #xFF)	  ; Halfwidth Katakana
    (t (cond ((<= #x0080 offset #x3380)	; half-blocks from U+0080 to U+3380
	      (floor offset #x80))
	     ((<= #xE000 offset #xFF80) ; half-blocks from U+E000 to U+FF80
	      (floor (- offset #xAC00) #x80))
	     (t
	      nil)))))

(defun list-offset-candidates (code-point)
  (declare (type unicode-code-point code-point))
  (let ((ret nil))
    ;; special offsets
    (cond ((<= #x00C0 code-point (+ #x00C0 #x7F)) ; Latin-1 letters + half of Latin Extended-A
	   (push #x00C0 ret))
	  ((<= #x0250 code-point (+ #x0250 #x7F)) ; IPA Extensions
	   (push #x0250 ret))
	  ((<= #x0370 code-point (+ #x0370 #x7F)) ; Greek
	   (push #x0370 ret))
	  ((<= #x0530 code-point (+ #x0530 #x7F)) ; Armenian
	   (push #x0530 ret))
	  ((<= #x3040 code-point (+ #x3040 #x7F)) ; Hiragana
	   (push #x3040 ret))
	  ((<= #x30A0 code-point (+ #x30A0 #x7F)) ; Katakana
	   (push #x30A0 ret))
	  ((<= #xFF60 code-point (+ #xFF60 #x7F)) ; Halfwidth Katakana
	   (push #xFF60 ret)))
    ;; half blocks
    (when (or (<= #x0080 code-point (+ #x3380 #x7F)) ; half-blocks from U+0080 to U+3380
	      (<= #xE000 code-point (+ #xFF80 #x7F)) ; half-blocks from U+E000 to U+FF80
	      (<= #x10000 code-point))		     ; SMP
      (push (logandc2 code-point #x7F) ret))
    (nreverse ret)))


(alexandria:define-constant +static-windows+
    #(#x0000	      ; (for quoting of tags used in single-byte mode)
      #x0080	      ; Latin-1 Supplement
      #x0100	      ; Latin Extended-A
      #x0300	      ; Combining Diacritical Marks
      #x2000	      ; General Punctuation
      #x2080	      ; Currency Symbols
      #x2100	      ; Letterlike Symbols and Number Forms
      #x3000)	      ; CJK Symbols & Punctuation
  :test 'equalp)
(declaim (type (array fixnum (8)) +static-windows+))

(defun lookup-static-window (window)
  (declare (type window-index window))
  (aref +static-windows+ window))

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
(defun standalone-character-p (code-point)
  (declare (type unicode-code-point code-point))
  (or (= code-point #xFEFF)		; signature
      (<= #xFDD0 code-point #xFDEF)	; non-character
      (<= #xFFF0 code-point #xFFFD)	; specials
      (let ((in-plane-pos (logand code-point #xFFFF))) ; non-character
	(<= #xFFFE in-plane-pos #xFFFF))))

(defun encode-extended-window-tag (window offset)
  (declare (type window-index window)
	   (type unicode-code-point offset))
  (let* ((off-tmp (floor (- offset #x10000) #x80))
	 (hbyte (logand #x1f (floor off-tmp #x100)))
	 (lbyte (logand #xff off-tmp)))
    (declare (type unicode-code-point off-tmp)
	     (type (unsigned-byte 8) hbyte lbyte))
    (setf (ldb (byte 3 5) hbyte) window) ; set window. FIXME: should make hbyte with one step?
    (values hbyte lbyte)))

(defun decode-extended-window-tag (hbyte lbyte)
  (declare (type (unsigned-byte 8) hbyte lbyte))
  (values
   (ldb (byte 3 5) hbyte)		; window
   (+ #x10000 (* #x80			; offset
		 (+ (* (logand hbyte #x1f) #x100)
		    lbyte)))))

;; (FIXME: use other implementation?)
(defun encode-to-surrogate-pair (code-point)
  (declare (type unicode-code-point code-point))
  (if (<= code-point #xFFFF)
      code-point
      (let* ((off-code-point (- code-point #x10000))
	     (high (+ #xD800 (ldb (byte 10 10) off-code-point)))
	     (low (+ #xDC00 (ldb (byte 10 0) off-code-point))))
	(declare (type (integer 0 #xFFFFF) off-code-point))
	(values high low))))

(defun decode-from-surrogate-pair (high low)
  (declare (type (unsigned-byte 16) high low))
  (+ #x10000
     (ash (- high #xD800) 10)
     (- low #xDC00)))
