(in-package :scsu)

;;; State
(defclass scsu-state ()
  ((mode :initform :single-byte-mode :accessor scsu-state-mode)
   (dynamic-window
    :initform (copy-seq +default-positions-for-dynamically-positioned-windows+)
    :accessor scsu-state-dynamic-window)
   (active-window-index :initform 0 :accessor scsu-state-active-window-index)))

(defun lookup-dynamic-window-position (state window)
  (aref (scsu-state-dynamic-window state) window))

(defun (setf lookup-dynamic-window-position) (offset state window)
  (setf (aref (scsu-state-dynamic-window state) window) offset))

(defmethod scsu-state-active-window-offset (state)
  (lookup-dynamic-window-position state (scsu-state-active-window-index state)))

(defmethod (setf scsu-state-active-window-offset) (offset state)
  (setf (lookup-dynamic-window-position state (scsu-state-active-window-index state))
	offset))


;;; Decoder
(defun read-two-bytes (stream)
  (let* ((byte1 (read-byte stream))
	 (byte2
	  (handler-bind ((error (lambda (c)
				  (declare (ignore c))
				  ;; TODO: unread byte
				  #+ignore(unread-byte byte1 stream))))
	    (read-byte stream))))
    (values byte1 byte2)))

(defun scsu-quote-unicode (stream)
  (multiple-value-bind (next-byte1 next-byte2)
      (read-two-bytes stream)
    (declare (type (unsigned-byte 8) next-byte1 next-byte2))
    (logior (ash next-byte1 8) next-byte2)))

(defun scsu-change-to-window (state window)
  (declare (type (integer 0 7) window))
  (setf (scsu-state-active-window-index state) window))

(defun scsu-define-window (state window offset)
  (declare (type (integer 0 #x10FFFF) offset))
  (scsu-change-to-window state window)  
  (setf (scsu-state-active-window-offset state) offset))

(defun scsu-define-window-extended (state stream)
  (multiple-value-bind (next-byte1 next-byte2)
      (read-two-bytes stream)
    (multiple-value-bind (window offset)
	(split-extended-window-tag next-byte1 next-byte2)
      (scsu-define-window state window offset))))

(defun decode-in-single-byte-mode (state stream)
  (let ((byte (read-byte stream)))
    (declare (type (unsigned-byte 8) byte))
    (cond ((<= 0 byte #x1F)		; Tag bytes.
	   (ecase byte
	     ((#x0 #x9 #xA #xD)		; pass
	      byte)
	     ((+SQ0+ +SQ1+ +SQ2+ +SQ3+ +SQ4+ +SQ5+ +SQ6+ +SQ7+) ; Quote from Window
	      (let ((window (find-SQn-window byte))
		    (next-byte (read-byte stream)))
		(declare (type (integer 0 7) window)
			 (type (unsigned-byte 8) next-byte))
		(cond ((<= #x0 next-byte #x7f)
		       (+ (lookup-static-window-position window) next-byte))
		      (t
		       (+ (lookup-dynamic-window-position state window)
			  (logand next-byte #x7f))))))
	     (+SDX+			; Define Extended
	      (scsu-define-window-extended state stream)
	      (decode state stream))
	     (#xC
	      (error "reserved byte ~A is used" byte))
	     (+SQU+			; Quote Unicode
	      (scsu-quote-unicode stream))
	     (+SCU+			; Change to Unicode
	      (setf (scsu-state-mode state) :unicode-mode)
	      (decode state stream))
	     ((+SC0+ +SC1+ +SC2+ +SC3+ +SC4+ +SC5+ +SC6+ +SC7+) ; Change to Window
	      (scsu-change-to-window state (find-SCn-window byte))
	      (decode state stream))
	     ((+SD0+ +SD1+ +SD2+ +SD3+ +SD4+ +SD5+ +SD6+ +SD7+) ; Define Window
	      (scsu-define-window state
				  (find-SDn-window byte)
				  (lookup-window-offset-table (read-byte stream)))
	      (decode state stream))))
	  ((<= byte #x7F)		  ; Basic Latin Block.
	   byte)
	  (t				; In active dynamic window.
	   (let ((offset (scsu-state-active-window-offset state))
		 (position (logand byte #x7F))) ; (- byte #x80)
	     (declare (type (integer 0 #x10FFFF) offset)
		      (type (integer 0 #x7F) position))
	     (+ offset position))))))

(defun decode-in-unicode-mode (state stream)
  (let ((byte (read-byte stream)))
    (declare (type (unsigned-byte 8) byte))
    (if (or (<= #x00 byte #xDF) (<= #xF3 byte #xFF)) ; MSB
	(+ (logior (ash byte 8) (read-byte stream)))
	(ecase byte
	  ((+UC0+ +UC1+ +UC2+ +UC3+ +UC4+ +UC5+ +UC6+ +UC7+) ; Change to Window
	   (scsu-change-to-window state (find-UCn-window byte))
	   (setf (scsu-state-mode state) :single-byte-mode)
	   (decode state stream))
	  ((+UD0+ +UD1+ +UD2+ +UD3+ +UD4+ +UD5+ +UD6+ +UD7+) ; Define Window
	   (scsu-define-window state
			       (find-UDn-window byte)
			       (lookup-window-offset-table (read-byte stream)))
	   (setf (scsu-state-mode state) :single-byte-mode)
	   (decode state stream))
	  (+UQU+			; Quote Unicode
	   (scsu-quote-unicode stream))
	  (+UDX+			; Define Extended
	   (scsu-define-window-extended state stream)
	   (setf (scsu-state-mode state) :single-byte-mode)
	   (decode state stream))
	  (#xF2
	   (error "reserved byte ~A is used" byte))))))


;; TODO: support surrogate decode

(defun decode (state stream)
  (ecase (scsu-state state)
    (:single-byte-mode (decode-in-single-byte-mode state stream))
    (:unicode-mode (decode-in-unicode-mode state stream))))


;;; Encode
(defun encode (state code-point stream)
  (check-type code-point (integer 0 #x10FFFF))
  (when (> code-point #xFFFF)
    (let* ((tmp (- code-point #x10000))
	   (high (+ #xD800 (ldb (byte 10 10) tmp)))
	   (low (+ #xDC00 (ldb (byte 10 0) tmp))))
      (encode state high stream)
      (return-from encode (encode state low stream))))
  (cond ((< code-point #x20)		
	 (case code-point
	   ((#x0 #x9 #xA #xD) (progn))
	   (t (write-byte +SQU+ stream))) ; tags are quoted.
	 (write-byte code-point stream))
	((<= code-point #x7F)		; Basic Latin
	 (write-byte code-point stream))
	;; ()				; in current dynamic window
	;; ()				; in static window
	;; ()				; in other dynamic window
	;; ()				; U+FEFF
	(t
	 ;; Define a new window, or goto unicode mode
	 ;; not-compressible => Unicode mode
	 ;;
	 )))
  

