(in-package :scsu)

;;; State
(defclass scsu-state ()
  ((mode :initform :single-byte-mode :accessor scsu-state-mode)
   (dynamic-window
    :initform (copy-seq +default-positions-for-dynamically-positioned-windows+)
    :accessor scsu-state-dynamic-window)
   (active-window-index :initform 0 :accessor scsu-state-active-window-index)))

(defun lookup-dynamic-window (state window)
  (aref (scsu-state-dynamic-window state) window))

(defun (setf lookup-dynamic-window) (offset state window)
  (setf (aref (scsu-state-dynamic-window state) window) offset))

(defmethod scsu-state-active-window-offset (state)
  (lookup-dynamic-window state (scsu-state-active-window-index state)))

(defmethod (setf scsu-state-active-window-offset) (offset state)
  (setf (lookup-dynamic-window state (scsu-state-active-window-index state))
	offset))


;;; Decoder
(defun read-two-bytes (stream)		; TODO: remove? (check behavior again..)
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
		       (+ (lookup-static-window window) next-byte))
		      (t
		       (+ (lookup-dynamic-window state window)
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

(defun decode (state stream)
  (let ((code-point 
	 (ecase (scsu-state state)
	   (:single-byte-mode (decode-in-single-byte-mode state stream))
	   (:unicode-mode (decode-in-unicode-mode state stream)))))
    (cond ((<= #xD800 code-point #xDBFF) ; high surrogate
	   (let ((low (decode state stream)))
	     (decode-from-surrogate-pair code-point low)))
	  ((<= #xDC00 code-point #xDFFF) ; low surrogate
	   (error "low surrogate appeared alone"))
	  (t
	   code-point))))


;;; Encode
(defun in-window-p (offset code-point)
  (declare (type (unsigned-byte 16) offset code-point))
  (<= offset code-point (+ offset #x7f)))

(defun find-suitable-window* (code-point offset-func)
  (loop for i of-type fixnum from 0 below +window-count+
     as offset = (funcall offset-func i code-point)
     when (in-window-p offset code-point)
     return i))

(defun find-suitable-static-window (code-point)
  (find-suitable-window* code-point #'lookup-static-window))

(defun find-suitable-dynamic-window (state code-point)
  (find-suitable-window* code-point
			 (lambda (i) (lookup-dynamic-window state i))))

(defun encode-in-single-byte-mode (state code-point stream)
  (cond ((< code-point #x20)		
	 (case code-point
	   ((#x0 #x9 #xA #xD) (progn))
	   (t (write-byte +SQU+ stream))) ; tags are quoted.
	 (write-byte code-point stream))
	((<= code-point #x7F)		; Basic Latin
	 (write-byte code-point stream))
	((in-window-p (scsu-state-active-window-offset state) code-point) ; in current dynamic window
	 (let ((offset (scsu-state-active-window-offset state)))
	   (write-byte (+ (- code-point offset) #x80) stream)))
	((let ((swindow (find-suitable-static-window code-point))) ; in static window
	   (when swindow
	     (let ((offset (lookup-static-window swindow)))
	       (write-byte (+ +SQ0+ swindow) stream)
	       (write-byte (- code-point offset) stream)))))
	((let ((dwindow (find-suitable-dynamic-window state code-point))) ; in other dynamic window
	   (when dwindow
	     (setf (scsu-state-active-window-index state) dwindow)
	     (let ((offset (lookup-dynamic-window state dwindow)))
	       ;; TODO: consider change or quote -- lookahead?
	       (write-byte (+ +SC0+ dwindow) stream)
	       (write-byte (+ (- code-point offset) #x80) stream)))))
	((= code-point #xFEFF)		; derived from SCSUmini
	 (write-byte +SQU+ stream)
	 (write-byte #xFE stream)
	 (write-byte #xFF stream))
	(t
	 ;; TODO:
	 ;; Define a new window, or goto unicode mode
	 ;; not-compressible => Unicode mode
	 (write-byte +SCU+ stream)
	 (setf (scsu-state-mode state) :unicode-mode)
	 (encode state code-point stream)
	 )))

(defun encode-in-unicode-mode (state code-point stream)
  (cond ((<= code-point #x7F)		; Basic Latin
	 (setf (scsu-state-mode state) :single-byte-mode)
	 (write-byte (+ +UC0+ (scsu-state-active-window-index state)) stream)
	 (encode state code-point stream))
	((let ((dwindow (find-suitable-dynamic-window state code-point))) ; in dynamic window
	   (when dwindow
	     (setf (scsu-state-mode state) :single-byte-mode)
	     (setf (scsu-state-active-window-index state) dwindow)
	     (write-byte (+ +UC0+ dwindow) stream)
	     (encode state code-point stream))))
	(t
	 (when (<= #xE000 code-point #xF2FF)
	   (write-byte +UQU+ stream))
	 (write-byte (ldb (byte 8 8) code-point) stream)
	 (write-byte (ldb (byte 8 0) code-point) stream))))

(defun encode (state code-point stream)
  (check-type code-point (integer 0 #x10FFFF))
  (when (> code-point #xFFFF)
    (multiple-value-bind (high low)
	(encode-to-surrogate-pair code-point)
      (encode state high stream)
      (return-from encode (encode state low stream))))
  (ecase (scsu-state state)
    (:single-byte-mode (encode-in-single-byte-mode state code-point stream))
    (:unicode-mode (encode-in-unicode-mode state code-point stream))))
