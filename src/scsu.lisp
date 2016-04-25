(in-package :scsu)

;;; State
(defclass scsu-state ()
  ((mode :initform :single-byte-mode :accessor scsu-state-mode)
   (dynamic-window
    :initform +default-positions-for-dynamically-positioned-windows+
    :accessor scsu-state-dynamic-window)
   (active-window-index :initform 0 :accessor scsu-state-active-window-index)))

(defun lookup-dynamic-window (state window)
  (declare (type window-index window))
  (aref (scsu-state-dynamic-window state) window))

(defun (setf lookup-dynamic-window) (offset state window)
  (declare (type unicode-code-point offset)
	   (type window-index window))
  (with-accessors ((dwindow scsu-state-dynamic-window))
      state
    (when (eq dwindow +default-positions-for-dynamically-positioned-windows+)
      (setf dwindow (copy-seq +default-positions-for-dynamically-positioned-windows+)))
    (setf (aref dwindow window) offset)))

(defmethod scsu-state-active-window-offset (state)
  (lookup-dynamic-window state (scsu-state-active-window-index state)))

(defmethod (setf scsu-state-active-window-offset) (offset state)
  (declare (type unicode-code-point offset))
  (setf (lookup-dynamic-window state (scsu-state-active-window-index state))
	offset))

(defmethod scsu-state-reset ((state scsu-state))
  (slot-makunbound state 'mode)
  (slot-makunbound state 'dynamic-window)
  (slot-makunbound state 'active-window-index)
  (shared-initialize state t))


;;; Condition
(define-condition scsu-error (error)
  ((start-position :accessor scsu-decode-error-start-position)
   (error-position :accessor scsu-decode-error-error-position)))

(define-condition scsu-encode-error (scsu-error)
  ())

(define-condition scsu-decode-error (scsu-error)
  ())


;;; Util
(defmacro with-reading-function ((function current)
				 (buffer start end &key (element-type '(unsigned-byte 8)))
				 &body body)
  (alexandria:once-only (buffer start end)
    `(locally (declare (type fixnum ,start ,end)
		       (type (array ,element-type (*)) ,buffer))
       (let ((,current ,start))
	 (declare (type fixnum ,current))
	 (flet ((,function ()
		  (cond ((< ,current ,end)
			 (prog1 (aref ,buffer ,current)
			   (incf ,current)))
			(t
			 (error 'scsu-error :format-control "Reached to the end of the buffer")))))
	   ,@body)))))

(defmacro with-writing-function ((function current)
				 (buffer start end &key (element-type '(unsigned-byte 8)))
				 &body body)
  (alexandria:once-only (buffer start end)
    `(locally (declare (type fixnum ,start ,end)
		       (type (array ,element-type (*)) ,buffer))
       (let ((,current ,start))
	 (declare (type fixnum ,current))
	 (flet ((,function (c)
		  (cond ((< ,current ,end)
			 (setf (aref ,buffer ,current) c)
			 (incf ,current))
			((array-has-fill-pointer-p ,buffer)
			 (vector-push-extend c ,buffer) ; TODO: wrap error from outer.
			 (incf ,current))
			(t
			 (error 'scsu-error :format-control "Reached to the end of the string")))))
	   ,@body)))))


;;; Decoder
(deftype read-func-type ()
  '(function () (unsigned-byte 8)))

(defun scsu-quote-unicode (read-func)
  (declare (type read-func-type read-func))
  (let ((next-byte1 (funcall read-func))
	(next-byte2 (funcall read-func)))
    (declare (type (unsigned-byte 8) next-byte1 next-byte2))
    (logior (ash next-byte1 8) next-byte2)))

(defun scsu-change-to-window (state window)
  (declare (type window-index window))
  (setf (scsu-state-active-window-index state) window))

(defun scsu-define-window (state window offset)
  (declare (type window-index window)
	   (type unicode-code-point offset))
  (scsu-change-to-window state window)  
  (setf (scsu-state-active-window-offset state) offset))

(defun scsu-define-window-extended (state read-func)
  (declare (type read-func-type read-func))
  (let ((next-byte1 (funcall read-func))
	(next-byte2 (funcall read-func)))
    (declare (type (unsigned-byte 8) next-byte1 next-byte2))
    (multiple-value-bind (window offset)
	(split-extended-window-tag next-byte1 next-byte2)
      (scsu-define-window state window offset))))

(defun decode-unit*/single-byte-mode (state read-func)
  (declare (type read-func-type read-func))
  (let ((byte (funcall read-func)))
    (declare (type (unsigned-byte 8) byte))
    (cond ((<= 0 byte #x1F)		; Tag bytes.
	   (ecase byte
	     ((#x0 #x9 #xA #xD)		; pass
	      byte)
	     ((#.+SQ0+ #.+SQ1+ #.+SQ2+ #.+SQ3+ #.+SQ4+ #.+SQ5+ #.+SQ6+ #.+SQ7+) ; Quote from Window
	      (let ((window (find-SQn-window byte))
		    (next-byte (funcall read-func)))
		(declare (type window-index window)
			 (type (unsigned-byte 8) next-byte))
		(cond ((<= #x0 next-byte #x7f)
		       (+ (lookup-static-window window) next-byte))
		      (t
		       (+ (lookup-dynamic-window state window)
			  (logand next-byte #x7f))))))
	     (#.+SDX+			; Define Extended
	      (scsu-define-window-extended state read-func)
	      (decode-unit* state read-func))
	     (#xC
	      (error 'scsu-decode-error
		     :format-control "reserved byte ~A is used"
		     :format-arguments (list byte)))
	     (#.+SQU+			; Quote Unicode
	      (scsu-quote-unicode read-func))
	     (#.+SCU+			; Change to Unicode
	      (setf (scsu-state-mode state) :unicode-mode)
	      (decode-unit* state read-func))
	     ((#.+SC0+ #.+SC1+ #.+SC2+ #.+SC3+ #.+SC4+ #.+SC5+ #.+SC6+ #.+SC7+) ; Change to Window
	      (scsu-change-to-window state (find-SCn-window byte))
	      (decode-unit* state read-func))
	     ((#.+SD0+ #.+SD1+ #.+SD2+ #.+SD3+ #.+SD4+ #.+SD5+ #.+SD6+ #.+SD7+) ; Define Window
	      (scsu-define-window state
				  (find-SDn-window byte)
				  (lookup-window-offset-table (funcall read-func)))
	      (decode-unit* state read-func))))
	  ((<= byte #x7F)		  ; Basic Latin Block.
	   byte)
	  (t				; In active dynamic window.
	   (let ((offset (scsu-state-active-window-offset state))
		 (position (logand byte #x7F))) ; (- byte #x80)
	     (declare (type unicode-code-point offset)
		      (type (unsigned-byte 8) position))
	     (+ offset position))))))

(defun decode-unit*/unicode-mode (state read-func)
  (declare (type read-func-type read-func))
  (let ((byte (funcall read-func)))
    (declare (type (unsigned-byte 8) byte))
    (if (or (<= #x00 byte #xDF) (<= #xF3 byte #xFF)) ; MSB
	(+ (logior (ash byte 8) (funcall read-func)))
	(ecase byte
	  ((#.+UC0+ #.+UC1+ #.+UC2+ #.+UC3+ #.+UC4+ #.+UC5+ #.+UC6+ #.+UC7+) ; Change to Window
	   (scsu-change-to-window state (find-UCn-window byte))
	   (setf (scsu-state-mode state) :single-byte-mode)
	   (decode-unit* state read-func))
	  ((#.+UD0+ #.+UD1+ #.+UD2+ #.+UD3+ #.+UD4+ #.+UD5+ #.+UD6+ #.+UD7+) ; Define Window
	   (scsu-define-window state
			       (find-UDn-window byte)
			       (lookup-window-offset-table (funcall read-func)))
	   (setf (scsu-state-mode state) :single-byte-mode)
	   (decode-unit* state read-func))
	  (#.+UQU+			; Quote Unicode
	   (scsu-quote-unicode read-func))
	  (#.+UDX+			; Define Extended
	   (scsu-define-window-extended state read-func)
	   (setf (scsu-state-mode state) :single-byte-mode)
	   (decode-unit* state read-func))
	  (#xF2
	   (error 'scsu-decode-error
		  :format-control "reserved byte ~A is used"
		  :format-arguments (list byte)))))))

(defun decode-unit* (state read-func)
  (declare (type read-func-type read-func))
  ;; TODO: handle errors
  ;; TODO: restore state if error??
  ;; -- bind state to dynamic variables? bind locally? use internal slot? (BAD)
  (let ((code-point 
	 (ecase (scsu-state-mode state)
	   (:single-byte-mode (decode-unit*/single-byte-mode state read-func))
	   (:unicode-mode (decode-unit*/unicode-mode state read-func)))))
    (declare (type (unsigned-byte 16) code-point))
    (cond ((<= #xD800 code-point #xDBFF) ; high surrogate
	   (let ((low (decode-unit* state read-func))) ; TODO: catch error.
	     (declare (type (unsigned-byte 16) low))
	     (unless (<= #xDC00 low #xDFFF)
	       (error 'scsu-decode-error
		      :format-control "High surrogate appeared alone"))
	     (decode-from-surrogate-pair code-point low)))
	  ((<= #xDC00 code-point #xDFFF) ; low surrogate
	   (error 'scsu-decode-error
		  :format-control "Low surrogate appeared alone"))
	  (t
	   code-point))))

(defun decode-to-string (bytes
			 &key (src-start 0) (src-end (length bytes))
			   (string (make-array (floor (length bytes) 2) ; no way to expect length..
					       :element-type 'character
					       :fill-pointer 0 :adjustable t))
			   (dst-start 0)
			   (dst-end (length string))
			   (state (make-instance 'scsu-state)))
  (declare (type (array (unsigned-byte 8) *) bytes)
	   (type fixnum src-start src-end dst-start dst-end)
	   (type string string))
  ;; TODO: merge with above?
  ;; TODO: Don't reduce to last bytes..
  (with-reading-function (pick-byte src-current)
      (bytes src-start src-end)
    (with-writing-function (put-char dst-current)
	(string dst-start dst-end :element-type character)
      (loop while (< src-current src-end) ; TODO: should return even after a completed control byte.
	 as code-point of-type unicode-code-point = (decode-unit* state #'pick-byte)
	 do (put-char (code-char code-point)))
      (values string dst-current state src-current))))

(defun decode-unit-to-bytes (bytes &key (start 0) (end (length bytes))
				     (state (make-instance 'scsu-state)))
  (let* ((ret-list (multiple-value-list 
		    (decode-to-string bytes :src-start start :src-end end :state state)))
	 (ret-string (first ret-list)))
    (declare (type list ret-list)
	     (type string ret-string))	     
    (apply #'values (char ret-string 0) (rest ret-list))))


;;; Encode
(deftype write-func-type ()
  '(function ((unsigned-byte 8)) t))

(defun in-window-p (offset code-point)
  (declare (type unicode-code-point offset code-point))
  (<= offset code-point (+ offset #x7f)))

(defun find-suitable-window* (code-point offset-func)
  (declare (type unicode-code-point code-point)
	   (type (function (fixnum)) offset-func))
  (loop for i of-type fixnum from 0 below +window-count+
     as offset of-type unicode-code-point = (funcall offset-func i)
     when (in-window-p offset code-point)
     return i))

(defun find-suitable-static-window (code-point)
  (declare (type unicode-code-point code-point))
  (find-suitable-window* code-point #'lookup-static-window))

(defun find-suitable-dynamic-window (state code-point)
  (declare (type unicode-code-point code-point))
  (find-suitable-window* code-point
			 (lambda (i)
			   (declare (type window-index i))
			   (lookup-dynamic-window state i))))

(defun encode-unit*/single-byte-mode (state code-point write-func) ; TODO: add 'fixed?' parameter
  (declare (type unicode-code-point code-point)
	   (type write-func-type write-func))
  (cond ((< code-point #x20)		
	 (case code-point
	   ((#x0 #x9 #xA #xD) (progn))
	   (t (funcall write-func +SQU+))) ; tags are quoted.
	 (funcall write-func code-point))
	((<= code-point #x7F)		; Basic Latin
	 (funcall write-func code-point))
	((in-window-p (scsu-state-active-window-offset state) code-point) ; in current dynamic window
	 (let ((offset (scsu-state-active-window-offset state)))
	   (declare (type unicode-code-point offset))
	   (funcall write-func (+ (- code-point offset) #x80))))
	((let ((swindow (find-suitable-static-window code-point))) ; in static window
	   (when swindow
	     (locally (declare (type window-index swindow))
	       (let ((offset (lookup-static-window swindow)))
		 (declare (type unicode-code-point offset))
		 (funcall write-func (+ +SQ0+ swindow))
		 (funcall write-func (- code-point offset)))))))
	((let ((dwindow (find-suitable-dynamic-window state code-point))) ; in other dynamic window
	   (when dwindow
	     (locally (declare (type window-index dwindow))
	       (setf (scsu-state-active-window-index state) dwindow)
	       (let ((offset (lookup-dynamic-window state dwindow)))
		 (declare (type unicode-code-point offset))
		 ;; TODO: consider change or quote -- lookahead?
		 (funcall write-func (+ +SC0+ dwindow))
		 (funcall write-func (+ (- code-point offset) #x80)))))))
	((= code-point #xFEFF)		; derived from SCSUmini
	 (funcall write-func +SQU+)
	 (funcall write-func #xFE)
	 (funcall write-func #xFF))
	(t
	 ;; TODO:
	 ;; Define a new window, or goto unicode mode
	 ;; not-compressible => Unicode mode
	 (funcall write-func +SCU+)
	 (setf (scsu-state-mode state) :unicode-mode)
	 (encode-unit* state code-point write-func))))

(defun encode-unit*/unicode-mode (state code-point write-func)
  (declare (type unicode-code-point code-point)
	   (type write-func-type write-func))
  (cond ((<= code-point #x7F)		; Basic Latin
	 (setf (scsu-state-mode state) :single-byte-mode)
	 (funcall write-func (+ +UC0+ (scsu-state-active-window-index state)))
	 (encode-unit* state code-point write-func))
	((let ((dwindow (find-suitable-dynamic-window state code-point))) ; in dynamic window
	   (when dwindow
	     (locally (declare (type window-index dwindow))
	       (setf (scsu-state-mode state) :single-byte-mode)
	       (setf (scsu-state-active-window-index state) dwindow)
	       (funcall write-func (+ +UC0+ dwindow))
	       (encode-unit* state code-point write-func)))))
	(t
	 (when (<= #xE000 code-point #xF2FF)
	   (funcall write-func +UQU+))
	 (funcall write-func (ldb (byte 8 8) code-point))
	 (funcall write-func (ldb (byte 8 0) code-point)))))

(defun encode-unit* (state code-point write-func)
  (declare (type unicode-code-point code-point)
	   (type write-func-type write-func))
  (cond ((> code-point #xFFFF)
	 (multiple-value-bind (high low)
	     (encode-to-surrogate-pair code-point)
	   (declare (type (unsigned-byte 16) high low))
	   (encode-unit* state high write-func)
	   (encode-unit* state low write-func)))
	(t
	 (ecase (scsu-state-mode state)
	   (:single-byte-mode (encode-unit*/single-byte-mode state code-point write-func))
	   (:unicode-mode (encode-unit*/unicode-mode state code-point write-func))))))

(defun encode-from-string (string
			   &key (src-start 0) (src-end (length string))
			     (bytes (make-array (length string) ; no way to expect length..
						:fill-pointer 0 :adjustable t
						:element-type '(unsigned-byte 8)))
			     (dst-start 0) ; TODO: change name to ":start1, :start2"?
			     (dst-end (length bytes))
			     (state (make-instance 'scsu-state)))
  (declare (type string string)
	   (type (array (unsigned-byte 8) *) bytes)	   
	   (type fixnum src-start src-end dst-start dst-end))
  (with-writing-function (put-byte current)
      (bytes dst-start dst-end :element-type (unsigned-byte 8))
    (loop for i of-type fixnum from src-start below src-end
       as c of-type character = (char string i)
       do (encode-unit* state (char-code c) #'put-byte))
    (values bytes current state)))

;; The required length is out of BMP case.
;; - SQU, <high surrogate 1>, <high surrogate 2>, SQU, <low surrogate 1>, <low surrogate 2>
;; - SDX, <high>, <low>, <byte>
(defconstant +encode-unit-default-bytes-length+ 6)

(defun encode-unit-to-bytes (code-point
			     &key (bytes (make-array +encode-unit-default-bytes-length+
						     :fill-pointer 0
						     :element-type '(unsigned-byte 8)))
			       (start 0)
			       (end (length bytes))
			       (state (make-instance 'scsu-state)))
  (declare (type fixnum start end)
	   (type (array (unsigned-byte 8) *) bytes))
  (check-type code-point (integer 0 #x10FFFF))
  (let ((tmp-string (make-array 1 :element-type 'character)))
    (declare (type simple-string tmp-string)
	     (dynamic-extent tmp-string))
    (setf (schar tmp-string 0) code-point)
    (encode-from-string tmp-string :src-start 0 :src-end 1
			:bytes bytes :dst-start start :dst-end end :state state)))
