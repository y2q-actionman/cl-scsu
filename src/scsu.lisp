(in-package :cl-scsu)

(defvar *scsu-state-trace* nil
  "If set to T, outputs tag or character informations seen by the decoder to *trace-output*")

;;; State
(defclass scsu-state ()
  ((mode :initform :single-byte-mode :accessor scsu-state-mode)
   (dynamic-window :initform nil	; see lookup-dynamic-window
		   :accessor scsu-state-dynamic-window)
   (active-window-index :initform 0 :accessor scsu-state-active-window-index)
   (timestamp-vector :initform nil	; see scsu-state-timestamp and initialize-timestamp
		     :accessor scsu-state-timestamp-vector)
   (current-timestamp :initform 0 :accessor scsu-state-current-timestamp :type fixnum)
   (fix-dynamic-window :initarg :fix-dynamic-window :initform nil
		       :accessor scsu-state-fix-dynamic-window :type boolean)
   ))

(defmethod lookup-dynamic-window (state window)
  (declare (type window-index window))
  (let ((dwindow (or (scsu-state-dynamic-window state)
		     +default-positions-for-dynamically-positioned-windows+)))
    (declare (type (array fixnum (8)) dwindow))
    (aref dwindow window)))

(defmethod (setf lookup-dynamic-window) (offset state window)
  (declare (type unicode-code-point offset)
	   (type window-index window))
  (let ((dwindow (scsu-state-dynamic-window state)))
    (when (null dwindow)
      (setf dwindow (copy-seq +default-positions-for-dynamically-positioned-windows+)
	    (scsu-state-dynamic-window state) dwindow))
    (locally (declare (type (array fixnum (8)) dwindow))
      (setf (aref dwindow window) offset))))

(defmethod scsu-state-active-window-offset (state)
  (lookup-dynamic-window state (scsu-state-active-window-index state)))

(defmethod (setf scsu-state-active-window-offset) (offset state)
  (declare (type unicode-code-point offset))
  (setf (lookup-dynamic-window state (scsu-state-active-window-index state))
	offset))

(defmethod scsu-state-timestamp (state window)
  (declare (type window-index window))
  ;; (assert (scsu-state-timestamp-vector state))
  (aref (scsu-state-timestamp-vector state) window))

(defmethod (setf scsu-state-timestamp) (val state window)
  (declare (type window-index window))
  ;; (assert (scsu-state-timestamp-vector state))
  (setf (aref (scsu-state-timestamp-vector state) window) val))

(defmethod scsu-state-reset ((state scsu-state))
  (slot-makunbound state 'mode)
  (slot-makunbound state 'dynamic-window)
  (slot-makunbound state 'active-window-index)
  (slot-makunbound state 'timestamp-vector)
  (slot-makunbound state 'current-timestamp)
  (shared-initialize state t))


;;; Util
(defmacro with-scsu-error-handling
    ((state &key src dst return) &body body)
  (alexandria:with-gensyms (%state %m %d-w %d-w-contents %a-w-i %t-v %t-v-contents %c-t %src %dst)
    `(let* ((,%state ,state)
	    (,%m (slot-value ,%state 'mode))
	    (,%d-w (slot-value ,%state 'dynamic-window))
	    (,%d-w-contents (make-array +window-count+ :element-type 'fixnum))
	    (,%a-w-i (slot-value ,%state 'active-window-index))
	    (,%t-v (slot-value ,%state 'timestamp-vector))
	    (,%t-v-contents (make-array +window-count+ :element-type 'fixnum))
	    (,%c-t (slot-value ,%state 'current-timestamp))
	    (,%src ,src) (,%dst ,dst))	; Holds a restartable state.
       (declare (type (simple-array fixnum 8) ,%d-w-contents ,%t-v-contents)
		(dynamic-extent ,%d-w-contents ,%t-v-contents))
       (when ,%d-w
	 (replace ,%d-w-contents ,%d-w))
       (when ,%t-v
	 (replace ,%t-v-contents ,%t-v))
       (restart-case
	   (handler-bind ((scsu-error
			   (alexandria:named-lambda scsu-error-filler (c)
			     (setf (scsu-error-src-error-position c) ,src ; evaluate at this point!
				   (scsu-error-dst-error-position c) ,dst)
			     )))	; Decline
	     (progn ,@body))
	 (restore-state ()
	   :report "Restore SCSU state to a restartable previous state, and return"
	   (setf (slot-value ,%state 'mode) ,%m
		 (slot-value ,%state 'dynamic-window) (if ,%d-w (replace ,%d-w ,%d-w-contents) nil)
		 (slot-value ,%state 'active-window-index) ,%a-w-i
		 (slot-value ,%state 'timestamp-vector) (if ,%t-v (replace ,%t-v ,%t-v-contents) nil)
		 (slot-value ,%state 'current-timestamp) ,%c-t)
	   (funcall ,return ,%dst ,%src))))))

(defmacro with-buffer-accessor ((&key (reader (gensym) reader-supplied-p)
				      (writer (gensym) writer-supplied-p)
				      current)
				   (buffer start end &key (element-type '*))
				&body body)
  (alexandria:once-only (buffer start end)
    (alexandria:with-gensyms (%raise-scsu-error)
      `(locally (declare (type fixnum ,start ,end)
			 (type (array ,element-type (*)) ,buffer))
	 (let ((,current ,start))
	   (declare (type fixnum ,current))
	   (labels
	       ((,%raise-scsu-error (fmt &rest args)
		  (apply #'error 'scsu-error :format-control fmt args))
		(,reader (&optional (peek-mode nil))
		  (cond ((< ,current ,end)
			 (prog1 (aref ,buffer ,current)
			   (unless peek-mode
			     (incf ,current))))
			(t (if peek-mode
			       nil
			       (,%raise-scsu-error "Reached to the end of the read buffer")))))
		(,writer (c)
		  (let ((fmt "Reached to the end of the write buffer"))
		    (cond ((< ,current ,end)
			   (setf (aref ,buffer ,current) c)
			   (incf ,current))
			  ((array-has-fill-pointer-p ,buffer)
			   (handler-case (vector-push-extend c ,buffer)
			     (error (c)
			       (,%raise-scsu-error fmt :parental-condition c))) ; wraps error.
			   (incf ,current))
			  (t
			   (,%raise-scsu-error fmt))))))
	     (declare (ignore ,@(if (not reader-supplied-p) `((function ,reader)))
			      ,@(if (not writer-supplied-p) `((function ,writer)))))
	     ,@body))))))

(defun scsu-trace-output (format-control &rest format-arguments)
  (when *scsu-state-trace*
    (apply #'format *trace-output* format-control format-arguments)))


;;; Decoder
(deftype read-func-type ()
  '(function () (or null (unsigned-byte 8))))

(defun decode-quote-unicode (read-func)
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

(defun decode-define-window-extended (state read-func)
  (declare (type read-func-type read-func))
  (let ((next-byte1 (funcall read-func))
	(next-byte2 (funcall read-func)))
    (declare (type (unsigned-byte 8) next-byte1 next-byte2))
    (multiple-value-bind (window offset)
	(decode-extended-window-tag next-byte1 next-byte2)
      (scsu-define-window state window offset))))

(defun decode-unit*/single-byte-mode (state read-func)
  (declare (type read-func-type read-func))
  (let ((byte (funcall read-func)))
    (declare (type (unsigned-byte 8) byte))
    (cond ((<= 0 byte #x1F)		; Tag bytes.
	   (ecase byte
	     ((#x0 #x9 #xA #xD)		; pass
	      (scsu-trace-output "[single-byte ASCII ~X]" byte)
	      byte)
	     ((#.+SQ0+ #.+SQ1+ #.+SQ2+ #.+SQ3+ #.+SQ4+ #.+SQ5+ #.+SQ6+ #.+SQ7+) ; Quote from Window
	      (let ((window (find-SQn-window byte))
		    (next-byte (funcall read-func)))
		(declare (type window-index window)
			 (type (unsigned-byte 8) next-byte))
		(scsu-trace-output "SQ~D " window)
		(cond ((<= #x0 next-byte #x7f)
		       (scsu-trace-output "[single quote static ~X]" next-byte)
		       (+ (lookup-static-window window) next-byte))
		      (t
		       (scsu-trace-output "[single quote dynamic ~X]" next-byte)
		       (+ (lookup-dynamic-window state window)
			  (logand next-byte #x7f))))))
	     (#.+SDX+			; Define Extended
	      (scsu-trace-output "SDX ")
	      (decode-define-window-extended state read-func)
	      '+SDX)
	     (#xC
	      (error 'scsu-error
		     :format-control "reserved byte ~A is used"
		     :format-arguments (list byte)))
	     (#.+SQU+			; Quote Unicode
	      (scsu-trace-output "SQU ")
	      (let ((char (decode-quote-unicode read-func)))
		(prog1 char
		  (scsu-trace-output "[quote unicode ~X]" char))))
	     (#.+SCU+			; Change to Unicode
	      (scsu-trace-output "SCU ")
	      (setf (scsu-state-mode state) :unicode-mode)
	      '+SCU+)
	     ((#.+SC0+ #.+SC1+ #.+SC2+ #.+SC3+ #.+SC4+ #.+SC5+ #.+SC6+ #.+SC7+) ; Change to Window
	      (let ((window (find-SCn-window byte)))
		(scsu-trace-output "SC~D " window)
		(scsu-change-to-window state window))
	      '+SC0+)
	     ((#.+SD0+ #.+SD1+ #.+SD2+ #.+SD3+ #.+SD4+ #.+SD5+ #.+SD6+ #.+SD7+) ; Define Window
	      (let ((window (find-SDn-window byte)))
		(scsu-trace-output "SD~D " window)
		(scsu-define-window state window
				    (lookup-window-offset-table (funcall read-func))))
	      '+SD0+)))
	  ((<= byte #x7F)		  ; Basic Latin Block.
	   (scsu-trace-output "[single-byte ASCII ~X]" byte)
	   byte)
	  (t				; In active dynamic window.
	   (let ((offset (scsu-state-active-window-offset state))
		 (position (logand byte #x7F))) ; (- byte #x80)
	     (declare (type unicode-code-point offset)
		      (type (unsigned-byte 8) position))
	     (scsu-trace-output "[single-byte dynamic window ~X]" byte)
	     (+ offset position))))))

(defun decode-unit*/unicode-mode (state read-func)
  (declare (type read-func-type read-func))
  (let ((byte (funcall read-func)))
    (declare (type (unsigned-byte 8) byte))
    (case byte
      ((#.+UC0+ #.+UC1+ #.+UC2+ #.+UC3+ #.+UC4+ #.+UC5+ #.+UC6+ #.+UC7+) ; Change to Window
       (let ((window (find-UCn-window byte)))
	 (scsu-trace-output "UC~D " window)
	 (scsu-change-to-window state window)
	 (setf (scsu-state-mode state) :single-byte-mode))
       '+UC0+)
      ((#.+UD0+ #.+UD1+ #.+UD2+ #.+UD3+ #.+UD4+ #.+UD5+ #.+UD6+ #.+UD7+) ; Define Window
       (let ((window (find-UDn-window byte)))
	 (scsu-trace-output "UD~D " window)
	 (scsu-define-window state window
			   (lookup-window-offset-table (funcall read-func)))
	 (setf (scsu-state-mode state) :single-byte-mode))
       '+UD0+)
      (#.+UQU+				; Quote Unicode
       (scsu-trace-output "UQU ")
       (let ((char (decode-quote-unicode read-func)))
	 (prog1 char
	   (scsu-trace-output "[quote unicode ~X]" char))))
      (#.+UDX+				; Define Extended
       (scsu-trace-output "UDX ")
       (decode-define-window-extended state read-func)
       (setf (scsu-state-mode state) :single-byte-mode)
       '+UDX+)
      (#xF2
       (error 'scsu-error
	      :format-control "reserved byte ~A is used"
	      :format-arguments (list byte)))
      (otherwise			; MSB of Unicode BMP char.
       ;; (assert (or (<= #x00 byte #xDF) (<= #xF3 byte #xFF)))
       (let ((next (funcall read-func)))
	 (declare (type (unsigned-byte 8) next))
	 (scsu-trace-output "[Unicode char ~X,~X]" byte next)
	 (logior (ash byte 8) next))))))

(defun decode-unit* (state read-func)	; recursion point
  (ecase (scsu-state-mode state)
    (:single-byte-mode (decode-unit*/single-byte-mode state read-func))
    (:unicode-mode (decode-unit*/unicode-mode state read-func))))

(defun decode-unit (state read-func)
  (let ((code-point (decode-unit* state read-func)))
    (cond ((symbolp code-point)
	   code-point)
	  ((<= #xD800 code-point #xDBFF) ; high surrogate
	   (scsu-trace-output " ")
	   (let ((low (decode-unit* state read-func)))
	     (declare (type (unsigned-byte 16) low))
	     (unless (<= #xDC00 low #xDFFF)
	       (error 'scsu-error
		      :format-control "High surrogate appeared alone"))
	     (prog1 (decode-from-surrogate-pair code-point low)
	       (scsu-trace-output "~&"))))
	  ((<= #xDC00 code-point #xDFFF) ; low surrogate
	   (error 'scsu-error
		  :format-control "Low surrogate appeared alone"))
	  (t
	   (prog1 code-point
	     (scsu-trace-output "~%"))))))

(defun decode-to-string (bytes
			 &key (start1 0) (end1 (length bytes))
			   (string (make-array (floor end1 2) ; a (too) rough expectation.
					       :element-type 'character
					       :fill-pointer 0 :adjustable t))
			   (start2 0) (end2 (length string))
			   (state (make-instance 'scsu-state)))
  (declare (type (array (unsigned-byte 8) *) bytes)
	   (type fixnum start1 end1 start2 end2)
	   (type string string))
  (scsu-trace-output "~2&")
  (with-buffer-accessor (:reader pick-byte :current src-current)
      (bytes start1 end1 :element-type (unsigned-byte 8))
    (with-buffer-accessor (:writer put-char :current dst-current)
	(string start2 end2 :element-type character)
      (loop while (< src-current end1)
	 do (with-scsu-error-handling
		(state :src src-current :dst dst-current
		       :return (lambda (dst src)
				 (return-from decode-to-string
				   (values string dst src state))))
	      (let ((code-point (decode-unit state #'pick-byte)))
		(cond ((symbolp code-point)
		       (progn))		; state transition only
		      ((>= code-point char-code-limit) ; For UTF-16 string implementation (i.e. Allegro CL)
		       (multiple-value-bind (high low)
			   (encode-to-surrogate-pair code-point)
			 (put-char (code-char high))
			 (put-char (code-char low))))
		      (t
		       (put-char (code-char code-point)))))))
      (values string dst-current src-current state))))


;;; Encode
(deftype write-func-type ()
  '(function ((unsigned-byte 8)) t))

(defun in-window-p (offset code-point)
  (declare (type unicode-code-point offset code-point))
  (<= offset code-point (+ offset #x7f)))

(defun in-active-window-p (state code-point)
  (in-window-p (scsu-state-active-window-offset state) code-point))

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

(defun use-define-window-p (state code-point &optional next-code-point)
  (declare (type unicode-code-point code-point next-code-point))
  (and (not (scsu-state-fix-dynamic-window state))
       (not (standalone-character-p code-point))
       (alexandria:if-let ((candidates (list-offset-candidates code-point)))
	 (if next-code-point
	     (nintersection candidates (list-offset-candidates next-code-point) :test '=)
	     candidates))))

(defun update-timestamp (state &optional (window (scsu-state-active-window-index state)))
  (declare (type window-index window))
  (let ((timestamp (incf (scsu-state-current-timestamp state))))
    (declare (type fixnum timestamp))
    (setf (scsu-state-timestamp state window) timestamp)))

(defun find-LRU-dynamic-window (state)
  (loop with ret of-type fixnum = 0
     with min of-type fixnum = (scsu-state-timestamp state 0)
     for i of-type fixnum from 1 below +window-count+
     as ts of-type fixnum = (scsu-state-timestamp state i)
     when (<= ts min) ; If timestamps are same (at beginning), latter entries are used.
     do (setf ret i
	      min ts)
     finally (return ret)))

(defun encode-define-window (state offset table-index write-func define-window-tag define-extended-tag)
  (declare (type unicode-code-point offset)
	   (type (unsigned-byte 8) table-index)
 	   (type write-func-type write-func)
	   (type (unsigned-byte 8) define-window-tag define-extended-tag))
  (let ((new-window (find-LRU-dynamic-window state)))
    (declare (type window-index new-window))
    (scsu-define-window state new-window offset)
    (cond ((<= offset #xFFFF)		; 'SDn' case
	   (funcall write-func (+ define-window-tag new-window))
	   (funcall write-func table-index))
	  (t				; 'SDX' case
	   (multiple-value-bind (hbyte lbyte)
	       (encode-extended-window-tag new-window offset)
	     (funcall write-func define-extended-tag)
	     (funcall write-func hbyte)
	     (funcall write-func lbyte))))))

(defun 1byte-encodable-p (state code-point &optional (window (scsu-state-active-window-index state)))
  (declare (type (or null unicode-code-point) code-point)
	   (type window-index window))
  (or (null code-point)
      (<= code-point #x7F)
      (in-window-p (lookup-dynamic-window state window) code-point)))

(defun write-16bit-code-point (code-point write-func)
  (declare (type (unsigned-byte 16) code-point))
  (funcall write-func (ldb (byte 8 8) code-point))
  (funcall write-func (ldb (byte 8 0) code-point)))
    
(defun encode-unit*/single-byte-mode (state code-point next-code-point write-func)
  (declare (type unicode-code-point code-point)
	   (type (or null unicode-code-point) next-code-point)
	   (type write-func-type write-func))
  (cond
    ;; 1byte
    ((< code-point #x20)		
     (case code-point
       ((#x0 #x9 #xA #xD) (progn))
       (t (funcall write-func +SQ0+)))	; tags are quoted to window 0.
     (funcall write-func code-point))
    ((<= code-point #x7F)		; Basic Latin
     (funcall write-func code-point))
    ((in-active-window-p state code-point) ; in current dynamic window
     (update-timestamp state)
     (let ((offset (scsu-state-active-window-offset state)))
       (declare (type unicode-code-point offset))
       (funcall write-func (+ (- code-point offset) #x80))))
    (t
     (let ((swindow (find-suitable-static-window code-point))
	   (dwindow (find-suitable-dynamic-window state code-point)))
       (cond
	 ;; 2byte
	 ((and swindow			; in static window
	       (or (null dwindow)
		   (null next-code-point)
		   ;; If continuous in dynamic window, don't use static one.
		   (not (in-window-p (lookup-dynamic-window state dwindow) next-code-point))))
	  (locally (declare (type window-index swindow))
	    (let ((offset (lookup-static-window swindow)))
	      (declare (type unicode-code-point offset))
	      (funcall write-func (+ +SQ0+ swindow))
	      (funcall write-func (- code-point offset)))))
	 (dwindow			; in other dynamic window
	  (locally (declare (type window-index dwindow))
	    (let ((offset (lookup-dynamic-window state dwindow)))
	      (declare (type unicode-code-point offset))
	      (cond ((1byte-encodable-p state next-code-point) ; quote
		     (update-timestamp state dwindow)
		     (funcall write-func (+ +SQ0+ dwindow))
		     (funcall write-func (+ (- code-point offset) #x80)))
		    (t			; change
		     (scsu-change-to-window state dwindow)
		     (funcall write-func (+ +SC0+ dwindow))
		     (encode-unit* state code-point next-code-point write-func))))))
	 ;; 3byte or more
	 ((alexandria:when-let* ((offsets (use-define-window-p state code-point)) ; define window
				 (offset1 (first offsets)))
	    (encode-define-window state offset1 (window-offset-to-table-index offset1)
				  write-func +SD0+ +SDX+)
	    (encode-unit* state code-point next-code-point write-func)))
	 ((or (standalone-character-p code-point)
	      ;; not both suitable for unicode-mode => next char can be encoded smally (1byte).
	      (1byte-encodable-p state next-code-point)
	      (find-suitable-static-window next-code-point)
	      ;; I don't check with FIND-SUITABLE-DYNAMIC-WINDOW,
	      ;; because "SQU, <unicode>, SCn" sequence is same length
	      ;; with "SCU, <unicode>, UCn" sequence.
	      )
	  ;; quote unicode.
	  (funcall write-func +SQU+)
	  (if (> code-point #xFFFF)
	      (multiple-value-bind (high low)
		  (encode-to-surrogate-pair code-point)
		;; I don't use unicode-mode combination.
		(write-16bit-code-point high write-func)
		(funcall write-func +SQU+)
		(write-16bit-code-point low write-func))
	      (write-16bit-code-point code-point write-func)))
	 (t
	  ;; goto unicode-mode.
	  (funcall write-func +SCU+)
	  (setf (scsu-state-mode state) :unicode-mode)
	  (encode-unit* state code-point next-code-point write-func)))))))

(defun encode-unit*/unicode-mode (state code-point next-code-point write-func)
  (declare (type unicode-code-point code-point next-code-point)
	   (type write-func-type write-func))
  (cond ((<= code-point #x7F)		; Basic Latin
	 (setf (scsu-state-mode state) :single-byte-mode)
	 (funcall write-func (+ +UC0+ (scsu-state-active-window-index state)))
	 (encode-unit* state code-point next-code-point write-func))
	((alexandria:when-let* ((dwindow (find-suitable-dynamic-window state code-point)) ; in dynamic window
				(_ (1byte-encodable-p state next-code-point dwindow)))
	   (locally (declare (type window-index dwindow))
	     (setf (scsu-state-mode state) :single-byte-mode)
	     (scsu-change-to-window state dwindow)
	     (funcall write-func (+ +UC0+ dwindow))
	     (encode-unit* state code-point next-code-point write-func))))
	((alexandria:when-let* ((offsets (use-define-window-p state code-point next-code-point)) ; define window
				(offset1 (first offsets)))
	   (setf (scsu-state-mode state) :single-byte-mode)
	   (encode-define-window state offset1 (window-offset-to-table-index offset1)
				 write-func +UD0+ +UDX+)
	   (encode-unit* state code-point next-code-point write-func)))
	((> code-point #xFFFF)		; use surrogate pair
	 (multiple-value-bind (high low)
	     (encode-to-surrogate-pair code-point)
	   (write-16bit-code-point high write-func)
	   (write-16bit-code-point low write-func)))
	(t
	 (when (<= #xE000 code-point #xF2FF)
	   (funcall write-func +UQU+))
	 (write-16bit-code-point code-point write-func))))

(defun encode-unit* (state code-point next-code-point write-func) ; recursion point
  (declare (type unicode-code-point code-point next-code-point)
	   (type write-func-type write-func))
  (ecase (scsu-state-mode state)
    (:single-byte-mode
     (encode-unit*/single-byte-mode state code-point next-code-point write-func))
    (:unicode-mode
     (encode-unit*/unicode-mode state code-point next-code-point write-func))))

(defun encode-unit (state read-func write-func)
  (declare (type read-func-type read-func))
  (let* ((char (funcall read-func))
	 (next-char (funcall read-func t))
	 (code-point (char-code char))
	 (next-code-point (and next-char (char-code next-char))))
    (declare (type character char)
	     (type (or null character) next-char)
	     (type unicode-code-point code-point)
	     (type (or null unicode-code-point) next-code-point))
    (cond ((<= #xD800 code-point #xDBFF) ; high surrogate
	   (unless (and next-code-point
			(<= #xDC00 next-code-point #xDFFF))
	     (error 'scsu-error
		    :format-control "High surrogate appeared alone"))
	   (let ((u32-code-point (decode-from-surrogate-pair code-point next-code-point))
		 (low (funcall read-func))
		 (next-char2 (funcall read-func t)))
	     (declare (ignorable low)
		      (type (or null character) next-char2))
	     (assert (eql low next-char))
	     (encode-unit* state u32-code-point
			   (and next-char2 (char-code next-char2))
			   write-func)))
	  ((<= #xDC00 code-point #xDFFF) ; low surrogate
	   (error 'scsu-error
		  :format-control "Low surrogate appeared alone"))
	  (t
	   (encode-unit* state code-point next-code-point write-func)))))

(defun initialize-timestamp (state initial-priority &optional string start end)
  (etypecase (scsu-state-timestamp-vector state)
    ((array fixnum)
     (progn))				; nop
    (null
     (setf (scsu-state-timestamp-vector state)
	   (make-array +window-count+ :element-type 'fixnum :initial-element -1))))
  (cond
    ((eq initial-priority :fixed)
     (setf (scsu-state-fix-dynamic-window state) t))
    ((eq initial-priority :lookahead)
     (let ((priority-array (make-array '(8) :element-type 'fixnum :initial-element 0)))
       (declare (type (simple-array fixnum (8)) priority-array)
		(dynamic-extent priority-array))
       (loop for i of-type fixnum from start below end
	  as w = (find-suitable-dynamic-window state (char-code (char string i)))
	  when w do (incf (aref priority-array w)))
       (initialize-timestamp state priority-array)))
    ((eq initial-priority :random)
       (loop for w of-type fixnum from 0 below +window-count+
	  do (setf (scsu-state-timestamp state w) (- (random 100)))))
    ((typep initial-priority 'array)
     (let ((max (loop for i across initial-priority maximize i)))
       (loop for w of-type fixnum from 0 below +window-count+
	  do (setf (scsu-state-timestamp state w)
		   (- (aref initial-priority w) max)))))
    (t
     (error "initial-priority must be one of :lookahead, :random, or fixnum array"))))

(defun encode-from-string (string
			   &key (start1 0) (end1 (length string))
			     (bytes (make-array (length string) ; no way to expect length..
						:fill-pointer 0 :adjustable t
						:element-type '(unsigned-byte 8)))
			     (start2 0) (end2 (length bytes))
			     (initial-priority :lookahead)
			     (state (make-instance 'scsu-state)))
  (declare (type string string)
	   (type (array (unsigned-byte 8) *) bytes)	   
	   (type fixnum start1 end1 start2 end2))
  (unless (scsu-state-timestamp-vector state)
    (initialize-timestamp state initial-priority string start1 end1))
  (with-buffer-accessor (:reader pick-char :current src-current)
      (string start1 end1 :element-type character)
    (with-buffer-accessor (:writer put-byte :current dst-current)
	(bytes start2 end2 :element-type (unsigned-byte 8))
      (loop while (< src-current end1)
	 do (with-scsu-error-handling
		(state :src src-current :dst dst-current
		       :return (lambda (dst src)
				 (return-from encode-from-string
				   (values bytes dst src state))))
	      (encode-unit state #'pick-char #'put-byte)))
      (values bytes dst-current end1 state))))

(defun encode-reset-sequence (state
			      &key (bytes (make-array (* 2 +window-count+) ; SDn * 8
						      :fill-pointer 0
						      :element-type '(unsigned-byte 8)))
				(start 0) (end (length bytes)))
  (declare (type (array (unsigned-byte 8) *) bytes)	   
	   (type fixnum start end))
  (with-buffer-accessor (:writer put-byte :current dst-current)
      (bytes start end :element-type (unsigned-byte 8))
    (with-scsu-error-handling
	(state :dst dst-current
	       :return (lambda (dst src)
			 (declare (ignore src))
			 (return-from encode-reset-sequence
			   (values bytes dst state))))
      ;; change to default windows
      (loop for w of-type fixnum downfrom (1- +window-count+) to 0
	 as offset of-type unicode-code-point = (lookup-dynamic-window state w)
	 as default-offset of-type unicode-code-point
	   = (aref +default-positions-for-dynamically-positioned-windows+ w)
	 when (/= offset default-offset)
	 do (put-byte (+ w (ecase (scsu-state-mode state)
			     (:single-byte-mode +SD0+)
			     (:unicode-mode (setf (scsu-state-mode state) :single-byte-mode)
					    +UD0+))))
	   (put-byte (window-offset-to-table-index default-offset)))
      ;; changes active window and mode if required
      (cond ((eq (scsu-state-mode state) :unicode-mode)
	     (setf (scsu-state-mode state) :single-byte-mode)	     
	     (scsu-change-to-window state 0)
	     (put-byte +UC0+))
	    ((/= 0 (scsu-state-active-window-index state))
	     (scsu-change-to-window state 0)
	     (put-byte +SC0+)))
      ;; clean object
      (scsu-state-reset state))
    (values bytes dst-current state)))
