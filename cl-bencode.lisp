;;;; cl-bencode.lisp

(in-package #:cl-bencode)

;;; "cl-bencode" goes here. Hacks and glory await!

(defun bencode-end-next-p (stream)
  (= (char-code #\e)
     (peek-byte stream nil)))

(defun integer-starts-next-p (stream)
  (= (char-code #\i)
     (peek-byte stream nil)))

(defun list-starts-next-p (stream)
  (= (char-code #\l)
     (peek-byte stream nil)))

(defun dictionary-starts-next-p (stream)
  (= (char-code #\d)
     (peek-byte stream nil)))

(defun digit-next-p (stream)
  (let ((digits (map 'list #'char-code (list #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\0))))
    (member (peek-byte stream nil) digits)))

(defun consume-digits (stream)
  (parse-integer
   (coerce
    (loop while (digit-next-p stream)
       collect (code-char (read-byte stream)))
    'string)))

(defun negative-sign-next-p (stream)
  (= (char-code #\-)
     (peek-byte stream nil)))

(defun parse-bencode-integer (stream)
  (read-byte stream) ; Discard the start character.
  (let ((sign-present (negative-sign-next-p stream)))
    (when sign-present
      (read-byte stream)) ; Discard the sign character.
    (let ((integer-value (consume-digits stream)))
      (assert (bencode-end-next-p stream))
      (read-byte stream) ; Discard the end character.
      (if sign-present
	  (- integer-value)
	  integer-value))))

(defun parse-bencode-string (stream)
  (let ((length (consume-digits stream)))
    (assert (peek-byte stream (char-code #\:)))
    (read-byte stream) ; Discard the seperator character.
    (let ((text (coerce (loop for i below length
			   collect (code-char (read-byte stream)))
			'string)))
      text)))

(defun parse-bencode-list (stream)
  (read-byte stream) ; Discard the start character.
  (let (elements)
    (loop until (bencode-end-next-p stream)
	 do (push (parse-iter stream) elements))
    (read-byte stream) ; Discard the end character.
    (nreverse elements)))

(defun parse-bencode-dictionary (stream)
  (read-byte stream) ; Discard the start character
  (let ((dictionary (make-hash-table :test #'equal)))
    (loop until (bencode-end-next-p stream)
       do (setf (gethash (parse-bencode-string stream) dictionary)
		(parse-iter stream)))
    (read-byte stream) ; Discard the end character
    dictionary))

(defun parse-iter (stream)
  (cond ((integer-starts-next-p stream)
	 (parse-bencode-integer stream))
	((digit-next-p stream)
	 (parse-bencode-string stream))
	((list-starts-next-p stream)
	 (parse-bencode-list stream))
	((dictionary-starts-next-p stream)
	 (parse-bencode-dictionary stream))))

(defun parse (stream)
  (parse-iter (flexi-streams:make-flexi-stream stream :external-format :latin-1 :element-type 'flexi-streams:octet)))
