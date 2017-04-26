;;;; cl-bencode.lisp

(in-package #:cl-bencode)

;;; "cl-bencode" goes here. Hacks and glory await!

(defclass bencode-item ()
  ((line-number :reader bencode-line-numer :initarg :line)))

(defmethod print-object ((item bencode-item) stream)
  (format stream "~a" (bencode-content item)))

(defclass bencode-integer (bencode-item)
  ((value :reader bencode-content :initarg :content)))

(defclass bencode-string (bencode-item)
  ((text :reader bencode-content :initarg :content)))

(defclass bencode-list (bencode-item)
  ((elements :reader bencode-content :initarg :content)))

(defclass bencode-dictionary (bencode-item)
  ((dictionary :reader bencode-content :initarg :content)))

(defmethod print-object ((item bencode-dictionary) stream)
  (format stream "<")
  (loop for key being the hash-keys of (bencode-content item)
     using (hash-value value)
     do (format stream "[~a|~a]" key value))
  (format stream ">"))

(defun bencode-end-next-p (stream)
  (char= #\e
	 (peek-char nil stream)))

(defun integer-starts-next-p (stream)
  (char= #\i
	 (peek-char nil stream)))

(defun list-starts-next-p (stream)
  (char= #\l
	 (peek-char nil stream)))

(defun dictionary-starts-next-p (stream)
  (char= #\d
	 (peek-char nil stream)))

(defun digit-next-p (stream)
  (digit-char-p (peek-char nil stream)))

(defun consume-digits (stream)
  (parse-integer
   (coerce
    (loop while (digit-next-p stream)
       collect (read-char stream))
    'string)))

(defun negative-sign-next-p (stream)
  (char= #\-
	 (peek-char nil stream)))

(defun parse-bencode-integer (stream)
  (read-char stream) ; Discard the start character.
  (let ((sign-present (negative-sign-next-p stream)))
    (when sign-present
      (read-char stream)) ; Discard the sign character.
    (let ((integer-value (consume-digits stream)))
      (assert (bencode-end-next-p stream))
      (read-char stream) ; Discard the end character.
      (make-instance 'bencode-integer
		     :content (if sign-present
				  (- integer-value)
				  integer-value)))))

(defun parse-bencode-string (stream)
  (let ((length (consume-digits stream)))
    (assert (peek-char #\: stream))
    (read-char stream) ; Discard the seperator character.
    (let ((text (coerce (loop for i below length
			     collect (read-char stream))
			'string)))
      (make-instance 'bencode-string
		     :content text))))

(defun parse-bencode-list (stream)
  (read-char stream) ; Discard the start character.
  (let (elements)
    (loop until (bencode-end-next-p stream)
	 do (push (parse-iter stream) elements))
    (read-char stream) ; Discard the end character.
    (make-instance 'bencode-list
		   :content (nreverse elements))))

(defun parse-bencode-dictionary (stream)
  (read-char stream) ; Discard the start character
  (let ((dictionary (make-hash-table :test #'equal)))
    (loop until (bencode-end-next-p stream)
       do (setf (gethash (bencode-content (parse-bencode-string stream)) dictionary)
		(parse-iter stream)))
    (read-char stream) ; Discard the end character
    (make-instance 'bencode-dictionary
		   :content dictionary)))

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
  (parse-iter stream))
