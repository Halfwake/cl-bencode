;;;; package.lisp

(defpackage #:cl-bencode
  (:use #:cl)
  (:import-from :flexi-streams
		:peek-byte
		:read-byte)
  (:export :parse))

(defpackage #:cl-bencode-tests
  (:use #:cl #:cl-bencode #:lisp-unit))
