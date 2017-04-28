;;;; package.lisp

(defpackage #:cl-bencode
  (:use #:cl)
  (:import-from :flexi-streams
		:peek-byte
		:read-byte)
  (:export :parse))

