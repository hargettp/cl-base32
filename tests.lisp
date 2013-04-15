;; Copyright (c) 2011 Phil Hargett

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.

(defpackage :cl-base32-tests
  (:use :cl :lisp-unit :cl-base32))

(in-package :cl-base32-tests)

;; start fresh--remove any tests that no longer exist
(remove-tests :all)

(define-test basic-encoding-decoding
  (assert-equalp #(1 2 3)
		(base32-to-bytes (bytes-to-base32 #(1 2 3)))))

(define-test check-padded-length
  (assert-equal 8
		(length (bytes-to-base32 #(1 2 3))))
  (assert-equal 8
		(length (bytes-to-base32 #(1 2 3 4 5))))
  (assert-equal 16
		(length (bytes-to-base32 #(1 2 3 4 5 16)))))

(define-test upcased-encoding-decoding
  (assert-equalp #(1 2 3)
		(base32-to-bytes (string-upcase (bytes-to-base32 #(1 2 3))))))

(run-tests :all :cl-base32-tests)
