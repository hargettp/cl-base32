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

(defsystem #:cl-base32
  :name "cl-base32"
  :version "0.1"
  :serial t
  :components ((:file "package")
	       (:file "base32"))
  :in-order-to ((test-op (test-op #:cl-base32/tests))))

(defsystem #:cl-base32/tests
  :name "cl-base32/tests"
  :version "0.1"
  :serial t
  :components ((:file "tests"))
  :depends-on (;; external packages
	       #:lisp-unit
	       ;; project packages
               #:cl-base32)
  :perform (test-op (op c)
                    (symbol-call :lisp-unit '#:run-tests :all :cl-base32-tests)))
