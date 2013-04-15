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

(in-package :cl-base32)

;;
;; Base32 encoding - http://tools.ietf.org/html/rfc4648
;;

(defvar *base32-alphabet* "abcdefghijklmnopqrstuvwxyz234567")

(defun encode-word (a-word)
  "Return the digit in the base32 alphabet corresponding to a word"
  (char *base32-alphabet* a-word))

(defun decode-word (a-digit)
  "Return the word encoded as a digit in the base32 alphabet"
  (let ((code (char-code a-digit)))
    (or (and (char<= #\a a-digit #\z)
             (- code (char-code #\a)))
        (and (char<= #\2 a-digit #\7)
             (+ 26 (- code (char-code #\2))))
        ;; upper case
        (and (char<= #\A a-digit #\Z)
             (- code (char-code #\A))))))

(defun read-word (some-bytes word-index)
  "Return the word (a 5-bit integer) found in some-bytes located at word-index"
  (let* ((bytes-length (length some-bytes))
	 ;; don't be confused : bit indexes aren't really pointing to
	 ;; the bit as understood by Lisp--they are more virtual in nature,
	 ;; which assumes that bit 0 is the MSB rather than bit 8 being MSB
         (start-bit-index (* 5 word-index) )
         (end-bit-index (+ 4 start-bit-index) )
         (part1-byte-index (floor start-bit-index 8)) 
         (part2-byte-index (floor end-bit-index 8))
         (part1-size (min 5 (- 8 (mod start-bit-index 8))))
         (part2-size (- 5 part1-size))
	 ;; here we translate the bit indexes so that the MSB is bit 8
	 ;; and the LSB is bit 0
         (source-part1 (byte part1-size 
                             (- (- 8 (mod start-bit-index 8)) part1-size)
                             )
	   )
         (source-part2 (byte part2-size 
                             (- (- 8 (mod end-bit-index 8)) 1) )
	   )
	 ;; becomes the upper bits in value
         (dest-part1 (byte part1-size part2-size)) 
	 ;; becomes the lower bits in value
         (dest-part2 (byte part2-size 0)) 
         (value 0))
    
    (setf (ldb dest-part1 value)
          (ldb source-part1 (aref some-bytes part1-byte-index)))
    (if (< part1-byte-index bytes-length)
        (if (> part2-byte-index part1-byte-index)
            (if (< part2-byte-index (length some-bytes))
                (setf (ldb dest-part2 value)
                      (ldb source-part2 (aref some-bytes part2-byte-index)))
                (setf (ldb dest-part2 value) 0 )))
        (setq value 0))
    value))

(defun write-word (some-bytes word-index word)
  "Write the word into the bits located at word-index in some-bytes"
  (let* ( 
         (bytes-length (length some-bytes))
         ;; don't be confused : bit indexes aren't really pointing to
         ;; the bit as understood by Lisp--they are more virtual in nature,
         ;; which assumes that bit 0 is the MSB rather than bit 8 being MSB
         (start-bit-index (* 5 word-index) )
         (end-bit-index (+ 4 start-bit-index) )
         (part1-byte-index (floor start-bit-index 8)) 
         (part2-byte-index (floor end-bit-index 8))
         (part1-size (min 5 (- 8 (mod start-bit-index 8))))
         (part2-size (- 5 part1-size))
	 ;; here we translate the bit indexes so that the MSB is bit 8
	 ;; and the LSB is bit 0
         (dest-part1 (byte part1-size 
                           (- (- 8 (mod start-bit-index 8)) part1-size)))
         (dest-part2 (byte part2-size 
                           (- (- 8 (mod end-bit-index 8)) 1) ))
	 ;; becomes the upper bits in value
         (source-part1 (byte part1-size part2-size)) 
	 ;; becomes the lower bits in value
         (source-part2 (byte part2-size 0))   
         (part1-byte (aref some-bytes part1-byte-index))
         (part2-byte (if (and (< part2-byte-index bytes-length)
                              (> part2-size 0)) 
                         (aref some-bytes part2-byte-index))))
    (setf (ldb dest-part1 part1-byte)
          (ldb source-part1 word))
    (if part2-byte
        (setf (ldb dest-part2 part2-byte)
              (ldb source-part2 word)))    
    (setf (aref some-bytes part1-byte-index) part1-byte)
    (if part2-byte
        (setf (aref some-bytes part2-byte-index) part2-byte))))

(defun unpadded-base32-length (base32-string)
  "Given a base32 string, compute the size of the raw base32 string,
   without any = padding
  "
  (let* ((padded-length (length base32-string))
         (unpadded-length padded-length))
    (dotimes (i padded-length)
        (if (eql #\= (aref base32-string (- padded-length i)))
            (decf unpadded-length)
            (return unpadded-length)))))

(defun byte-length-from-base32 (base32-string)
  "Given a base32 string, compute the number of bytes in the 
   decoded data
  "
  (let* ((padded-length (length base32-string))
         (unpadded-length padded-length)
         (padding 0)
         (block-count (ceiling padded-length 8)))
    (if (<= padded-length 0)
        0
        (progn
          (dotimes (i padded-length)
            (if (eql #\= (aref base32-string (- padded-length i 1)))
                (progn
                  (decf unpadded-length)
                  (incf padding))))
          (- (* 5 block-count)
             (ecase padding
	       (0 0)
	       (6 4)
	       (4 3)
	       (3 2)
	       (1 1)))))))

(defun base32-length-from-bytes (some-bytes)
  "Given bytes of unencoded data, determine the length of the
   corresponding base32-encoded string
  "
  (let* ((word-count (ceiling (* 8 (length some-bytes)) 5) )
         (digit-count (* 8 (ceiling word-count 8))))
    (values digit-count word-count)))

(defun bytes-to-base32 (some-bytes)
  "Return a base32 string encoding of the provided vector of bytes"
  (let* ((word-count (ceiling (* 8 (length some-bytes)) 5) )
         (digit-count (* 8 (ceiling word-count 8)))
         (base32-string (make-string digit-count :initial-element #\=)))
    (dotimes (i word-count)
      (setf (aref base32-string i)
            (encode-word (read-word some-bytes i))))
    base32-string))

(defun base32-to-bytes (base32-string)
  "Return the bytes decoded from the supplied base32 string"
  (let* ((byte-count (byte-length-from-base32 base32-string) )
         (base32-bytes (make-array `(,byte-count) 
                                   :element-type '(unsigned-byte 8) 
                                   :initial-element 0)))
    (dotimes (i (length base32-string))
      (let ((word (decode-word (aref base32-string i))))
        (if word
            (write-word base32-bytes i word)
            (return nil))))
    base32-bytes))
