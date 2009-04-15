;;;
;;; json.scm - JSON (RFC4627) Parser
;;;
;;;   Copyright (c) 2006 Rui Ueyama (rui314@gmail.com)
;;;
;;;   Redistribution and use in source and binary forms, with or without
;;;   modification, are permitted provided that the following conditions
;;;   are met:
;;;
;;;   1. Redistributions of source code must retain the above copyright
;;;      notice, this list of conditions and the following disclaimer.
;;;
;;;   2. Redistributions in binary form must reproduce the above copyright
;;;      notice, this list of conditions and the following disclaimer in the
;;;      documentation and/or other materials provided with the distribution.
;;;
;;;   3. Neither the name of the authors nor the names of its contributors
;;;      may be used to endorse or promote products derived from this
;;;      software without specific prior written permission.
;;;
;;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;

;;; http://www.ietf.org/rfc/rfc4627.txt

(define-module json
  (use peg)
  (use srfi-13)
  (use srfi-14)
  (use srfi-43)
  (export parse-json
          ->json))
(select-module json)

;;;============================================================
;;; Parser
;;;
(define %ws ($many ($one-of #[ \t\r\n])))

(define %begin-array ($seq %ws ($char #\[) %ws))
(define %begin-object ($seq %ws ($char #\{) %ws))
(define %end-array ($seq %ws ($char #\]) %ws))
(define %end-object ($seq %ws ($char #\}) %ws))
(define %name-separator ($seq %ws ($char #\:) %ws))
(define %value-separator ($seq %ws ($char #\,) %ws))

(define %false ($do (($string "false")) ($return 'false)))
(define %null  ($do (($string "null"))  ($return 'null)))
(define %true  ($do (($string "true"))  ($return 'true)))

(define %object
  (let1 %member ($do (k %string)
                     %name-separator
                     (v %value)
                     ($return (cons k v)))
    ($between %begin-object
              ($sep-by %member %value-separator)
              %end-object)))

(define %array
  ($do %begin-array
       (lis ($sep-by %value %value-separator))
       %end-array
       ($return (list->vector (semantic-value-finalize! lis)))))

(define %number
  (let* ((%sign ($or ($do (($char #\-)) ($return -1))
                     ($do (($char #\+)) ($return 1))
                     ($return 1)))
         (%digits ($do (d ($many digit 1))
                       ($return (string->number (apply string d)))))
         (%int %digits)
         (%frac ($do (($char #\.))
                     (d ($many digit 1))
                     ($return (string->number (apply string #\0 #\. d)))))
         (%exp ($do (($one-of #[eE])) (s %sign) (d %digits)
                    ($return (* s d)))))
    ($do (sign %sign)
         (int %int)
         (frac ($or %frac ($return 0)))
         (exp ($or %exp ($return 0)))
         ($return (* sign (+ int frac) (expt 10 exp))))))

(define %string
  (let* ((%dquote ($char #\"))
         (%escape ($char #\\))
         (%hex4 ($do (s ($many hexdigit 4 4))
                     ($return (string->number (apply string s) 16))))
         (%special-char
          ($do %escape
               ($or ($do ($char #\") ($return #\"))
                    ($do ($char #\\) ($return #\\))
                    ($do ($char #\/) ($return #\/))
                    ($do ($char #\b) ($return #\b))
                    ($do ($char #\f) ($return #\f))
                    ($do ($char #\n) ($return #\n))
                    ($do ($char #\r) ($return #\r))
                    ($do ($char #\t) ($return #\t))
                    ($do ($char #\u) (c %hex4) ($return (ucs->char c))))))
         (%unescaped ($none-of #[\"]))
         (%body-char ($or %special-char %unescaped))
         (%string-body ($->rope ($many %body-char))))
    ($between %dquote %string-body %dquote)))

(define %value ($or %false %null %true %object %array %number %string))

(define %json-text ($or %object %array))

;; entry point
(define (parse-json str)
  (parse-string %json-text str))

;;;============================================================
;;; Writer
;;;

(define (print-value obj)
  (cond ((eq? obj 'false) (display "false"))
        ((eq? obj 'null)  (display "null"))
        ((eq? obj 'true)  (display "true"))
        ((pair? obj)      (print-object obj))
        ((vector? obj)    (print-array obj))
        ((number? obj)    (print-number obj))
        ((string? obj)    (print-string obj))
        (else (error "->json expects list or vector, but got:" obj))))

(define (print-object obj)
  (display "{")
  (fold (lambda (attr comma)
          (display comma)
          (print-string (car attr))
          (display ":")
          (print-value (cdr attr))
          ",")
        "" obj)
  (display "}"))

(define (print-array obj)
  (display "[")
  (vector-for-each (lambda (i val)
                     (unless (zero? i) (display ","))
                     (print-value val))
                   obj)
  (display "]"))

(define (print-number num)
  ;;; 無限大は=で比較できる。NaN同士の=による比較は#fが返る
  (unless (and (zero? (imag-part num))
               (not (= num 1/0))
               (not (= num -1/0))
               (= num num))
    (error "rational number expected, but got" num))
  (write num))

(define (print-string str)
  (define (print-char c)
    (if (char-set-contains? char-set:ascii c)
      (write-char c)
      (format #t "\\u~4,0x" (char->ucs c))))
  (display "\"")
  (string-for-each print-char str)
  (display "\""))

(define (->json x)
  (with-output-to-string
   (lambda ()
     (cond ((pair? x)   (print-object x))
           ((vector? x) (print-array x))
           (else (error "->json expects list or vector, but got" x))))))

(provide "json")
