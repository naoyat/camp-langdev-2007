(use peg)
(require "mecab-wrap")

(define (list->stream lst)
  (let loop ((l lst) (pos 0))
	(lambda ()
	  (if (null? l)
		  (let loop () (values #f pos loop))
		  (values (car l)
				  pos
				  (loop (cdr l) (+ pos 1)))))))

(define (parse-stream parse stream)
  (define (error->string err)
    (case (failure-type err)
      ((message)  (failure-message err))
      ((expect)   (failure-message err))
      ((unexpect) (format #f "unexpected: ~a" (failure-message err)))))
  (let1 r (parse stream)
    (if (parse-success? r)
      (semantic-value-finalize! (result-value r))
      (raise (make-condition <parse-error>
               'position (failure-position r)
               'message (error->string r))))))

