(require "mecab-wrap")
;(require "peg-mecab")
(require "parser")
(require "printer")

;; REPL
(define *input-prompt* "INPUT> ")
(define *output-prompt* "OUTPUT> ")
(define *break-message* "BREAK.")

(define (repl)
  (display *input-prompt*)
  (flush)
  (let ((line (read-line)))
	(cond ((eof-object? line) 
		   (print *break-message*)
		   'break)
		  ((string=? "quit" line)
		   (print *break-message*)
		   'break)
		  (else
;		   (let1 sentence (parse-sentence line)
;				 (display *output-prompt*)
;				 (print-sentence sentence)
		   (let* ((segs (parse-segs (mecab line)))
				  (_ (print "> " (segs-surface segs)))
				  (sentence (parse-sentence (cdr segs)))
				  (_ (map print (cadr sentence))))
										;				 (print (map picker mecab-result)))
			 (repl) ))
		  )))

;; REPL
(repl)

;; Destroy
;; - MeCab
(mecab-destroy *mecab-instance*)
