(use mecab)

(define *mecab-instance* (mecab-new2 "--node-format=(m\\s%m(%F\\s[0,1,2,3])(%F\\s[4,5])%f[6](%F\\s[7,8])) --bos-format=( --eos-format=)"))

; １段目のパースで得たリストを、コンビネータパーサが扱えるストリームに変換
(define (mecab str)
  (read-from-string (mecab-sparse-tostr *mecab-instance* str)))
;		(list->stream morpheme-list)))

(define (m? m) (and (pair? m) (eq? 'm (car m))))
(define (m-surface m) (cadr m))
(define (m-parts m) (caddr m))
(define (m-conjugs m) (cadddr m))
(define (m-original m) (car (cddddr m)))
(define (m-yomis m) (cadr (cddddr m)))

(define (m-surface-str m) (if (m? m) (symbol->string (m-surface m)) ""))

(define (m-pronounce m) (cadr (m-yomis m)))

(define (m-part m)
  (car (m-parts m)))
(define (m-part2 m)
  (cadr (m-parts m)))
;(define (m-conjug-paradigm m)
;  (let1 cs (m-conjugs m)
;		(if (null? cs) '* (cadr cs))))
(define (m-conjug m)
  (let1 cs (m-conjugs m)
		(if (null? cs) '* (cadr cs))))

;;;
(define (m-surface-eq? m surface) (eq? (m-surface m) surface))
(define (m-surface-check-proc surface) (lambda (m) (eq? (m-surface m) surface)))
(define (m-part-eq? m part) (eq? (m-part m) part))
(define (m-part-check-proc part) (lambda (m) (eq? (m-part m) part)))
(define (m-parts-check-proc parts) (lambda (m) (equal? (m-parts m) parts)))

(define (名詞? m) (m-part-eq? m '名詞))
(define (形容詞? m) (m-part-eq? m '形容詞))
(define (連体詞? m) (m-part-eq? m '連体詞))
(define (助動詞? m) (m-part-eq? m '助動詞))
(define (助詞? m) (m-part-eq? m '助詞))
(define (副詞? m) (m-part-eq? m '副詞))
(define (動詞? m) (m-part-eq? m '動詞))
(define (接続詞? m) (m-part-eq? m '接続詞))
(define (接頭詞? m) (m-part-eq? m '接頭詞))
(define (感動詞? m) (m-part-eq? m '感動詞))
(define (句点? m) (m-surface-eq? m '。))
(define (読点? m) (m-surface-eq? m '、))

(define (m-conditional? m) (eq? '仮定形 (m-conjug m)))

