(require "mecab-wrap")

(define (symbol-join symbols)
  (string->symbol (string-join (map symbol->string symbols) "")))
(define (symbol-append . symbols)
  (symbol-join symbols))

(define (get-operator v-orig)
  (case v-orig
	((加える) 'add)
	((引く) 'sub)
	((代入する) 'assign)
	((言う) 'say)
	((話す 叫ぶ 吠える 鳴く) 'talk)
	((鳴らす) 'ring)
	((する) 'let)
	((入れる) 'power-on)
	((切る) 'power-off)
	((戻る) 'goto)
	(else v-orig)
	))

(define (seg-debugprint seg)
  (case (car seg)
	((m) (m-surface-str seg))

	((npvp) (let ((vp (cadr seg))
				  (nps (caddr seg)))
			  (format "(apply ~a (~a))"
					  (seg-debugprint vp)
					  (string-join (map seg-debugprint nps))
					  )))
	((nppred) (let ((pred (cadr seg))
					(nps (caddr seg)))
				(format "(~a ~a)"
						(seg-debugprint pred)
						(string-join (map seg-debugprint nps))
						)))
	((pred cmppred) (let ((p (cadr seg))
						  (aux (caddr seg))
						  (n-or-cmp (cadddr seg)))
					  (if (and (not (null? aux))
							   (eq? '仮定形 (m-conjug (car aux))))
						  (format "(if ~a ...)"
								  (seg-debugprint n-or-cmp))
						  (format "(~a.~a ~a)"
								  (string-join (map m-surface-str aux))
								  (string-join (map m-surface-str p))
								  (seg-debugprint n-or-cmp))
						  )))
;	((cmppred) (let ((p (cadr seg))
;					 (aux (caddr seg))
;					 (cmp (cadddr seg)))
;				 (format "(~a.~a ~a)"
;						 (string-join (map m-surface-str aux))
;						 (string-join (map m-surface-str p))
;						 (seg-debugprint cmp))
;				 ))
	((cmpp) (let ((p (cadr seg))
				  (cmp (caddr seg)))
			  (format "(~a ~a)"
					  (string-join (map seg-debugprint p))
					  (seg-debugprint cmp))
			  ))
	
	((quoted) (let1 content (cadr seg)
					(format "(quoted \"~a\" ~a)"
							(string-join (map m-surface-str content))
							(symbol-join (map m-pronounce content))
							)))
	((paren) (format "(list ~a)"
					 (string-join (map m-surface-str (cadr seg))) ))

	((lambda) (format "(lambda ~a.~a)"
					  (string-join (map m-surface-str (cadr seg)))
					  (if (not (null? (cddr seg)))
						 (string-join (map m-surface-str (caddr seg))) "") ))

	((adj) (let ((mod (cadr seg))
				 (p (caddr seg)))
			 (if (eq? '仮定形 (m-conjug mod))
				 (format "(if (~a?) ...)" (m-original mod))
				 (format "(~a.~a)"
						 (m-surface-str (cadr seg))
						 (string-join (map m-surface-str (caddr seg)))
						 ))))

	((times) (format "(<times>~a ~a)"
					 (seg-debugprint (caddr seg)) ;num
					 (seg-debugprint (cadr seg)) )) ;num
	((label) (format "(label ~a)"
					 (string-join (map m-surface-str (cdr seg)))))
	((adv) (format "(~a)"
				   (seg-debugprint (cadr seg)))) ;num
;				   (m-surface-str (cadr seg))))
	((cmp) (let ((op (cadr seg))
				 (lhs (caddr seg))
				 (rhs (cadddr seg)))
			 (format "(~a *~a *~a)"
					 op
					 (seg-debugprint lhs)
					 (seg-debugprint rhs)
					 )))
	((num) (format "#~a" (cadr seg) ))
	((ns) (let1 ns (cadr seg)
				(format "~a"
						(string-join (map m-surface-str ns))
						)))
	((np) (let ((p (cadr seg))
				(n (caddr seg)))
			(format "{~a ~a}"
					(string-join (map m-surface-str p))
					(seg-debugprint n)
					)))
	((vp) (let ((v (cadr seg))
				(aux (caddr seg))
				(p (cadddr seg)))
;			(m-surface-str v)
;			(string-join (map m-surface-str aux))
;			(string-join (map m-surface-str p))
			(format "~a" (get-operator (m-original v)))
			))
	(else (format "<~a>" (car seg))))
  )

(define (sentence-debugprint sentence)
  (string-join (map seg-debugprint (cdr sentence)) " : "))
