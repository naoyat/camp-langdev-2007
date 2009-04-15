;; ja grammar, written in PEG
(use peg)
;(use srfi-34)
(require "mecab-wrap")
(require "peg-mecab")

(define (symbol-join symbols)
  (string->symbol (string-join (map symbol->string symbols) "")))
(define (symbol-append . symbols)
  (symbol-join symbols))

(define (zennum->integer zn)
  (case zn
	((０) 0)
	((１) 1)
	((２) 2)
	((３) 3)
	((４) 4)
	((５) 5)
	((６) 6)
	((７) 7)
	((８) 8)
	((９) 9)
	))

(define ($表層形が surface)
  ($do (m ($satisfy (m-surface-check-proc surface) surface))
       ($return m)))

(define ($品詞が part)
  ($do (m ($satisfy (m-part-check-proc part) part))
       ($return m)))

(define ($品詞詳細が parts)
  ($do (m ($satisfy (m-parts-check-proc parts) parts))
       ($return m)))

(define ($名詞で part2)
  (let1 parts (list '名詞 part2)
		($do (m ($satisfy (m-parts-check-proc parts) parts))
			 ($return m) )))
(define ($名詞だが非 . x)
  ($do (m ($satisfy (lambda (m) (and (eq? (m-part m) '名詞)
									 (not (memq (m-part2 m) x))))
					x))
       ($return m)))

(define ($動詞で原形が x)
  ($do (m ($satisfy (lambda (m) (and (eq? (m-part m) '動詞)
									 (eq? (m-original m) x)))
					x))
       ($return m)))

(define ($比較演算子)
  ($do (m ($satisfy (lambda (m) (case (m-surface m)
								  ((＝ ＞ ≧ ＜ ≦ ≠ ≒) #t)
								  (else #f))) 'cmp))
	   ($return m)))

(define ($何でもいい x)
  ($do (m ($satisfy (lambda (m) (m? m)) x))
       ($return m)))

(define %名詞 ($品詞が '名詞))
(define %形容詞 ($品詞が '形容詞))
(define %連体詞 ($品詞が '連体詞))
(define %助動詞 ($品詞が '助動詞))
(define %助詞 ($品詞が '助詞))
(define %副詞 ($品詞が '副詞))
(define %動詞 ($品詞が '動詞))
(define %接続詞 ($品詞が '接続詞))
(define %接頭詞 ($品詞が '接頭詞))
(define %感動詞 ($品詞が '感動詞))
(define %句点 ($表層形が '。))
(define %読点 ($表層形が '、))

(define %引用開 ($表層形が '「))
(define %引用閉 ($表層形が '」))
(define %括弧開 ($表層形が '（))
(define %括弧閉 ($表層形が '）))

(define %助動詞-nocond
  ($do (m ($satisfy (lambda (m) (and (助動詞? m) (not (m-conditional? m)))) 'nocond))
       ($return m)))
(define %助動詞-cond
  ($do (m ($satisfy (lambda (m) (and (助動詞? m) (m-conditional? m))) 'cond))
       ($return m)))
(define %形容詞-nocond
  ($do (m ($satisfy (lambda (m) (and (形容詞? m) (not (m-conditional? m)))) 'nocond))
       ($return m)))
(define %形容詞-cond
  ($do (m ($satisfy (lambda (m) (and (形容詞? m) (m-conditional? m))) 'cond))
       ($return m)))

(define %比較演算子; ($比較演算子))
  ($or ($do (($表層形が '＝)) ($return '==))
	   ($do (($表層形が '＞)) ($return '>))
	   ($do (($表層形が '≧)) ($return '>=))
	   ($do (($表層形が '＜)) ($return '<))
	   ($do (($表層形が '≦)) ($return '<=))
	   ($do (($表層形が '≠)) ($return '<>))
	   ($do (($表層形が '≒)) ($return '~~)) ))
(define %λ ($表層形が 'λ))
(define %： ($表層形が '：))

(define %接尾助数詞 ($品詞詳細が '(名詞 接尾 助数詞)))

(define %記号 ($品詞が '記号))
;(define %引用閉以外 ($表層形が非 '」))

(define %何でもいい ($何でもいい 'any))

(define %数 ($名詞で '数))
(define %非数名詞 ($名詞だが非 '数))
(define %非数非サ変名詞 ($名詞だが非 '数 'サ変接続))

(define %サ変名詞 ($名詞で 'サ変接続))
(define %非サ変名詞 ($名詞だが非 'サ変接続))
(define %する ($動詞で原形が 'する))

(define (make-サ変動詞 n v)
  (list 'm
		(symbol-append (m-surface n) (m-surface v))
		(m-parts v)
		(m-conjugs v)
		(symbol-append (m-surface n) (m-original v))
		(map symbol-append (m-yomis n) (m-yomis v))
		))

(define %sentence
  (let* (
		 (%label ($do (_ %：)
					  (name ($many-till %何でもいい %：))
					  (_ %：)
					  ($return (cons 'label name))))

		 (%lambda ($do (_ %λ)
					   (def ($many ($or %名詞 %記号) 1))
					   (p   ($many %助詞 1 2))
					   ($return (list 'lambda def p))))

		 (%num ($do (num ($many %数 1))
					($return (list 'num
								   (fold (lambda (x y) (+ (* y 10) x))
										 0
										 (map zennum->integer (map m-surface num)))
								   ))))
		 (%times ($do (num %num)
					  (suffix %接尾助数詞)
					  ($return (list 'times num suffix))))

		 (%ns ($do (ns ($many %非数非サ変名詞 1))
				   ($return (list 'ns ns))))

		 (%サ変動詞 ($do (n %サ変名詞)
						 (v %する)
						 ($return (make-サ変動詞 n v))))
		 (%vp ($do (v   ($or %動詞 %サ変動詞))
				   (aux ($many %助動詞 0 2))
				   (p   ($many %助詞 0 2))
				   ($return (list 'vp v aux p))))

;		 ($quoted ($between %引用開 %引用閉 %何でもいい))
		 (%quoted ($do (_ %引用開)
					   (content ($many-till %何でもいい %引用閉))
					   (_ %引用閉)
					   ($return (list 'quoted content))))
;		 ($paren ($between %括弧開 %括弧閉 %何でもいい))
		 (%paren ($do (_ %括弧開)
					  (content ($many-till %何でもいい %括弧閉))
					  (_ %括弧閉)
					  ($return (list 'paren content))))

		 (%n類 ($do (n_ ($or %times %ns %num %quoted %paren %lambda))
					($return n_)))

		 (%cmp ($do (left %n類)
					(cmp %比較演算子)
					(right %n類)
					($return (list 'cmp cmp left right))))

		 (%cmpp ($do (cmp %cmp)
					 (p ($many %助詞 1 2))
					 ($return (list 'cmpp p cmp))))
         (%pred ($do (n   ($or %cmp %n類))
					 (aux ($do (aux1 %助動詞-nocond)
							   (aux2 ($optional %助動詞))
							   ($return (cons aux1 aux2))))
					 (p   ($many %助詞 0 2))
					 ($return (list 'pred p aux n))))
         (%pred-cond ($do (n   ($or %cmp %n類))
						  (aux ($do (aux1 %助動詞-cond)
									(aux2 ($optional %助動詞))
									($return (cons aux1 aux2))))
;						  (aux ($do %助動詞-cond ($optional %助動詞)))
						  (p   ($many %助詞 0 2))
						  ($return (list 'pred-cond p aux n))))

         (%adj ($do (mod ($or %連体詞 %形容詞-nocond))
					(p   ($many ($or %助動詞 %助詞) 0 2))
                    ($return (list 'adj mod p))))
         (%adj-cond ($or ($do (mod %形容詞-cond)
							  (p   ($many ($or %助動詞 %助詞) 0 2))
							  ($return (list 'adj-cond mod p)))
						 ($do (mod ($or %連体詞 %形容詞-nocond))
							  (p   ($many ($or %助動詞-cond %助詞) 0 2))
							  ($return (list 'adj-cond mod p)))
						 ))
         (%adv ($do (mod  %副詞)
                    ($return (list 'adv mod))))

		 (%comma ($do %読点
					  ($return '(comma))))
		 (%period ($do %句点
					   ($return '(period))))
		 (%sign ($do (k ($many %記号 1))
					 ($return (list 'sign k))))

         (%np ($do (n %n類)
				   (p ($many %助詞 0 2))
				   ($return (list 'np p n))))
		 (%nps ($do (nps ($many %np 1))
					($return nps)))
;		 (%npvp ($do (nps %nps)
;					 (vp %vp)
;					 ($return (list 'npvp vp nps))))
		 (%nppred ($do (nps %nps)
					   (pred ($or %pred %vp))
					   ($return (list 'nppred pred nps))))
		 (%nppred-cond ($do (nps %nps)
							(pred-cond %pred-cond) ; or %vp-cond
							($return (list 'nppred-cond pred-cond nps))))
		 
		 (%segs ($or %times ;%npvp
					 %pred %pred-cond %nppred %nppred-cond %cmpp %cmp %label
					 %vp
					 %adj %adj-cond %adv))
		 )
	($do (segs ($many %segs 1))
		 (period ($optional %period))
		 ($return (cons 'sentence segs)))
	))

(define (parse-sentence ss) 
  (guard (e ((<parse-error> e)
			 (list (ref e 'position)
				   (ref e 'message)))
;										;(else e))
		  (else (print 'ERROR) #f))
		 (parse-stream %sentence (list->stream ss))
		 (error (list "parse-sentence failed" e))))

(define (parse-sentence ss) (parse-stream %sentence (list->stream ss)))
