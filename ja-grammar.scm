;; ja grammar, written in PEG
(use peg)
(require "mecab-wrap")

(define ($表層形が x)
  ($do (m ($satisfy (lambda (m) (eq? (m-surface m) x))
					x))
       ($return m)))

(define ($品詞が x)
  ($do (m ($satisfy (lambda (m) (eq? (m-part m) x))
					x))
       ($return m)))

;(define ($何でもいい x)
;  ($do (m ($satisfy (lambda (m) (eq? (m-part m) x))
;					x))
;       ($return m)))

(define %名詞 ($品詞が '名詞))
(define %記号 ($品詞が '記号))
(define %形容詞 ($品詞が '形容詞))
(define %連体詞 ($品詞が '連体詞))
(define %助動詞 ($品詞が '助動詞))
(define %助詞 ($品詞が '助詞))
(define %副詞 ($品詞が '副詞))
(define %動詞 ($品詞が '動詞))
(define %接続詞 ($品詞が '接続詞))
(define %接頭詞 ($品詞が '接頭詞))
(define %句点 ($表層形が '。))
(define %読点 ($表層形が '、))

;(define %もし ($表層形が 'もし))
;(define %なら ($表層形が 'なら))

;(define %any 

(define %seg
  (let* ((%vp ($do (v   ($many %動詞 1 2))
				   (aux ($many %助動詞 0 2))
				   (p   ($many %助詞 0 2))
				   ($return (list 'vp v aux p))))
		 (%n ($do (n ($many %名詞 1))
				  ($return (list 'n n))))
         (%np ($do (n  ($many %名詞 1))
				   (p  ($many ($or %助動詞 %助詞) 0 2))
				   ($return (list 'np n p))))
         (%adj ($do (mod ($or %連体詞 %形容詞))
					(p   ($many ($or %助動詞 %助詞) 0 2))
                    ($return (list 'adj mod p))))
         (%adv ($do (mod  %副詞)
                    ($return (list 'adv mod))))
		 (%句読点 ($do
				   ($optional %句点)
				   ($optional %読点)
				   ($return '())))
		 )
	($or %np %vp %adj %adv %句読点)
	))

(define %sentence
  ($do (segs ($many %seg 1))
;	   ($optional %句点)
	   ($return (list 'sentence segs))))
  
;		 (%if ($do (optional %もし)
;				   (condition ($many-till ($or %np %vp %adj %adv) %なら)); ($or %np %vp %adj %adv %句読点) 1))
;				   (nara %なら)
;				   (consequent ($many ($or %np %vp %adj %adv %句読点) 1))
;				   ($return (list 'if condition consequent))))
		 
;         (%seg ($or %if %np %vp %adj %adv %句読点))
;		 )
	
