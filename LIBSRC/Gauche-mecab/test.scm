;;
;; test for mecab module
;;

(use gauche.test)

(test-start "mecab")
(use mecab)
(test-module 'mecab)

(define m (mecab-new2 ""))
(test* "mecab-new2" #t (is-a? m <mecab>))
(test* "mecab-destroy" #f (mecab-destroyed? m))
(mecab-destroy m)
(test* "mecab-destroy" #t (mecab-destroyed? m))

(test* "mecab-sparse-tostr" #f
       (mecab-sparse-tostr m "太郎は次郎が持っている本を花子に渡した。"))
(test* "mecab-strerror" #t (string? (mecab-strerror m)))

(define m (mecab-new2 ""))
(test* "mecab-sparse-tostr"
       "太郎\t名詞,固有名詞,人名,名,*,*,太郎,タロウ,タロー\nは\t助詞,係助詞,*,*,*,*,は,ハ,ワ\n次郎\t名詞,固有名詞,人名,名,*,*,次郎,ジロウ,ジロー\nが\t助詞,格助詞,一般,*,*,*,が,ガ,ガ\n持っ\t動詞,自立,*,*,五段・タ行,連用タ接続,持つ,モッ,モッ\nて\t助詞,接続助詞,*,*,*,*,て,テ,テ\nいる\t動詞,非自立,*,*,一段,基本形,いる,イル,イル\n本\t名詞,一般,*,*,*,*,本,ホン,ホン\nを\t助詞,格助詞,一般,*,*,*,を,ヲ,ヲ\n花子\t名詞,固有名詞,人名,名,*,*,花子,ハナコ,ハナコ\nに\t助詞,格助詞,一般,*,*,*,に,ニ,ニ\n渡し\t動詞,自立,*,*,五段・サ行,連用形,渡す,ワタシ,ワタシ\nた\t助動詞,*,*,*,特殊・タ,基本形,た,タ,タ\n。\t記号,句点,*,*,*,*,。,。,。\nEOS\n"
       (mecab-sparse-tostr m "太郎は次郎が持っている本を花子に渡した。"))
(mecab-destroy m)

(test-end)
