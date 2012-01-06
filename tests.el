;; http://nschum.de/src/emacs/elk-test/

(deftest "php-chr" (assert-equal "A" (php-chr 65)))

(deftest "php-chunk-split" (assert-equal "333\n333\n333\n" (php-chunk-split "333333333" 3)))

(deftest "php-explode" (assert-equal '("1" "2" "3" "4") 
									 (php-explode "," "1,2,3,4")))

(deftest "php-implode" (assert-equal "1,2,3,4" (php-implode "," '("1" "2" "3" "4"))))

(deftest "php-join" (assert-equal "1,2,3,4" (php-join "," '("1" "2" "3" "4"))))

(deftest "php-preg-match-all" 
  (assert-equal '("aaa" "123") 
				(php-preg-match-all "\\([a-z]+\\)\\([0-9]+\\)" "aaa123")))

(deftest "php-sprintf" (assert-equal "one two 3" (php-sprintf "%s %s %i" "one" "two" 3)))

(deftest "php-str-repeat" (assert-equal "aaaaa" (php-str-repeat "a" 5)))

(deftest "php-str-replace with lists" 
  (assert-equal "hello gold" (php-str-replace '("bye" "wor") 
											  '("hello" "go") "bye world")))

(deftest "php-str-replace with strings" 
  (assert-equal "asdfware" (php-str-replace "gold" "asdf" "goldware")))

(deftest "php-str-split" (assert-equal '("a" "s" "d" "f") 
									   (php-str-split "asdf")))

(deftest "php-stripos nil" (assert-equal nil (php-stripos "abcdefgh" "z")))

(deftest "php-stripos t" (assert-equal 2 (php-stripos "abcDefgh" "cde")))

(deftest "php-strpos nil" (assert-equal nil (php-strpos "abcdefgh" "cDe")))

(deftest "php-strpos t" (assert-equal 2 (php-strpos "abcDefgh" "cDe")))
