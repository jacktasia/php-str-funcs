;; TODO: docstrings
;; TODO: a detailed note about PHP vs Emacs Lisp variable types
;; ...

(defun php-chop (str &optional charlist) 
  (php-rtrim str charlist))

(defun php-chr (ascii) 
  (string ascii))

(defun php-chunk-split (body &optional chunklen end) 
  (unless chunklen 
	(setq chunklen 76)) 
  (unless end 
	(setq end "\n")) 
  (with-temp-buffer 
	(insert body) 
	(goto-char 1) 
	(let ((chunks (/ (length body) chunklen)) 
		  (c 0)) 
	  (while (< c chunks) 
		(forward-char chunklen) 
		(insert end) 
		(incf c)) 
	  (buffer-string))))

(defun php-count-chars (str &optional mode) 
  "modes 0-2 return a hash-table; models 3-4 return a string"
  ;; TODO: add support for modes 3,4 w/ tests then needs a big cleanup
  (unless mode 
	(setq mode 0)) 
  (let ((c 0) 
		(ret (make-hash-table))) 
	(dolist (bv (string-to-list str)) 
	  (if (gethash bv ret) 
		  (puthash bv (1+ (gethash bv ret)) ret) 
		(puthash bv 1 ret))) 
	(cond ((= mode 0) 
		   (php-count-chars-mode0 ret)) 
		  ((= mode 1) ret) 
		  ((= mode 2) 
		   (php-count-chars-mode2 ret)) 
		  ((= mode 3) 
		   (php-count-chars-mode3 str)) 
		  ((= mode 4) 
		   (php-count-chars-mode4 str)))))

(defun php-count-chars-mode0 (counts-data) 
  "This is not a php function too. php-count-chars code is split
	up by mode since it's actually 5 functions" 
  (let ((c 0) 
		(counts counts-data)) 
	(while (> 256 c) 
	  (unless (gethash c counts) 
		(puthash c 0 counts)) 
	  (incf c))
	counts))

(defun php-count-chars-mode2 (count-data) 
  "This is not a php function too. php-count-chars code is split
	up by mode since it's actually 5 functions" 
  (let ((c 0) 
		(counts (make-hash-table))) 
	(while (> 256 c) 
	  (unless (gethash c count-data) 
		(puthash c 0 counts)) 
	  (incf c))
	counts))

(defun php-count-chars-mode3 (str) 
  "This is not a php function too. php-count-chars code is split
	up by mode since it's actually 5 functions" 
  (let ((strs '())) 
	(mapc 
	 (lambda (l) 
	   (unless (member l strs) 
		 (setq strs (cons l strs)))) 
	 (string-to-list str)) 
	(apply 'string (reverse strs))))

(defun php-count-chars-mode4 (str) 
  "This is not a php function too. php-count-chars code is split
	up by mode since it's actually 5 functions" 
  (let ((used-strs '()) 
		(c 0) 
		(strs '())) 
	(mapc 
	 (lambda (l) 
	   (unless (member l used-strs) 
		 (setq used-strs (cons l used-strs)))) 
	 (string-to-list str)) 
	(while (> 256 c) 
	  (unless (member c used-strs) 
		(setq strs (cons c strs))) 
	  (incf c)) 
	(apply 'string (reverse strs))))

(defun php-explode (delimiter string) 
  (split-string string delimiter))

(defun php-implode (glue &rest pieces) 
  (mapconcat 'identity (first pieces) glue))

(defun php-join (glue &rest pieces) 
  (apply 'php-implode glue pieces))

(defun php-lcfirst (str) 
  (with-temp-buffer 
	(insert str) 
	(goto-char 1) 
	(while (re-search-forward "^\\([A-Z]\\)" nil t) 
	  (replace-match (downcase (match-string 1)) t nil)) 
	(buffer-string)))

(defun php-ltrim (str &optional charlist) 
  (unless charlist 
	(setq charlist " \t\n\r\x0000\x000B")) 
  (with-temp-buffer 
	(insert str) 
	(goto-char 1) 
	(while (re-search-forward (format "^\\([%s]+\\)" charlist) nil t) 
	  (replace-match "" nil nil)) 
	(buffer-string)))

(defun php-ord (str) 
  (car (string-to-list str)))

;; TODO: This is too simpified; use a macro to get same functionality as PHP
(defun php-preg-match-all (re_pattern subject) 
  (let ((matches '()) 
		(x 1)) 
	(while (< x 20) 
	  (string-match re_pattern subject) 
	  (when (match-string x subject) 
		(push (match-string x subject) matches)) 
	  (incf x)) 
	(reverse matches)))

(defun php-rtrim (str &optional charlist) 
  (unless charlist 
	(setq charlist " \t\n\r\x0000\x000B")) 
  (with-temp-buffer 
	(insert str) 
	(goto-char 1) 
	(while (re-search-forward (format "\\([%s]+\\)$" charlist) nil t) 
	  (replace-match "" nil nil)) 
	(buffer-string)))

(defun php-sprintf (strformat &rest args) 
  (apply 'format strformat args))

(defun php-str-split (str) 
  (mapcar 'string (string-to-list str)))

(defun php-str-replace (search replace subject) 
  (unless (listp search) 
	(setq search (list search))) 
  (unless (listp replace) 
	(setq replace (list replace))) 
  (when (= (length search) 
		   (length replace)) 
	(let ((plength (length search)) 
		  (pcount 0)) 
	  (while (> plength pcount) 
		(with-temp-buffer 
		  (insert subject) 
		  (goto-char 1) 
		  (flet 
			  ((message (fs &rest args))) 
			(replace-string (nth pcount search) 
							(nth pcount replace))) 
		  (goto-char 1) 
		  (setq subject (buffer-string)) 
		  (incf pcount)))))
  subject)

(defun php-str-repeat (input times) 
  (let ((pcount 0) 
		(str "")) 
	(while (> times pcount) 
	  (setq str (concat str input)) 
	  (incf pcount))
	str))

(defun php-stripos (haystack needle) 
  (string-match-p needle haystack))

(defun php-strlen (str) 
  (length str))

(defun php-strpos (haystack needle) 
  (let ((case-fold-search nil)) 
	(string-match-p needle haystack)))

(defun php-strtolower (str) 
  (downcase str))

(defun php-strtoupper (str) 
  (upcase str))

(defun php-trim (str &optional charlist) 
  (php-rtrim (php-ltrim str charlist) charlist))

(defun php-ucfirst (str) 
  (with-temp-buffer 
	(insert str) 
	(goto-char 1) 
	(while (re-search-forward "^\\([A-Za-z]\\)" nil t)
	  (replace-match (upcase (match-string 1)) t nil)) 
	(buffer-string)))

(defun php-ucwords (str) 
  (php-implode " " (mapcar 
					(lambda (x) 
					  (php-ucfirst x)) 
					(php-explode " " str))))

(provide 'php-str-funcs)