;; TODO: docstrings
;; TODO: a detailed note about PHP vs Emacs Lisp variable types
;; ...

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
		   (php-count-chars-mode2 ret)))))
;;(php-count-chars "aasdf")

(defun php-count-chars-mode0 (counts-data) 
  "This is not a php function too. php-count-chars code is split
	up since it's actually 4 functions" 
  (let ((c 0) 
		(counts counts-data)) 
	(while (> 256 c) 
	  (unless (gethash c counts) 
		(puthash c 0 counts)) 
	  (incf c))
	counts))

(defun php-count-chars-mode2 (count-data) 
  "This is not a php function too. php-count-chars code is split
	up since it's actually 4 functions" 
  (let ((c 0) 
		(counts (make-hash-table))) 
	(while (> 256 c) 
	  (unless (gethash c count-data) 
		(puthash c 0 counts)) 
	  (incf c))
	counts))

;;(defun php

(defun php-explode (delimiter string) 
  (split-string string delimiter))

(defun php-implode (glue &rest pieces) 
  (mapconcat 'identity (first pieces) glue))

(defun php-join (glue &rest pieces) 
  (apply 'php-implode glue pieces))

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

(defun php-strpos (haystack needle) 
  (let ((case-fold-search nil)) 
	(string-match-p needle haystack)))

(provide 'php-str-funcs)