
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

(defun php-explode (delimiter string) 
  (split-string string delimiter))

(defun php-implode (glue &rest pieces) 
  (mapconcat 'identity (first pieces) glue))

(defun php-join (glue &rest pieces) 
  (apply 'php-implode glue pieces))

(defun php-preg-match-all (re_pattern subject) 
  (let ((matches '()) 
		(x 1)) 
	(while (< x 20) 
	  (string-match re_pattern subject) 
	  (when (match-string x subject) 
		(push (match-string x subject) matches)) 
	  (setq x (1+ x))) 
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
		  (setq pcount (1+ pcount))))))
  subject)

(defun php-str-repeat (input times) 
  (let ((pcount 0) 
		(str "")) 
	(while (> times pcount) 
	  (setq str (concat str input)) 
	  (setq pcount (1+ pcount)))
	str))

(defun php-stripos (haystack needle) 
  (string-match-p needle haystack))

(defun php-strpos (haystack needle) 
  (let ((case-fold-search nil)) 
	(string-match-p needle haystack)))

