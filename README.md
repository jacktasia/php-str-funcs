# PHP String Functions in Emacs Lisp

## About

In the process of creating some of the [php string functions](http://php.net/manual/en/ref.strings.php) in [Emacs Lisp](http://en.wikipedia.org/wiki/Emacs_Lisp) for fun. Also I think it may be an interesting way to learn Emacs Lisp for people who are more familiar with PHP (which I would think would be most).

I plan on fully commenting the code after I finish my *first pass* of the functions.

## Tests

tests.el uses [elk-test](http://nschum.de/src/emacs/elk-test/).

## Functions 

### Completed (so far) *first pass*

*There may be bugs. Let me know if you find any or know a better way of doing something.*

* chop
* chr
* chunk_split
* count_chars
* explode
* implode
* join
* lcfirst
* ltrim
* ord
* rtrim
* sprintf
* str_split
* str_replace
* str_repeat
* stripos
* strlen
* strpos
* strtolower
* strtoupper
* trim
* ucfirst
* ucwords

### Next Up

* preg_replace
* preg_replace_all
* md5_file
* money_format
* nl2br
* number_format
* parse_str
* sprintf
* str_getcsv
* str_ireplace
* str_pad
* str_shuffle
* str_world_count
* stripcslashes
* stripslashes
* strpbrk
* strrchr
* strrev
* strripos
* strrpos
* strspn
* strstr
* substr_count
* substr_replace
* substr
* wordwrap

### Maybe
* echo
* crc32
* get_html_translation_table
* html_entity_decode
* htmlspecialchars_decde
* htmlspecialchars
* sha1
* sha1_file
* strcspn
* strip_tags
* stristr
* strtok
* strtr
* vprintf
* vsprintf

### Probably won't do
* bin2hex
* convert_cyr_string
* convert_uudecode
* convert_uuencode
* crypt
* fprintf
* get_html_translation_table ^
* hebrev
* hebrevc
* hex2bin
* levenshtein
* localeconv
* metaphone
* nl_langinfo
* soundex
* sscanf
* strnatcasecmp
* strnatcmp
* strncasecmp
* strncmp
* substr_compare
* similar_text
* str_rot13
* strcasecmp
* strcmp
* strcoll


