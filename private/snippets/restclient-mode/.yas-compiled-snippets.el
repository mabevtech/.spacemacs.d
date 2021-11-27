;;; Compiled snippets and support files for `restclient-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'restclient-mode
                     '(("var" ":${1:var} ${2:$(if (string-prefix-p \"(\" yas-text) \":\" \"\")}= ${2:value}" ":var = value" nil nil nil "/Users/mabo3n/.emacs.d/private/snippets/restclient-mode/var" nil nil)
                       ("setvar" "-> run-hook (restclient-set-var \":${1:var}\" (cdr (assq '${2:field} (json-read))))\n" "-> run-hook (restclient-set-var \":var\" (cdr (assq 'field (json-read))))" nil nil nil "/Users/mabo3n/.emacs.d/private/snippets/restclient-mode/setvar" nil nil)
                       ("header" "${1:Header}: ${2:value}" "Header: value" nil nil nil "/Users/mabo3n/.emacs.d/private/snippets/restclient-mode/header" nil nil)
                       ("fileupload" "Content-type: text/plain\n\n< ${1:/Users/${2:mabo3n}/${3:Desktop/}${4:file}}\n" "< /Users/mabo3n/Desktop/file" nil nil nil "/Users/mabo3n/.emacs.d/private/snippets/restclient-mode/fileupload" nil nil)
                       ("encodeddata" "(url-hexify-string \"${1:data}\")\n" "(url-hexify-string \"data\")" nil nil nil "/Users/mabo3n/.emacs.d/private/snippets/restclient-mode/encodeddata" nil nil)))


;;; Do not edit! File generated at Sun Oct 31 02:48:24 2021
