(require 'ob-js)

(add-to-list 'org-babel-load-languages '(js . t))

(org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)

(add-to-list 'org-babel-tangle-lang-exts '("js" . "js"))

(provide 'babel)
