(define-package "symex" "1.0" "An evil way to edit Lisp symbolic expressions as trees"
  '((emacs "24.4")
    (lispy "0.26.0")
    (paredit "24")
    (evil-cleverparens "20170718.413")
    (evil "1.2.14")
    (smartparens "1.11.0")
    (evil-surround "1.0.4")
    (hydra "0.15.0")
    (seq "2.22")
    (undo-tree "0.7.5"))
  :commit "d37532a9dcff8ec5a2fdc54f27b517890f972bfb" :authors
  '(("Siddhartha Kasivajhula" . "sid@countvajhula.com"))
  :maintainer
  '("Siddhartha Kasivajhula" . "sid@countvajhula.com")
  :keywords
  '("lisp" "evil")
  :url "https://github.com/countvajhula/symex.el")
;; Local Variables:
;; no-byte-compile: t
;; End:
