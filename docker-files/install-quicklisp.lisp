; Complete the installation of quicklisp

(load "quicklisp.lisp")
(quicklisp-quickstart:install)

(load "~/quicklisp/setup.lisp")
(ql:add-to-init-file)
