;; -*- no-byte-compile: t; -*-
;;; .doom.d/packages.el

;;; Examples:
;; (package! some-package)
;; (package! another-package :recipe (:host github :repo "username/repo"))
;; (package! builtin-package :disable t)


(package! flycheck)
(package! elpy)
(package! py-autopep8)                     ;; Run autopep8 on save
(package! blacken)
(package! pip-requirements)
(package! python-pytest)
(package! direnv)
;; (package! company-anaconda)
(package! company-jedi)
(package! yasnippet)
(package! arduino-mode)
(package! docker)
(package! dockerfile-mode)
(package! docker-compose-mode)
(package! ob-http)  ;; org-babel http for REST requests
(package! ng2-mode)
(package! prettier-js)
(package! outshine)
