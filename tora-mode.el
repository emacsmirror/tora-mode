;;; tora-mode.el --- Major mode of Tora languages

;; Copyright (C) 2012 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; URL:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'font-lock)

(defgroup tora nil
  "Tora major mode"
  :group 'languages)

(defcustom tora-tab-width tab-width
  "The tab width to use when indenting"
  :type 'integer
  :group 'tora)

;; mode
(defvar tora-mode-map (make-keymap)
  "Keymap for Tora major mode")

;;
;; Indentation
;;

(defun tora-indent-line ()
  "Indent current line as tora"
  (interactive)
  (if (= (point) (point-at-bol))
      (insert-tab)
    (save-excursion
      (let ((prev-indent (tora-previous-indent))
            (cur-indent (current-indentation)))
        ;; Shift one column to the left
        (beginning-of-line)
        (insert-tab)

        (when (= (point-at-bol) (point))
          (forward-char tora-tab-width))

        ;; We're too far, remove all indentation.
        (when (> (- (current-indentation) prev-indent) tora-tab-width)
          (backward-to-indentation 0)
          (delete-region (point-at-bol) (point)))))))

(defun tora-previous-indent ()
  "Return the indentation level of the previous non-blank line."
  (save-excursion
    (forward-line -1)
    (if (bobp)
        0
      (progn
        (while (and (looking-at "^[ \t]*$") (not (bobp))) (forward-line -1))
        (current-indentation)))))

;; String Literals
(defvar tora-string-regexp
  "\"\\([^\\]\\|\\\\.\\)*?\"\\|'\\([^\\]\\|\\\\.\\)*?'")

(defvar tora-storage-regexp
  "my" "local")

(defvar tora-this-regexp
  "this")

;; remainder keywords
;; static

(defvar tora-builtin-func-regexp
  (regexp-opt
   '("new" "alias")
   'words))

(defvar tora-boolean-regexp
  "\\<\\(true\\|false\\|undef\\|undefined\\)\\>")

;; Regular Expressions Literal
(defvar tora-regexp-regexp
  "\\/\\(\\\\.\\|\\[\\(\\\\.\\|.\\)+?\\]\\|[^/]\\)+?\\/")

;; Label Regexp
(defvar tora-label-regexp
  "^\\<[a-zA-Z_][a-zA-Z_0-9]+:")

(defvar tora-arrow-regexp
  "->")

;;; declaration
(defconst tora-identifier-regexp
  "$?[a-zA-Z_][a-zA-Z0-9_:]*")

;; class declaration
(defconst tora-class-definition-regexp
  (concat
   "\\<class\\s-+\\(" tora-identifier-regexp "\\)"))

;; variable declaration
(defconst tora-variable-definition-regexp
  (concat
   "\\<my\\s-+\\(" tora-identifier-regexp "\\)"))

;; function declaration
(defconst tora-function-definition-regexp
  (concat
   "\\<sub\\s-+\\(" tora-identifier-regexp "\\)"))

;;
(defconst tora-import-module-regexp
  (concat
   "\\<use \\(" tora-identifier-regexp "\\)"))

;; Tora keywords
(defvar tora-keywords
  '("class" "for" "new" "sub" "if" "elisf" "else" "next" "last" "return"
    "initialize" "while" "static" "alias" "use" "unless"))

(defvar tora-keywords-regexp
  (regexp-opt (append tora-keywords) 'words))

(defvar tora-font-lock-keywords
  `(
    (,tora-string-regexp . font-lock-string-face)
    (,tora-this-regexp . font-lock-variable-name-face)
    (,tora-class-definition-regexp
     (1 font-lock-type-face))
    (,tora-variable-definition-regexp
     (1 font-lock-variable-name-face))
    (,tora-function-definition-regexp
     (1 font-lock-function-name-face))
    (,tora-import-module-regexp
     (1 font-lock-function-name-face))
;    (,tora-regexp-regexp . font-lock-constant-face)
    (,tora-storage-regexp . font-lock-keyword-face)
    (,tora-label-regexp . font-lock-keyword-face)
    (,tora-arrow-regexp . font-lock-keyword-face)
    (,tora-boolean-regexp . font-lock-constant-face)
    (,tora-keywords-regexp . font-lock-keyword-face)))

(defvar tora-mode-hook nil
  "A hook for you to run your own code when the mode is loaded")

(defconst tora-syntax-propertize-function
  (syntax-propertize-rules
   ("__\\(DATA\\|END\\)__\\(\r?\n\\|.\\)+\\(\\'\\)"
    (1 "<")
    (3 ">"))))

;;;###autoload
(define-derived-mode tora-mode fundamental-mode
  "Tora"
  "Major mode for editing Tora"

  ;; key bindings

  ;; code for syntax highlighting
  (setq font-lock-defaults '((tora-font-lock-keywords)))

  ;; perl style comment: "# ..."
  (modify-syntax-entry ?#  "< b" tora-mode-syntax-table)
  (modify-syntax-entry ?\n "> b" tora-mode-syntax-table)
  (make-local-variable 'comment-start)
  (setq comment-start "#")

  ;; __DATA__, __END__
  (set (make-local-variable 'syntax-propertize-function)
       tora-syntax-propertize-function)

  ;; indentation
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'tora-indent-line)
  (set (make-local-variable 'tab-width) tora-tab-width)

  (run-hooks 'tora-mode-hook))

(provide 'tora-mode)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.tra" . tora-mode))

;;; tora-mode.el ends here
