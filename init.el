(add-to-list 'load-path (expand-file-name "~/.emacs.d/"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/yasnippet-0.6.1c/"))

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(grep-highlight-matches (quote always))
 '(indent-tabs-mode nil)
 '(max-lisp-eval-depth 50000)
 '(max-specpdl-size 50000)
 '(safe-local-variable-values (quote ((buffer-file-coding-system . utf-8-auto) (buffer-file-coding-system . utf-8-dos) (buffer-file-coding-system . windows-1250-unix) (buffer-file-coding-system . utf-8-unix)))))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(diff-added ((t (:foreground "DodgerBlue" :bold t))))
 '(diff-removed ((t (:foreground "FireBrick" :bold t)))))

;; set grep-command under Windows
(if (and (eq system-type 'windows-nt) (file-exists-p "C:/gnuwin32/bin/grep.exe"))
    (setq grep-command "\"C:/gnuwin32/bin/grep.exe\" -n")
  (setq grep-command "grep -n"))

(setq inhibit-splash-screen t)
(setq scroll-step 2)
(setq c-default-style "bsd"
      c-basic-offset 4)
(tool-bar-mode -1)
(set-face-attribute 'default nil :height 90)

;; C-c, C-v etc.
(cua-mode t)

;; php-mode for Emacs23
(autoload 'php-mode "php-mode" "Major mode for editing php code." t)
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))

;; required by one of my snippets
(require 'perl-mode)

;; YASnippet - http://code.google.com/p/yasnippet/
(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/.emacs.d/yasnippet-0.6.1c/snippets")

;; hg
(require 'mercurial)

;; js2-mode
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; sql-indent
(eval-after-load "sql"
  (load-library "sql-indent"))

;; zencoding
(require 'zencoding-mode)
(add-hook 'sgml-mode-hook 'zencoding-mode) ;; Auto-start on any markup modes

;; markdown
(autoload 'markdown-mode "markdown-mode.el"
   "Major mode for editing Markdown files" t)
(setq auto-mode-alist
   (cons '("\\.text" . markdown-mode) auto-mode-alist))

;; switch vc-diff to unified diff 
(setf vc-diff-switches "-u")

;; remember password in tramp session
(setq password-cache-expiry nil)

;; Better auto completion for buffer switching and command execution.
;; ido-enable-flex-matching means that if the entered string does not match any buffer name, any buffer name containing the entered characters in the given sequence will match. 
(require 'ido)
;(ido-mode 1)
(ido-mode 'buffer)
(setq ido-enable-flex-matching t)
(put 'narrow-to-region 'disabled nil)

;; paren matching
(require 'mic-paren)
(paren-activate)
(setq paren-priority 'close)
 
;; enter (RET) mapping
(dolist (hook (list
               'js2-mode-hook
               'perl-mode-hook))
  (add-hook hook
            (function
             (lambda ()
               (define-key (current-local-map) (kbd "<return>")
                 'reindent-then-newline-and-indent)))))

;; C-o 
(define-key isearch-mode-map (kbd "C-o")
  (lambda ()
    (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string
                 (regexp-quote isearch-string))))))

;; grep - project LP
(defun do-grep (query)
  "Run the grep command from the current project root."
  (interactive "MGrep query: ")
  (let ((dir (file-relative-name (vc-hg-root buffer-file-name) (file-name-directory buffer-file-name))))
    (grep (concat grep-command
                  " -r "
                  "\"" (replace-regexp-in-string "\"" "\\\\\"" query) "\""
                  (concat " --exclude=" dir "cgi-bin/hittest.pl ")
                  (concat " --exclude=" dir "modules/Configuration.pm ")
                  (mapconcat (lambda(sub) (concat dir sub))
                             '("cgi-bin/*.pl"
                               "htdocs/*.html"
                               "htdocs/css/*.css"
                               "modules/*.pm"
                               "scripts/*.js"
                               "tools/*.*")
                             " ")))))
(global-set-key (kbd "C-M-g") 'do-grep)

(defun do-grep-global (query)
  "Run the grep command from the current project root."
  (interactive "MGrep query: ")
  (let ((dir (file-name-directory buffer-file-name)))
    (grep (concat grep-command
                  " -r "
                  "\"" (replace-regexp-in-string "\"" "\\\\\"" query) "\" "
                  (mapconcat (lambda(sub) (concat dir sub))
                             '("../class/*.php"
                               "../includes/*.php"
                               "../templates/*.tpl"
                               "../*.php")
                             " ")))))
(global-set-key (kbd "C-M-S-G") 'do-grep-global)

(defun bf-pretty-print-xml-region (begin end)
  "Pretty format XML markup in region. You need to have nxml-mode
http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do
this.  The function inserts linebreaks to separate tags that have
nothing but whitespace between them.  It then indents the markup
by using nxml's indentation rules."
  (interactive "r")
  (save-excursion
      (nxml-mode)
      (goto-char begin)
      (while (search-forward-regexp "\>[ \\t]*\<" nil t) 
        (backward-char) (insert "\n"))
      (indent-region begin end))
    (message "Ah, much better!"))

;; toggle windows vertically / horizontally
(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
	     (next-win-buffer (window-buffer (next-window)))
	     (this-win-edges (window-edges (selected-window)))
	     (next-win-edges (window-edges (next-window)))
	     (this-win-2nd (not (and (<= (car this-win-edges)
					 (car next-win-edges))
				     (<= (cadr this-win-edges)
					 (cadr next-win-edges)))))
	     (splitter
	      (if (= (car this-win-edges)
		     (car (window-edges (next-window))))
		  'split-window-horizontally
		'split-window-vertically)))
	(delete-other-windows)
	(let ((first-win (selected-window)))
	  (funcall splitter)
	  (if this-win-2nd (other-window 1))
	  (set-window-buffer (selected-window) this-win-buffer)
	  (set-window-buffer (next-window) next-win-buffer)
	  (select-window first-win)
	  (if this-win-2nd (other-window 1))))))

(global-set-key (kbd "C-x 4") 'toggle-window-split)
