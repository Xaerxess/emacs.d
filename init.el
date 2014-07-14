(add-to-list 'load-path (expand-file-name "~/.emacs.d/"))

(require 'package)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

(setq package-enable-at-startup nil)
(package-initialize)

(setq package-list '(auto-complete ido markdown-mode mic-paren less-css-mode sql-indent yasnippet zencoding-mode))
(or (file-exists-p package-user-dir)
    (package-refresh-contents))
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(prefer-coding-system 'utf-8)
;(setq coding-system-for-read 'utf-8-unix)
;(setq coding-system-for-write 'utf-8-unix)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(c-basic-offset 2)
 '(c-offsets-alist (quote ((func-decl-cont . ++) (arglist-intro . ++))))
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

(setq inhibit-splash-screen t)          ;; turn off splash screen
(setq scroll-step 1)
(setq c-default-style "bsd"
      c-basic-offset 2)
;; (setq c-offsets-alist (quote ((func-decl-cont . ++))))

(setq-default tab-width 2)
(tool-bar-mode -1)
(set-face-attribute 'default nil :height 90)
(setq x-select-enable-clipboard t)      ;; better clipboard under X11
(global-linum-mode t)                   ;; line numbers
(defalias 'yes-or-no-p 'y-or-n-p)       ;; I'm lazy ;)

(setq hscroll-margin 4)
(setq hscroll-step   4)
(global-set-key (kbd "C-<") (function (lambda () (interactive) (scroll-left 4))))
(global-set-key (kbd "C->") (function (lambda () (interactive) (scroll-right 4))))

;; CUA mode: C-c, C-v etc.
(cua-mode t)

;; Navigate windows with M-<arrows>
(windmove-default-keybindings 'meta)
(setq windmove-wrap-around t)

;; set browser
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome")

;; required by one of my snippets
(require 'perl-mode)

;; YASnippet
(require 'yasnippet)
(setq yas-snippet-dirs (append yas-snippet-dirs
                               '("~/.emacs.d/yasnippets")))
(yas-global-mode 1)

;; regular auto-complete initialization
(require 'auto-complete-config)
(ac-config-default)

;; nXhtml-mode
(load "~/.emacs.d/nxhtml/autostart")

;; php-mode
(add-to-list 'auto-mode-alist '("\\.module\\'" . php-mode))
(add-to-list 'auto-mode-alist '("\\.inc\\'" . php-mode))

(add-hook 'php-mode-hook
      (function (lambda ()
                  (setq indent-tabs-mode nil)
                  (setq tab-width 2)
                  (setq c-basic-offset 2))))

;; java-mode
(add-hook 'java-mode-hook
          (lambda ()
            "Treat Java 1.5 @-style annotations as comments."
            (setq c-comment-start-regexp "(@|/(/|[*][*]?))")
            (modify-syntax-entry ?@ "< b" java-mode-syntax-table)))

;; switch vc-diff to unified diff
(setf vc-diff-switches "-u")

;; js2-mode - currently testing built-in javascript-mode
;; (autoload 'js2-mode "js2" nil t)
;; (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; sql-indent
(eval-after-load "sql"
  (load-library "sql-indent"))

;; HTML & CSS hooks
;; TODO: organize this
(defvar hexcolour-keywords
  '(("#[abcdef[:digit:]]\\{6\\}"
     (0 (put-text-property
         (match-beginning 0)
         (match-end 0)
         'face (list :background
                     (match-string-no-properties 0)))))))
(defun hexcolour-add-to-font-lock ()
  (font-lock-add-keywords nil hexcolour-keywords))
(add-hook 'css-mode-hook 'hexcolour-add-to-font-lock)

(defun wrap-text (aa bb)
  "Wrap strings aa and bb around current word or region."
  (save-excursion
    (let (p1 p2 myword)
      (if (and transient-mark-mode mark-active)
          (progn (setq p1 (region-beginning)) (setq p2 (region-end)))
        (progn
          (skip-chars-backward "-A-Za-z")
          (setq p1 (point))
          (skip-chars-forward "-A-Za-z")
          (setq p2 (point))))
      (setq myword (buffer-substring-no-properties p1 p2))
      (goto-char p2) (insert bb)
      (goto-char p1) (insert aa))))
(defun wrap-strong ()
  "Wrap a HTML <strong> tag around current word or region."
  (interactive)
  (wrap-text "<strong>" "</strong>"))
(global-set-key (kbd "<f6>") 'wrap-strong)

;; less-css-mode.el
(require 'less-css-mode)

;; zencoding
(require 'zencoding-mode)
(add-hook 'sgml-mode-hook 'zencoding-mode) ;; Auto-start on any markup modes

;; markdown
(require 'markdown-mode)
(setq auto-mode-alist
   (cons '("\\.text" . markdown-mode) auto-mode-alist))

;; whitespace-mode
(setq whitespace-line-column 200)
(setq newline-mark nil)
;; make whitespace-mode use just basic coloring
(setq whitespace-style (quote
  (spaces tabs newline space-mark tab-mark newline-mark)))

;; remember password in tramp session
(setq password-cache-expiry nil)

;; Better auto completion for buffer switching and command execution.
;; ido-enable-flex-matching means that if the entered string does not match any buffer name, any buffer name containing the entered characters in the given sequence will match.
(require 'ido)
(ido-mode 'buffer)
(setq ido-enable-flex-matching t)
(put 'narrow-to-region 'disabled nil)
(global-set-key (kbd "<M-f4>") 'save-buffers-kill-terminal)
(global-set-key (kbd "C-x C-c") 'ido-switch-buffer) ;; save-buffers-kill-terminal remapped to <M-f4>
(global-set-key (kbd "C-x C-b") 'ibuffer)           ;; override buffer-menu

;; paren matching
(require 'mic-paren)
(paren-activate)
(setq paren-priority 'close)

;; enter (RET) mapping
(dolist (hook (list
               ;;'js2-mode-hook
               'php-mode-hook
               'perl-mode-hook))
  (add-hook hook
            (function
             (lambda ()
               (define-key (current-local-map) (kbd "<return>")
                 'reindent-then-newline-and-indent)))))

;; C-o (occur)
(define-key isearch-mode-map (kbd "C-o")
  (lambda ()
    (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string
                 (regexp-quote isearch-string))))))

;; set grep-command under Windows
(if (and (eq system-type 'windows-nt) (file-exists-p "C:/gnuwin32/bin/grep.exe"))
    (setq grep-command "\"C:/gnuwin32/bin/grep.exe\" -n")
  (setq grep-command "grep -n"))

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
                               "htdocs/scripts/*.js"
                               "modules/*.pm"
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

(defun pretty-print-xml-region (begin end)
  "Pretty format XML markup in region. You need to have nxml-mode
http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do
this.  The function inserts linebreaks to separate tags that have
nothing but whitespace between them.  It then indents the markup
by using nxml's indentation rules."
  (interactive "r")
  (save-excursion
    (nxml-mode)
    (goto-char begin)
    ;; split <foo><foo> or </foo><foo>, but not <foo></foo>
    (while (search-forward-regexp ">[ \t]*<[^/]" end t)
      (backward-char 2) (insert "\n") (setq end (1+ end)))
    ;; split <foo/></foo> and </foo></foo>
    (goto-char begin)
    (while (search-forward-regexp "<.*?/.*?>[ \t]*<" end t)
      (backward-char) (insert "\n") (setq end (1+ end)))
    (indent-region begin end nil)
    (normal-mode))
  (message "All indented!"))

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
