(defvar my/packages '(markdown-mode
                      multiple-cursors
                      clj-refactor
                      ;; slamhound
                      tabbar
                      color-theme-solarized
                      solarized-theme
                      tide
                      yasnippet-snippets
                      ts-comint
                      elm-mode
                      ;; eyebrowse
                      rvm
                      ))

(prelude-require-packages my/packages)

(require 'org)
(defvar my/todo-files '("~/org/today.org" "~/org/todo.org" "~/org/home.org"))
(setq org-agenda-files my/todo-files)
;; https://orgmode.org/worg/org-configs/org-config-examples.html#orgbf5a5f8
(define-key org-mode-map (kbd "C-RET") 'org-insert-heading-respect-content)
(define-key org-mode-map (kbd "S-M-RET") 'org-insert-todo-heading)
;; (add-hook 'org-mode-hook
;;           (lambda ()
;;             (local-set-key (kbd "C-RET") 'org-insert-heading-respect-content)
;;             ;; (local-set-key "\M-n" 'outline-next-visible-heading)
;;             ;; (local-set-key "\M-p" 'outline-previous-visible-heading)
;;             ;; ;; table
;;             ;; (local-set-key "\C-\M-w" 'org-table-copy-region)
;;             ;; (local-set-key "\C-\M-y" 'org-table-paste-rectangle)
;;             ;; (local-set-key "\C-\M-l" 'org-table-sort-lines)
;;             ;; ;; fix tab
;;             ;; (local-set-key "\C-y" 'yank)
;;             ))

;; Keys
;; escape sequences
(defvar my/escape-sequence-chars '(";" "/" "?" "(" ")" "{" "}" "<" ">" "+"))
(defun my/create-esc-seq-mapping (character)
  (let ((esc-seq (concat "M-[ " character))
        (orig-seq (concat "C-" character)))
    (define-key key-translation-map (kbd esc-seq) (kbd orig-seq))))
(mapc 'my/create-esc-seq-mapping my/escape-sequence-chars)

(define-key key-translation-map (kbd "M-[ m") (kbd "C-RET"))
(define-key key-translation-map (kbd "M-[ M") (kbd "M-S-RET"))


;; key mappings
(defun my/create-global-mapping (kbd-fn)
  (global-set-key (kbd (car kbd-fn)) (car (cdr kbd-fn))))
;; (defun my/create-mapping (mmap)
;;  (lambda (kbd-fn) (define-key mmap (kbd (car kbd-fn)) (car (cdr kbd-fn)))))
(defun my/create-prelude-mapping (kbd-fn)
  (define-key prelude-mode-map (kbd (car kbd-fn)) (car (cdr kbd-fn))))

(defvar my/global-keys
  '(("M-i" other-window)
    ("M-I" (lambda () (interactive) (other-window -1)))
    ("M-K" (lambda () (interactive) (kill-buffer (current-buffer))))
    ("M-F" tabbar-forward-tab)
    ("M-B" tabbar-backward-tab)
    ;; ("C-c x" clipboard-kill-region)
    ;; ("C-c c" clipboard-kill-ring-save)
    ;; ("C-c v" clipboard-yank)
    ;; ("M-DEL" subword-backward-kill)
    ))
(mapc 'my/create-global-mapping my/global-keys)

(defvar my/prelude-keys '())
(mapc 'my/create-prelude-mapping my/prelude-keys)

;; Hooks
(defun my/clojure-mode-hook ()
  (clj-refactor-mode 1)
  (yas-minor-mode 1)
  (cljr-add-keybindings-with-prefix "C-c C-m"))
(add-hook 'clojure-mode-hook #'my/clojure-mode-hook)

(make-variable-buffer-local 'prelude-clean-whitespace-on-save)
(defun my/markdown-settings ()
  (setq prelude-clean-whitespace-on-save nil))
(add-hook 'markdown-mode-hook 'my/markdown-settings)

;; fix how helm splits windows
(setq split-height-threshold nil)
;; (setq 'prelude-theme solarized)

(beacon-mode 0)


(defun light-mode ()
  (interactive)
  (set-terminal-parameter nil 'background-mode 'light)
  (setq frame-background-mode 'light)
  (mapc 'frame-set-background-mode (frame-list))
  (enable-theme 'solarized))

(defun dark-mode ()
  (interactive)
  (set-terminal-parameter nil 'background-mode 'dark)
  (setq frame-background-mode 'dark)
  (mapc 'frame-set-background-mode (frame-list))
  (enable-theme 'solarized))

(dark-mode)

;; https://stackoverflow.com/questions/1242352/get-font-face-under-cursor-in-emacs
(defun what-face (pos)
  (interactive "d")
  (let ((face (or (get-char-property (pos) 'read-face-name)
                  (get-char-property (pos) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

;; could also try https://github.com/jinzhu/configure/blob/master/emacs/settings/tabbar.el
(defun my/tabbar-buffer-groups-by-project ()
  "Return the list of group names the current buffer belongs to.
Return a list of one element based on major mode."
  (list
   (cond
    ((or (get-buffer-process (current-buffer))
         ;; Check if the major mode derives from `comint-mode' or
         ;; `compilation-mode'.
         (tabbar-buffer-mode-derived-p
          major-mode '(comint-mode compilation-mode)))
     "Process"
     )
    ((or
      (memq major-mode '(fundamental-mode))
      (member (buffer-name)
              '("*scratch*" "*Messages*")))
     "Common"
     )
    ((eq major-mode 'dired-mode)
     "Dired"
     )
    ((memq major-mode
           '(help-mode apropos-mode Info-mode Man-mode Custom-mode))
     "Help"
     )
    ((memq major-mode
           '(rmail-mode
             rmail-edit-mode vm-summary-mode vm-mode mail-mode
             mh-letter-mode mh-show-mode mh-folder-mode
             gnus-summary-mode message-mode gnus-group-mode
             gnus-article-mode score-mode gnus-browse-killed-mode))
     "Mail"
     )
    ((eq major-mode 'helm-major-mode)
     "Hmm")
    (t
     (if
         (projectile-project-p)
         (projectile-project-name)
         ;; Return `mode-name' if not blank, `major-mode' otherwise.
         (if (and (stringp mode-name)
                  ;; Take care of preserving the match-data because this
                  ;; function is called when updating the header line.
                  (save-match-data (string-match "[^ ]" mode-name)))
             mode-name
           (symbol-name major-mode)))
     ))))

(setq tabbar-buffer-groups-function 'my/tabbar-buffer-groups-by-project)
;; (setq tabbar-buffer-groups-function 'tabbar-buffer-groups)

;; https://www.reddit.com/r/emacs/comments/6i0u5e/react_jsx_indentation_on_emacs/
(require 'web-mode)

(add-hook
 'web-mode-hook
 (lambda ()
   (if
       (equal web-mode-content-type "javascript")
       (web-mode-set-content-type "jsx")
     (message "now set to: %s" web-mode-content-type))))

(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

;; https://github.com/ananthakumaran/tide
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)
(add-hook 'typescript-mode-hook #'setup-tide-mode)
(add-hook 'typescript-mode-hook #'smartparens-mode)

(add-hook 'web-mode-hook
          (lambda ()
            (when (or
                   (string-equal "tsx" (file-name-extension buffer-file-name)))
              (setup-tide-mode))
            (smartparens-mode)))
;; enable typescript-tslint checker
(flycheck-add-mode 'typescript-tslint 'web-mode)

(require 'tide)
(define-key tide-mode-map (kbd "M-RET") 'tide-fix)

;; http://jbm.io/2014/01/react-in-emacs-creature-comforts/
;; (defun modify-syntax-table-for-jsx ()
;;   (modify-syntax-entry ?< "(>")
;;   (modify-syntax-entry ?> ")<"))
;; (add-hook 'js2-mode-hook 'modify-syntax-table-for-jsx)
;; (add-hook 'web-mode-hook 'modify-syntax-table-for-jsx)

;; (eval-after-load 'js2-mode
;;   '(sp-local-pair 'js2-mode "<" ">"))
;; (eval-after-load 'web-mode
;;   '(sp-local-pair 'js2-mode "<" ">"))

;; (setq tide-format-options '(:indentSize 4
;;                             :tabSize 4))


;; http://tkf.github.io/emacs-jedi/latest/
;; (defun my/python-mode-hooks ()
;;   (jedi:setup)
;;   (anaconda-mode 0))
;; (add-hook 'python-mode-hook 'my/python-mode-hooks)
;; (setq jedi:complete-on-dot t)  ; optional
(yas-global-mode)
(rvm-use-default)
