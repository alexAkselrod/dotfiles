;; For questions look here https://github.com/daviwil/emacs-from-scratch/blob/75f1d4e08512c49ea073c26058df6d4cca3a0d6b/Desktop.org#panel-with-polybar

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
;;=======================Polybar===================================================
(defun alex/polybar-exwm-workspace ()
  (pcase exwm-workspace-current-index
    (0 "Main")
    (1 "Internet")
    (2 "Video")
    (3 3)
    (4 4)))

(defun efs/send-polybar-hook (module-name hook-index)
  (start-process-shell-command "polybar-msg" nil (format "polybar-msg hook %s %s" module-name hook-index)))

(defun efs/send-polybar-exwm-workspace ()
  (efs/send-polybar-hook "exwm-workspace" 1))

;; Update panel indicator when workspace changes
(add-hook 'exwm-workspace-switch-hook #'efs/send-polybar-exwm-workspace)

;;=============================== Applications ====================================
(setq processes '((vpn 'nil "/opt/cisco/anyconnect/bin/vpnui") (browser 'nil "opera") (time 'nil "/opt/TiMe/time-desktop") (panel 'nil "polybar panel") (ktalk 'nil "/opt/Толк/ktalk") (bluetooth 'nil "blueman-manager")))

(defun app/kill-process (name)
  (setq pid (nth 1 (assq name processes)))
   (when pid
     (ignore-errors
       (kill-process pid)))
   (setf pid nil)
  )

(defun app/start-process (name)
  (app/kill-process name)
  (setq command (nth 2 (assq name processes)))
  (setq pid (start-process-shell-command command nil command))
  (setf (nth 1 (assq name processes)) pid)
  )

(defun app/start-bluetooth ()
  (interactive)
  (app/kill-process 'bluetooth)
  (app/start-process 'bluetooth)
  )

(defun app/start-browser ()
  (interactive)
  (app/kill-process 'browser)
  (app/start-process 'browser)
  )

(defun app/start-vpn ()
  (interactive)
  (app/kill-process 'vpn)
  (app/start-process 'vpn)
  )

(defun app/start-time ()
  (interactive)
  (app/kill-process 'time)
  (app/start-process 'time)
  )

(defun app/start-panel ()
  (interactive)
  (app/kill-process 'panel)
  (app/start-process 'panel)
  )

(defun app/start-ktalk ()
  (interactive)
  (app/kill-process 'ktalk)
  (app/start-process 'ktalk)
  )

(defun app/set-font-size (size)
  (print size)
  (set-face-attribute 'default nil
		      :font "JetBrains Mono"
		      :weight 'light
		      :height size)
  )

(defun app/detect-scale ()
  (interactive)
  (setenv "GDK_SCALE" "2")
  (app/set-font-size 260)
  (dolist (el (display-monitor-attributes-list))    
    (setq d-name (cdr (assq 'name el)))
    (when (not (string= d-name "eDP-1"))
      (setenv "GDK_SCALE" "1")
      (app/set-font-size 130)
      )
    )
  )

(defun app/rerun-gtk-apps ()
  (interactive)
  (dolist (element '(browser time ktalk bluetooth))
    (app/start-process element)
    )
  )
;;=============================== Displays =====================================================

(defun disp/enable-home ()
  (interactive)
  (call-process-shell-command "xrandr -d :0 --output DP-1 --auto --output eDP-1 --off")
  (setenv "GDK_SCALE" "1")
  (app/set-font-size 130)
  (app/rerun-gtk-apps)
  )

(defun disp/enable-mobile ()
  (interactive)
  (call-process-shell-command "autorandr --change mobile")
  (setenv "GDK_SCALE" "2")
  (app/set-font-size 260)
  (app/rerun-gtk-apps)
  )

;;===============================Development=====================================================
;; Company mode
(use-package company
  :ensure t
  :init
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1)
  :config
    (global-set-key (kbd "<C-return>") 'company-complete)
    (global-company-mode 1)
)

(use-package flycheck-golangci-lint
	     :ensure t)

(use-package yasnippet
  :ensure t)

(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))

;; For details look here https://gitlab.com/skybert/my-little-friends/blob/master/emacs/.emacs
;; lsp-workspace-folders-* to add/remove folder to/from LSP
(use-package lsp-java
  :ensure t
  :config
  (setq lsp-java-vmargs
        (list
         "-noverify"
         "-Xmx3G"
         "-XX:+UseG1GC"
         "-XX:+UseStringDeduplication"
         "-Djava.awt.headless=true"
         )
        lsp-java-java-path "/usr/lib/jvm/java-19-openjdk-amd64/bin/java"
        ;; Don't organise imports on save
        lsp-java-save-action-organize-imports nil
	)
  (setq lsp-java-configuration-runtimes '[(:name "JavaSE-19"
                                                 :path "/usr/lib/jvm/java-19-openjdk-amd64"
                                                 :default t)])
  (add-hook 'java-mode-hook 'lsp)) 

;; Man about projectile is https://docs.projectile.mx/projectile/usage.html
;; projectile-remove-known-project
(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :config
  (setq projectile-project-search-path '("~/projects/"))
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map)))

(use-package java-snippets
  :ensure t)

;; Manual on how to activate LSP features https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/
;; Another good man about LSP https://develop.spacemacs.org/layers/+tools/lsp/README.html
(use-package lsp-mode
  :ensure t
  ;; uncomment to enable gopls http debug server
  ;; :custom (lsp-gopls-server-args '("-debug" "127.0.0.1:0"))
  :commands (lsp lsp-deferred)
  :hook
  ((go-mode . lsp-deferred)
   (go-mode . lsp-go-install-save-hooks)
   (go-mode . yas-minor-mode)
   (python-mode . lsp-deferred)
   (python-mode . yas-minor-mode)
   (java-mode . lsp-deferred)
   (java-mode . yas-minor-mode)
   (lsp-mode . lsp-enable-which-key-integration)
   )
  :config (progn
            ;; use flycheck, not flymake
            (setq lsp-prefer-flymake nil)
	      (setq gc-cons-threshold 100000000)
	      (setq read-process-output-max (* 1024 1024)) ;; 1mb
	      (setq lsp-idle-delay 0.500)
	    ;;(setq lsp-trace nil)
	    (setq lsp-print-performance nil)
	    (setq lsp-log-io nil))
  :bind
    (:map lsp-mode-map
          (("\C-\M-g" . lsp-find-implementation)
           ("M-RET" . lsp-execute-code-action)))
  )

(use-package which-key :ensure t :config (which-key-mode))

(use-package lsp-ui
  :ensure t)

(use-package flycheck
  :ensure t)

(use-package use-package-hydra
  :ensure t)

(use-package hydra
  :ensure t)

;; DAP
(use-package dap-mode
  :ensure t
  :custom
  (lsp-enable-dap-auto-configure nil)
  :config
  (dap-mode 1)
  (setq dap-print-io t)
  ;;(setq fit-window-to-buffer-horizontally t)
  ;;(setq window-resize-pixelwise t)
  (require 'dap-hydra)
  (require 'dap-dlv-go)
  (dap-ui-mode 1)
  (dap-tooltip-mode 1)
  :hook
  (dap-stopped . (lambda (arg) (call-interactively #'dap-hydra)))
  )

;; Manual about treemacs is here https://github.com/Alexander-Miller/treemacs
(use-package lsp-treemacs :ensure t)

(use-package origami
  :ensure t
  :demand
  :config
  (define-prefix-command 'origami-mode-map)
  (global-set-key (kbd "C-x C-z") 'origami-mode-map)
  (global-set-key (kbd "<backtab>") 'origami-recursively-toggle-node)
  (global-origami-mode)
  :bind
  (:map origami-mode-map
   ("o" . origami-open-node)
   ("O" . origami-open-node-recursively)
   ("c" . origami-close-node)
   ("C" . origami-close-node-recursively)
   ("t" . origami-toggle-node)
   ("a" . origami-recursively-toggle-node)
   ("R" . origami-open-all-nodes)
   ("M" . origami-close-all-nodes)
   ("v" . origami-show-only-node)
   ("k" . origami-previous-fold)
   ("j" . origami-forward-fold)
   ("x" . origami-reset)))
;;===============================UI==============================================================
(require 'subr-x)
(setq dw/is-termux
      (string-suffix-p "Android" (string-trim (shell-command-to-string "uname -a"))))

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)       ; Give some breathing room
(menu-bar-mode -1)            ; Disable the menu bar

;; Set up the visible bell
(setq visible-bell t)

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time
(setq use-dialog-box nil) ;; Disable dialog boxes since they weren't working in Mac OSX


(set-frame-parameter (selected-frame) 'alpha '(100 . 100))
(add-to-list 'default-frame-alist '(alpha . (100 . 100)))
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(column-number-mode)

;; Enable line numbers for some modes
(dolist (mode '(text-mode-hook
                prog-mode-hook
                conf-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 1))))

;; Override some modes which derive from the above
(dolist (mode '(org-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(use-package  spacegray-theme
  :ensure t)
(use-package doom-themes
  :ensure t)


(load-theme 'doom-palenight t)
(doom-themes-visual-bell-config)

(set-face-attribute 'default nil
                    :font "JetBrains Mono"
                    :weight 'light
                    :height 130)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil
                    :font "JetBrains Mono"
                    :weight 'light
                    :height 260)

(use-package default-text-scale
  :ensure t
  :config
  (default-text-scale-mode)
  )
;;==========================Auto-Save============================================================

(use-package diminish
  :ensure t)

(use-package super-save
  :ensure t
  :config
  (super-save-mode +1)
  (diminish 'super-save-mode)
  (setq super-save-auto-save-when-idle t)  
  )

(setq global-auto-revert-non-file-buffers t)
(global-auto-revert-mode 1)
;;==========================Other================================================================
(set-default-coding-systems 'utf-8)
(server-start)
(setq inhibit-startup-message t)
(set-default 'truncate-lines t)
(setq debug-on-error t)
;;==========================Chats================================================================
(use-package visual-fill-column
  :ensure t)
(use-package rainbow-identifiers
  :ensure t)
(use-package telega
  :ensure t
  :commands (telega)
  :config
  (setq telega-server-libs-prefix "/usr/local")
  :defer t)
;;==========================Chats================================================================

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(appt-display-format 'echo)
 '(custom-safe-themes
   '("631c52620e2953e744f2b56d102eae503017047fb43d65ce028e88ef5846ea3b" "5f128efd37c6a87cd4ad8e8b7f2afaba425425524a68133ac0efd87291d05874" "2e05569868dc11a52b08926b4c1a27da77580daa9321773d92822f7a639956ce" default))
 '(default-input-method "russian-computer")
 '(exwm-randr-screen-change-hook nil)
 '(global-company-mode t)
 '(ispell-dictionary nil)
 '(ntlm-compatibility-level 5)
 '(package-selected-packages
   '(magit exwm-config dap-java which-key projectile java-snippets lsp-java ox-hugo excorporate openwith org-alert exwm elfeed-org emms elfeed company mu4e-alert counsel swiper ivy mu4e use-package-hydra use-package dap-mode lsp-ui lsp-mode go-autocomplete yasnippet multi-compile gotest go-scratch go-rename go-guru go-eldoc go-direx flycheck company-go))
 '(telega-server-libs-prefix "/usr/local"))
;;==============================================Mail===================================================================

;; (setq dw/mail-enabled nil)

 (use-package excorporate
   :ensure t
   :config
   (setq org-agenda-include-diary t)
   (setq excorporate-configuration (quote ("a.akselrod" . "https://ews.tcsbank.ru/EWS/Exchange.asmx")))
   (excorporate-diary-enable)
   )

;; (use-package mu4e
;;   :config

;;   ;; Load org-mode integration
;;   (require 'org-mu4e)

;;   ;; Refresh mail using isync every 10 minutes
;;   (setq mu4e-update-interval (* 10 60))
;;   (setq mu4e-get-mail-command "mbsync -a")
;;   (setq mu4e-maildir "~/.mail/goods")
;;       ;; Make sure that moving a message (like to Trash) causes the
;;     ;; message to get a new file name.  This helps to avoid the
;;     ;; dreaded "UID is N beyond highest assigned" error.
;;     ;; See this link for more info: https://stackoverflow.com/a/43461973
;;   (setq mu4e-change-filenames-when-moving t)
;;   ;; Display options
;;   (setq mu4e-view-show-images t)
;;   (setq mu4e-view-show-addresses 't)

;;   ;; Composing mail
;;   (setq mu4e-compose-dont-reply-to-self t)
;;   ;; Use Ivy for mu4e completions (maildir folders, etc)
;;   (setq mu4e-completing-read-function #'ivy-completing-read)
;;   ;; Use mu4e for sending e-mail
;;   (setq mail-user-agent 'mu4e-user-agent
;;         message-send-mail-function 'smtpmail-send-it
;;         smtpmail-smtp-server "mail.sbermegamarket.ru"
;;         smtpmail-smtp-service 587
;;         smtpmail-stream-type  'starttls)
;;   (require 'mu4e-icalendar)
;;   (mu4e-icalendar-setup)
;;   (require 'org-agenda)
;;   (setq gnus-icalendar-org-capture-file "~/work/calendar.org")
;;   (setq gnus-icalendar-org-capture-headline '("Unprocessed"))
;;   (gnus-icalendar-org-setup)
;;   :hook
;;   (mu4e-compose-pre . (lambda () 
;; 			(setq user-mail-address "aleksandr.akselrod@sbermegamarket.ru")))
;;   )

;;==========================Ivy=======================================================================
(use-package swiper
  :ensure t)
(use-package counsel
  :ensure t)
(use-package ivy
  :ensure t
  :config
  (ivy-mode)
  (setq ivy-use-virtual-buffers t)
  :init
  (global-set-key "\C-s" 'swiper)
  (global-set-key (kbd "C-c C-r") 'ivy-resume)
  (global-set-key (kbd "<f6>") 'ivy-resume)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "<f1> f") 'counsel-describe-function)
  (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
  (global-set-key (kbd "<f1> o") 'counsel-describe-symbol)
  (global-set-key (kbd "<f1> l") 'counsel-find-library)
  (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
  (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
  (global-set-key (kbd "C-c g") 'counsel-git)
  (global-set-key (kbd "C-c j") 'counsel-git-grep)
  (global-set-key (kbd "C-c k") 'counsel-ag)
  (global-set-key (kbd "C-x l") 'counsel-locate)
  (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history))
;;=======================================m4u alerts====================================================================
;; (use-package mu4e-alert
;;   :ensure t
;;   :hook
;;   ((after-init . mu4e-alert-enable-mode-line-display))
;;   :after mu4e
;;   :config
;;   ;; Show unread emails from all inboxes
;;   ;;(setq mu4e-alert-interesting-mail-query dw/mu4e-inbox-query)

;;     ;; Show notifications for mails already notified
;;   (setq mu4e-alert-notify-repeated-mails nil)

;;   (mu4e-alert-enable-notifications))
;;======================================OrgMode=======================================================================
(use-package org-alert
  :ensure t
  :custom (alert-default-style 'message)
  :config
  (setq org-alert-interval 300
      org-alert-notify-cutoff 10
      org-alert-notify-after-event-cutoff 10)
  (org-alert-enable))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)))

(setq org-refile-targets '(
			  (nil :maxlevel . 3)
			  )
      )

(add-hook 'org-agenda-mode-hook (lambda ()
				  (setq org-refile-targets '(
							     (org-agenda-files :maxlevel . 3)
							     )
					)
				  )
	  )

(add-hook 'org-mode-hook 'org-indent-mode)
(setq org-directory
      "~/work")
(global-set-key (kbd "C-c a") 'org-agenda)

(defun dw/org-path (path)
  (expand-file-name path org-directory))

(setq org-agenda-files '("~/work"))

(setq org-default-notes-file (dw/org-path "notes.org"))

;;- =TODO= - A task that should be done at some point
;;- =NEXT= - This task should be done next (in the Getting Things Done sense)
;;- =BACK= - A task in the backlog to be done some day but not now
;;- =WAIT= - Waiting for someone else to be actionable again
;;- =DONE= - It's done!
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
	(sequence "|" "WAIT(w)" "BACK(b)")))

;; TODO: org-todo-keyword-faces
(setq org-todo-keyword-faces
      '(("NEXT" . (:foreground "orange red" :weight bold))
	("WAIT" . (:foreground "HotPink2" :weight bold))
	("BACK" . (:foreground "MediumPurple3" :weight bold))))

;; Configure common tags
(setq org-tag-alist
      '((:startgroup)
					; Put mutually exclusive tags here
	(:endgroup)
	("@arch" . ?a)
	("followup" . ?f)))

;;================================Agenda=================================
(setq org-agenda-window-setup 'current-window)
(setq org-agenda-span 'day)
(setq org-agenda-start-with-log-mode t)

;; Make done tasks show up in the agenda log
(setq org-log-done 'time)
(setq org-log-into-drawer t)

(setq org-columns-default-format "%20CATEGORY(Category) %65ITEM(Task) %TODO %6Effort(Estim){:}  %6CLOCKSUM(Clock) %TAGS")

(setq org-agenda-custom-commands
      `(("d" "Dashboard"
         ((agenda "" ((org-deadline-warning-days 7)))
          (tags-todo "+PRIORITY=\"A\""
                     ((org-agenda-overriding-header "High Priority")))
          (tags-todo "+followup" ((org-agenda-overriding-header "Needs Follow Up")))
          (todo "NEXT"
                ((org-agenda-overriding-header "Next Actions")
                 (org-agenda-max-todos nil)))
          (todo "TODO"
                ((org-agenda-overriding-header "Unprocessed Inbox Tasks")
                 (org-agenda-files '(,(dw/org-path "Inbox.org")))
                 (org-agenda-text-search-extra-files nil)))))

        ("n" "Next Tasks"
         ((agenda "" ((org-deadline-warning-days 7)))
          (todo "NEXT"
                ((org-agenda-overriding-header "Next Tasks")))))

        ;; Low-effort next actions
        ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
         ((org-agenda-overriding-header "Low Effort Tasks")
          (org-agenda-max-todos 20)
          (org-agenda-files org-agenda-files)))))
;;================================Agenda=================================
(add-hook 'org-timer-set-hook #'org-clock-in)
;;==============================Templates================================
(global-set-key (kbd "C-c c") 'org-capture)
(defun dw/get-todays-journal-file-name ()
  "Gets the journal file name for today's date"
  (interactive)
  (let* ((journal-file-name
          (expand-file-name
           (format-time-string "%Y/%Y-%2m-%B.org")
           (dw/org-path "Journal/")))
         (journal-year-dir (file-name-directory journal-file-name)))
    (if (not (file-directory-p journal-year-dir))
        (make-directory journal-year-dir))
    journal-file-name))

(defun dw/on-org-capture ()
  ;; Don't show the confirmation header text
  (setq header-line-format nil)

  ;; Control how some buffers are handled
  (let ((template (org-capture-get :key t)))
    (pcase template
      ("jj" (delete-other-windows)))))

(add-hook 'org-capture-mode-hook 'dw/on-org-capture)

(setq org-capture-templates
      `(("t" "Tasks")
	("tt" "Task" entry (file ,(dw/org-path "Inbox.org"))
         "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)
	("ts" "Clocked Entry Subtask" entry (clock)
         "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)

	("j" "Journal Entries")
	("je" "General Entry" entry
         (file+olp+datetree ,(dw/org-path "Journal.org"))
         "\n* %<%I:%M %p> - %^{Title} \n\n%?\n\n"
         :tree-type week
         :clock-in :clock-resume
         :empty-lines 1)
	("jt" "Task Entry" entry
         (file+olp+datetree ,(dw/org-path "Journal.org"))
         "\n* %<%I:%M %p> - Task Notes: %a\n\n%?\n\n"
         :tree-type week
         :clock-in :clock-resume
         :empty-lines 1)
	("#" "used by gnus-icalendar-org" entry
	 (file+olp "~/work/calendar.org" "Calendar")
	 "%i" :immediate-finish t)
	("jj" "Journal" entry
         (file+olp+datetree ,(dw/org-path "Journal.org"))
         "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
         :tree-type week
         :clock-in :clock-resume
         :empty-lines 1)))
;;==============================Templates================================
;;=============================Russian Keyboard==========================
(defun do-reverse-input-method (input-metod)
  (if (and input-method (symbolp input-method))
      (setq input-method (symbol-name input-method)))
  (let ((current current-input-method)
        (modifiers '(nil (control) (meta) (control meta))))
    (when input-method
      (activate-input-method input-method))
    (when (and current-input-method quail-keyboard-layout)
      (dolist (map (cdr (quail-map)))
        (let* ((to (car map))
               (from (quail-get-translation
                      (cadr map) (char-to-string to) 1)))
          (when (and (characterp from) (characterp to))
            (dolist (mod modifiers)
              (define-key local-function-key-map
                (vector (append mod (list from)))
                (vector (append mod (list to)))))))))
    (when input-method
      (activate-input-method current))))

(defun reverse-input-method (input-method)
  "Build the reverse mapping of single letters from INPUT-METHOD."
  (interactive
   (list (read-input-method-name "Use input method (default current): ")))
  (do-reverse-input-method input-method))

(setq input-method "russian-computer")
(do-reverse-input-method "russian-computer")
;;====================================RSS==============================
;; Key Bindings

;; b: Open the article in the browser
;; G: Fetch feed updates from the servers
;; s: Update the search filter
;; c: Clear the search filter
;; r Mark the entry as read
;; u: Mark the entry as unread
;; g: Refresh view of the feed listing (remove unread items)
;; q: Quit the browser

(use-package elfeed-org
  :ensure t)

(use-package elfeed
  :ensure t
  :config
  (elfeed-org)
  (setq rmh-elfeed-org-files (list "/home/alex/work/rss.org"))
  :bind
  ("C-x w" . elfeed)
  )
;;================================EXWM===================================
 (setq display-time-day-and-date t)
 (display-time-mode 1)

 (defun alex/exwm-update-class ()
   (exwm-workspace-rename-buffer exwm-class-name))

(require 'em-tramp)
(setq eshell-prefer-lisp-functions t)
(setq eshell-prefer-lisp-variables t)
;; alias sudo 'eshell/sudo $*'
(setq password-cache t) ; enable password caching
(setq password-cache-expiry 3600) ; for one hour (time in secs)

(defun alex/configure-window-by-class ()
  (interactive)
  (pcase exwm-class-name
    ("firefox" (exwm-workspace-move-window 1))
    ("Sol" (exwm-workspace-move-window 3))
    ("mpv" (exwm-floating-toggle-floating)
           (exwm-layout-toggle-mode-line))))

(defun alex/exwm-update-class ()
  (exwm-workspace-rename-buffer exwm-class-name))

(defun alex/exwm-update-title ()
  (pcase exwm-class-name
    ("Opera" (exwm-workspace-rename-buffer (format "Opera: %s" exwm-title)))))

(use-package exwm
  :ensure t
  :config
  ;; Set the default number of workspaces
   (setq exwm-workspace-number 5)

  ;; ;; When window "class" updates, use it to set the buffer name
   (add-hook 'exwm-update-class-hook #'alex/exwm-update-class)
   (add-hook 'exwm-update-title-hook #'alex/exwm-update-title)
   (add-hook 'exwm-manage-finish-hook #'alex/configure-window-by-class)

  ;; ;; Rebind CapsLock to Ctrl
  ;; (start-process-shell-command "xmodmap" nil "xmodmap ~/.emacs.d/exwm/Xmodmap")

  ;; ;; Set the screen resolution (update this to be the correct resolution for your screen!)
   (require 'exwm-randr)
   (exwm-randr-enable)

   (defun dw/on-screen-changed ()
     (interactive)
     (app/detect-scale)
     (app/rerun-gtk-apps)
     )

   (setq mouse-autoselect-window t
   	focus-follows-mouse t)
  
  ;; ;; Load the system tray before exwm-init
  ;; ;; (require 'exwm-systemtray)
  ;; ;; (exwm-systemtray-enable)

     ;; using xim input
   (setenv "GTK_IM_MODULE" "xim")
   (setenv "QT_IM_MODULE" "xim")
   (setenv "XMODIFIERS" "@im=exwm-xim")
   (setenv "CLUTTER_IM_MODULE" "xim")
   (setenv "GDK_DPI_SCALE" "-1")
   (setenv "EDITOR" "emacsclient")
   
   (require 'exwm-xim)
   (exwm-xim-enable)
  
   (push ?\C-\\ exwm-input-prefix-keys)   ;; use Ctrl + \ to switch input method
  ;; These keys should always pass through to Emacs
   (setq exwm-input-prefix-keys
    '(?\C-x
      ?\C-u
      ?\C-h
      ?\C-\\
      ?\M-x
      ?\M-`
      ?\M-&
      ?\M-:
      ?\C-\M-j  ;; Buffer list
      ?\C-\M-n  ;; Next workspace
      ?\C-\;))  ;; Ctrl+Space
   
   ;; Ctrl+Q will enable the next key to be sent directly
   (define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)

   ;; ;; Set up global key bindings.  These always work, no matter the input state!
   ;; ;; Keep in mind that changing this list after EXWM initializes has no effect.
   (setq exwm-input-global-keys
         `(
           ;; Reset to line-mode (C-c C-k switches to char-mode via exwm-input-release-keyboard)
           ([?\s-r] . exwm-reset)

           ;; Move between windows
           ([s-left] . windmove-left)
           ([s-right] . windmove-right)
           ([s-up] . windmove-up)
           ([s-down] . windmove-down)
           ;; Launch applications via shell command
           ([?\s-&] . (lambda (command)
                        (interactive (list (read-shell-command "$ ")))
                        (start-process-shell-command command nil command)))

           ;; Switch workspace
           ([?\s-w] . exwm-workspace-switch)
           ([?\s-`] . (lambda () (interactive) (exwm-workspace-switch-create 0)))

           ;; 's-N': Switch to certain workspace with Super (Win) plus a number key (0 - 9)
           ,@(mapcar (lambda (i)
                       `(,(kbd (format "s-%d" i)) .
                         (lambda ()
                           (interactive)
                           (exwm-workspace-switch-create ,i))))
                     (number-sequence 0 9))))

   (defun exwm/run-in-background (command)
     (let ((command-parts (split-string command "[ ]+")))
       (apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))

   (defun dw/exwm-init-hook ()
     (app/start-panel)
     (exwm/run-in-background "nm-applet")
     (exwm/run-in-background "blueman-applet")
     (exwm/run-in-background "indicator-sound-switcher")
     
     (dw/on-screen-changed)
     )
   (add-hook 'exwm-init-hook #'dw/exwm-init-hook)
   (exwm-enable)
   )

;;================================Hugo===================================
(use-package ox-hugo
  :ensure t)
(setq org-hugo-base-dir "/home/alex/work/org-share")
;;================================EAF====================================
(use-package eaf
  :load-path "~/.emacs.d/site-lisp/emacs-application-framework"
;;  :custom
					; See https://github.com/emacs-eaf/emacs-application-framework/wiki/Customization
  ;;(eaf-browser-continue-where-left-off t)
  ;;(eaf-browser-enable-adblocker t)
  ;;(browse-url-browser-function 'eaf-open-browser)
  :config
  (require 'eaf-pdf-viewer)
;;  (require 'eaf-browser)
;;  (require 'eaf-camera)
;;  (defalias 'browse-web #'eaf-open-browser)
  (eaf-bind-key scroll_up "C-n" eaf-pdf-viewer-keybinding)
  (eaf-bind-key scroll_down "C-p" eaf-pdf-viewer-keybinding)
;;  (eaf-bind-key take_photo "p" eaf-camera-keybinding)
  ;;(eaf-bind-key nil "M-q" eaf-browser-keybinding)
  ) ;; unbind, see more in the Wiki
;;================================Git====================================
(use-package magit
  :ensure t)
