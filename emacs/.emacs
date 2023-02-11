(use-package package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(setq toggle-truncate-lines t)

;;=======================Polybar===================================================
(defun dw/polybar-exwm-workspace ()
  (pcase exwm-workspace-current-index
    (0 "")
    (1 "")
    (2 "")
    (3 "")
    (4 "")))
(defun dw/polybar-exwm-workspace-path ()
  (let ((workspace-path (frame-parameter nil 'bufler-workspace-path-formatted)))
    (if workspace-path
        (substring-no-properties workspace-path)
      "")))

(defun dw/polybar-mail-count (max-count)
  (if (and dw/mail-enabled dw/mu4e-inbox-query)
    (let* ((mail-count (shell-command-to-string
                         (format "mu find --nocolor -n %s \"%s\" | wc -l" max-count dw/mu4e-inbox-query))))
      (format " %s" (string-trim mail-count)))
    ""))

(defun dw/telega-normalize-name (chat-name)
  (let* ((trimmed-name (string-trim-left (string-trim-right chat-name "}") "◀{"))
         (first-name (nth 0 (split-string trimmed-name " "))))
    first-name))

(defun dw/propertized-to-polybar (buffer-name)
  (if-let* ((text (substring-no-properties buffer-name))
            (fg-face (get-text-property 0 'face buffer-name))
            (fg-color (face-attribute fg-face :foreground)))
    (format "%%{F%s}%s%%{F-}" fg-color (dw/telega-normalize-name text))
    text))

(defun dw/polybar-telegram-chats ()
  (if (> (length tracking-buffers) 0)
    (format " %s" (string-join (mapcar 'dw/propertized-to-polybar tracking-buffers) ", "))
    ""))

(defvar efs/polybar-process nil
  "Holds the process of the running Polybar instance, if any")

(defun efs/kill-panel ()
  (interactive)
  (when efs/polybar-process
    (ignore-errors
      (kill-process efs/polybar-process)))
  (setq efs/polybar-process nil))

(defun efs/start-panel ()
  (interactive)
  (efs/kill-panel)
  (setq efs/polybar-process (start-process-shell-command "polybar" nil "polybar panel")))

;;===============================Development=====================================================
;; Company mode
(use-package company
  :init
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1))

(use-package flycheck-golangci-lint
	     :ensure t)

(use-package yasnippet
  :ensure t)

(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))

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
   )
  :config (progn
            ;; use flycheck, not flymake
            (setq lsp-prefer-flymake nil)
	    ;;(setq lsp-trace nil)
	    (setq lsp-print-performance nil)
	    (setq lsp-log-io nil))
  )

(use-package lsp-ui
  :ensure t)

(use-package flycheck
  :ensure t)

(use-package use-package-hydra
  :ensure t)

;; DAP
(use-package dap-mode
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
;;===============================Development=====================================================
;;===============================UI==============================================================
(require 'subr-x)
(setq dw/is-termux
      (string-suffix-p "Android" (string-trim (shell-command-to-string "uname -a"))))

(unless dw/is-termux
  (scroll-bar-mode -1)        ; Disable visible scrollbar
  (tool-bar-mode -1)          ; Disable the toolbar
  (tooltip-mode -1)           ; Disable tooltips
  (set-fringe-mode 10))       ; Give some breathing room

(menu-bar-mode -1)            ; Disable the menu bar

;; Set up the visible bell
(setq visible-bell t)

(unless dw/is-termux
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
  (setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
  (setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
  (setq scroll-step 1) ;; keyboard scroll one line at a time
  (setq use-dialog-box nil)) ;; Disable dialog boxes since they weren't working in Mac OSX

(unless dw/is-termux
  (set-frame-parameter (selected-frame) 'alpha '(100 . 100))
  (add-to-list 'default-frame-alist '(alpha . (100 . 100)))
  (set-frame-parameter (selected-frame) 'fullscreen 'maximized)
  (add-to-list 'default-frame-alist '(fullscreen . maximized)))

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

(unless dw/is-termux
  (load-theme 'doom-palenight t)
  (doom-themes-visual-bell-config))

(pcase system-type
  ((or 'gnu/linux 'windows-nt 'cygwin)
   (set-face-attribute 'default nil
                       :font "JetBrains Mono"
                       :weight 'light
                       :height 130))
  ('darwin (set-face-attribute 'default nil :font "Fira Mono" :height 130)))

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil
                    :font "JetBrains Mono"
                    :weight 'light
                    :height 130)

(use-package default-text-scale
  :ensure t
  :config
  (default-text-scale-mode)
  )

;;===============================UI==============================================================
;;==========================Auto-Save============================================================
(use-package super-save
  :ensure t
  :config
  (super-save-mode +1)
  (diminish 'super-save-mode)
  (setq super-save-auto-save-when-idle t)  
  )

(setq global-auto-revert-non-file-buffers t)
(global-auto-revert-mode 1)
;;==========================Auto-Save============================================================
;;==========================Other================================================================
(set-default-coding-systems 'utf-8)
(server-start)
(setq inhibit-startup-message t)
;;(setq debug-on-error t)
;;==========================Other================================================================
;;==========================Chats================================================================
(use-package telega
  :ensure t
  )
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
 '(custom-safe-themes
   '("631c52620e2953e744f2b56d102eae503017047fb43d65ce028e88ef5846ea3b" "5f128efd37c6a87cd4ad8e8b7f2afaba425425524a68133ac0efd87291d05874" "2e05569868dc11a52b08926b4c1a27da77580daa9321773d92822f7a639956ce" default))
 '(ispell-dictionary nil)
 '(package-selected-packages
   '(excorporate openwith org-alert exwm elfeed-org emms elfeed company mu4e-alert counsel swiper ivy mu4e telega use-package-hydra use-package dap-mode lsp-ui lsp-mode go-autocomplete yasnippet multi-compile gotest go-scratch go-rename go-guru go-eldoc go-direx flycheck company-go)))
;;==============================================Mail===================================================================

(setq dw/mail-enabled nil)

(use-package excorporate
  :ensure t
  :config
  (setq org-agenda-include-diary t)
  (setq excorporate-configuration (quote ("aleksandr.akselrod@sbermegamarket.ru" . "https://10.30.48.65/EWS/Exchange.asmx")))
  (excorporate-diary-enable)
  )

(use-package mu4e
  :config

  ;; Load org-mode integration
  (require 'org-mu4e)

  ;; Refresh mail using isync every 10 minutes
  (setq mu4e-update-interval (* 10 60))
  (setq mu4e-get-mail-command "mbsync -a")
  (setq mu4e-maildir "~/.mail")
      ;; Make sure that moving a message (like to Trash) causes the
    ;; message to get a new file name.  This helps to avoid the
    ;; dreaded "UID is N beyond highest assigned" error.
    ;; See this link for more info: https://stackoverflow.com/a/43461973
  (setq mu4e-change-filenames-when-moving t)
  ;; Display options
  (setq mu4e-view-show-images t)
  (setq mu4e-view-show-addresses 't)

  ;; Composing mail
  (setq mu4e-compose-dont-reply-to-self t)
  ;; Use Ivy for mu4e completions (maildir folders, etc)
  (setq mu4e-completing-read-function #'ivy-completing-read)
  ;; Use mu4e for sending e-mail
  (setq mail-user-agent 'mu4e-user-agent
        message-send-mail-function 'smtpmail-send-it
        smtpmail-smtp-server "mail.sbermegamarket.ru"
        smtpmail-smtp-service 587
        smtpmail-stream-type  'starttls)
  (require 'mu4e-icalendar)
  (mu4e-icalendar-setup)
  (require 'org-agenda)
  (setq gnus-icalendar-org-capture-file "~/work/calendar.org")
  (setq gnus-icalendar-org-capture-headline '("Unprocessed"))
  (gnus-icalendar-org-setup)
  :hook
  (mu4e-compose-pre . (lambda () 
			(setq user-mail-address "aleksandr.akselrod@sbermegamarket.ru")))
  )

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
;;  (global-set-key (kbd "M-x") 'counsel-M-x)
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
(use-package mu4e-alert
  :ensure t
  :hook
  ((after-init . mu4e-alert-enable-mode-line-display))
  :after mu4e
  :config
  ;; Show unread emails from all inboxes
  ;;(setq mu4e-alert-interesting-mail-query dw/mu4e-inbox-query)

    ;; Show notifications for mails already notified
  (setq mu4e-alert-notify-repeated-mails nil)

  (mu4e-alert-enable-notifications))
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

(defun efs/exwm-update-class ()
  (exwm-workspace-rename-buffer exwm-class-name))

(use-package exwm
  :config
  ;; Set the default number of workspaces
  (setq exwm-workspace-number 5)

  ;; When window "class" updates, use it to set the buffer name
  (add-hook 'exwm-update-class-hook #'efs/exwm-update-class)

  ;; Rebind CapsLock to Ctrl
  (start-process-shell-command "xmodmap" nil "xmodmap ~/.emacs.d/exwm/Xmodmap")

  ;; Set the screen resolution (update this to be the correct resolution for your screen!)
  (require 'exwm-randr)
  (exwm-randr-enable)
  ;; (start-process-shell-command "xrandr" nil "xrandr --output Virtual-1 --primary --mode 2048x1152 --pos 0x0 --rotate normal")

  (setq exwm-randr-workspace-monitor-plist '(2 "HDMI-1" 3 "HDMI-1" 4 "DP-3"))
  (setq exwm-workspace-warp-cursor t)
  (setq mouse-autoselect-window t
	focus-follows-mouse t)
  
  ;; Load the system tray before exwm-init
  ;; (require 'exwm-systemtray)
  ;; (exwm-systemtray-enable)

    ;; using xim input
  (setenv "GTK_IM_MODULE" "xim")
  (setenv "QT_IM_MODULE" "xim")
  (setenv "XMODIFIERS" "@im=exwm-xim")
  (setenv "CLUTTER_IM_MODULE" "xim")
  
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

  ;; Set up global key bindings.  These always work, no matter the input state!
  ;; Keep in mind that changing this list after EXWM initializes has no effect.
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
    (efs/start-panel)
    (exwm/run-in-background "nm-applet")
    (exwm/run-in-background "blueman-applet")
    (exwm/run-in-background "indicator-sound-switcher")
    )
  (add-hook 'exwm-init-hook #'dw/exwm-init-hook)
  (exwm-enable)
  )
