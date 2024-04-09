(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
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
 '(global-company-mode t)
 '(ispell-dictionary nil)
 '(ntlm-compatibility-level 5)
 '(org-agenda-files
   '("/home/alex/work/wiki/20240111092255-tmm.org" "/home/alex/work/wiki/20240111141520-self_service.org" "/home/alex/work/wiki/20240112101039-23_q1_goals.org" "/home/alex/work/wiki/20240114134832-service_rituals.org" "/home/alex/work/wiki/20240116104650-all_hands.org" "/home/alex/work/wiki/20240116104851-tmsg_all_hands_q3_23.org" "/home/alex/work/wiki/20240116105039-self_service_all_hands_q1_24.org" "/home/alex/work/wiki/20240116162800-anti_siebel.org" "/home/alex/work/wiki/20240117171530-tl_all_hands_q1_24.org" "/home/alex/work/wiki/20240118103113-direct_reports.org" "/home/alex/work/wiki/20240123164952-levchuk.org" "/home/alex/work/wiki/20240125134335-siebel_help_presa.org" "/home/alex/work/wiki/20240126113419-tmsg_all_hands_q1_24.org" "/home/alex/work/wiki/20240126115515-team_api.org" "/home/alex/work/wiki/20240126141813-rfc_kafka_prima.org" "/home/alex/work/wiki/20240127104237-payment_account.org" "/home/alex/work/wiki/20240130094118-skogorev.org" "/home/alex/work/wiki/20240201104302-robocomm.org" "/home/alex/work/wiki/20240205110405-vacancies.org" "/home/alex/work/wiki/20240206153800-korneev.org" "/home/alex/work/wiki/20240206170010-news.org" "/home/alex/work/wiki/20240208090517-enikeev.org" "/home/alex/work/wiki/20240208093114-client_360.org" "/home/alex/work/wiki/20240208102812-loginov.org" "/home/alex/work/wiki/20240208180015-problem_service.org" "/home/alex/work/wiki/20240213093625-goals_q1_24.org" "/home/alex/work/wiki/20240215102742-goals_q2_24.org" "/home/alex/work/wiki/20240219123305-org_structure.org" "/home/alex/work/wiki/20240220094902-app_portfolio_management.org" "/home/alex/work/wiki/20240304095041-chat_bots.org" "/home/alex/work/wiki/20240304110744-2024_q2_conference.org" "/home/alex/work/wiki/20240304130716-telegram_forbidden.org" "/home/alex/work/wiki/20240313112004-it_weekly.org" "/home/alex/work/wiki/20240315085119-family.org" "/home/alex/work/wiki/20240315085846-team_setup.org" "/home/alex/work/wiki/20240315163443-q2_goals.org" "/home/alex/work/wiki/20240318091905-backlog_goals.org" "/home/alex/work/wiki/20240319101745-meetings.org" "/home/alex/work/wiki/20240319121959-proc_stability.org" "/home/alex/work/wiki/20240319182950-tsay.org" "/home/alex/work/wiki/20240321182502-notes.org" "/home/alex/work/wiki/20240325135617-efficency.org" "/home/alex/work/wiki/20240328111016-teletype.org" "/home/alex/work/wiki/20240329125407-efficiency.org" "/home/alex/work/wiki/20240403155517-rosbank.org" "/home/alex/work/wiki/20240406161827-.org" "/home/alex/work/Inbox.org"))
 '(package-selected-packages
   '(plain-theme cmake-mode org-roam org-dotemacs ox-beamer org-beamer emms-setup org-present magit exwm-config dap-java which-key projectile java-snippets lsp-java ox-hugo excorporate openwith org-alert exwm elfeed-org emms elfeed company mu4e-alert counsel swiper ivy mu4e use-package-hydra use-package dap-mode lsp-ui lsp-mode go-autocomplete yasnippet multi-compile gotest go-scratch go-rename go-guru go-eldoc go-direx flycheck company-go))
 '(telega-server-libs-prefix "/usr/local")
 '(w3m-default-display-inline-images t))

(add-to-list 'exec-path "/home/alex/.local/bin")

(setenv JIRA_USER "a.akselrod")
(setenv JIRA_PASSWORD "Ospappt6")
(use-package org-dotemacs
   :ensure t
   :config
   (org-dotemacs-load-default))
