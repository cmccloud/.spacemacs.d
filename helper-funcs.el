;; repl map
(defvar mcc-repl-table (make-hash-table)
  "Associates major modes to repl enviroments.
Uses 'eql' as comparator on key-lookup.")

;; generic
(defun mcc-pop-repl ()
  "Uses the current major mode to call for a
REPL using 'mcc-repl-table', if one has been specified."
  (interactive)
  (funcall (gethash major-mode mcc-repl-table (lambda ()))))

;; cider repl
(defun mcc--pop-cider-repl ()
  "Attempts to locate and display as a popup window first active
instance of cider repl."
  (when (get-buffer "*cider-repl localhost*")
    (if (get-buffer-window "*cider-repl localhost*" t)
        (progn
          (popwin:close-popup-window t)
          ;; only if popwin failed
          (-when-let (retry (get-buffer-window "*cider-repl localhost*" t))
            (delete-window retry)))
      (popwin:popup-buffer
       "*cider-repl localhost*"
       :position 'bottom
       :height .2
       :stick t
       :tail t
       :noselect t
       :dedicated t))))

;; ielm repl
(defun mcc--pop-ielm-repl ()
  "Displays as a popup window ielm instance. Uses existing buffer if found."
  (let ((instance (get-buffer "*ielm*"))
        (options '("*ielm*" :position bottom :height .4 :stick t :tail t :noselect nil :dedicated t))
        (buf (current-buffer)))
    (cond ((null instance)
           (ielm)
           (switch-to-buffer buf)
           (apply 'popwin:popup-buffer options))
          ((get-buffer-window "*ielm*" t)
           (popwin:close-popup-window t)
           ;; only if popwin failed
           (-when-let (retry (get-buffer-window "*ielm*" t))
               (delete-window retry)))
          (t (apply 'popwin:popup-buffer options)))))

;; Associations
(puthash 'inferior-emacs-lisp-mode #'mcc--pop-ielm-repl mcc-repl-table)
(puthash 'emacs-lisp-mode #'mcc--pop-ielm-repl mcc-repl-table)
(puthash 'clojure-mode #'mcc--pop-cider-repl mcc-repl-table)
(puthash 'cider-repl-mode #'mcc--pop-cider-repl mcc-repl-table)

(defun mcc-pop-win-push (&rest windows)
  "Adds window to popwin:special-display-config with default settings"
  (let ((settings '(:dedicated t :position bottom :stick t :noselect nil :height 0.4)))
    (while windows
      (push (cons (pop windows) settings) popwin:special-display-config))))

;; Helm Describe Bindings Helpers
(defun mcc-helm-desc-major-mode-bindings ()
  (interactive)
  (helm-descbinds " m"))

(defun mcc-filter-descbinds (next candidates)
  "next :: function in advice chain, candidates :: list of (key . command)"
  (->> candidates
       (--filter (not (member (cdr it) helm-descbinds-strings-to-ignore)))
       (funcall next)))

(defun helm-find-contrib-file ()
  "Runs helm find files on spacemacs contrib folder"
  (interactive)
  (helm-find-files-1 (expand-file-name (concat user-emacs-directory "contrib/"))))

(defun helm-find-spacemacs-file ()
  "Runs helm find files on spacemacs directory"
  (interactive)
  (helm-find-files-1 (expand-file-name (concat user-emacs-directory "spacemacs/"))))

(defun mcc-instrument-with-edebug ()
  "Instruments top level defun under point."
  (interactive)
  (eval-defun t))
