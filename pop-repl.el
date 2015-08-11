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
       :height .3
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
