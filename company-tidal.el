;;; company-tidal --- Company backend for Tidal Cycles.
;;
;;; Commentary:
;;
;; A `company` autocompletion backend for Tidal Cycles.
;;
;; FIXME: The completions are from previous run. This is a bug, maybe
;; from parellel programming.
;;
;; TODO: Autocomplete d[1-8]. They come from Tidal but company doesn't
;; show that many. Maybe sort them to the top?
;;
;; TODO: Keep the output only when completing, not when typing stuff
;; directly to Tidal comint window. Maybe make the hook conditional on
;; the mode?
;;
;; TODO: Autocomplete samples. They need to be asked from SuperDirt or
;; SuperCollider
;;
;; Work is delegated to the Haskell GHCi actually.
;; 
;;; Code:

(require 'cl-lib)

(defun keep-process-output (output)
  "A filter function for keeping process OUTPUT."
  (setq tidal-kept-output output))

;; Change settings of the comint buffer. Where does this go? Somewhere
;; in tidal.el maybe?
(with-current-buffer tidal-buffer
  (setq comint-prompt-regexp "tidal\> ")
  (setq comint-use-prompt-regexp t)
  (add-hook 'comint-output-filter-functions 'keep-process-output t t))

(defun tidal-get-completions (arg)
  "Get completions starting with ARG from Tidal process.

Assuming the filter is in place."
  ;; This send message to Tidal, and a filter captures output in
  ;; tidal-process-output

  (message (concat "Getting completions for " arg))

  (comint-send-string tidal-buffer
                      (concat ":complete repl " "\"" arg "\"" "\n"))

  (seq-remove
   (lambda (item) (equal item "tidal> "))
   (seq-drop (seq-map
              (lambda (item) (replace-regexp-in-string "\"" "" item))
              (split-string tidal-kept-output "\n")) 1)))

;; Testing it
(tidal-get-completions "so")


(defun company-tidal-backend (command &optional arg &rest ignored)
  "Backend COMMAND for Tidal Cycles items starting with ARG."
  (interactive (list 'interactive))

  (cl-case command
    (interactive (company-begin-backend 'company-tidal-backend))
    (prefix (and (eq major-mode 'tidal-mode)
                 (company-grab-symbol)))
    (candidates
     (message arg)
     (tidal-get-completions arg))))

(company-tidal-backend 'company-tidal-backend "de")

;;; Enable these when things are working
;; Install the backend.
;; (add-to-list 'company-backends 'company-tidal-backend)

;; Bind completion to tab.
;; (define-key map [\t] 'company-indent-or-complete-common)

;;; company-tidal.el ends here
