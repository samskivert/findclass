;; findclass.el - bindings for the findclass tool
;;
;; Author: Michael Bayne <mdb * samskivert com>
;; Version: 1.0
;; URL: http://github.com/samskivert/findclass
;; Compatibility: GNU Emacs 22.x, GNU Emacs 23.x
;;
;; This file is NOT part of GNU Emacs.
;;
;;; Commentary:
;;
;; Configure findclass for use in Java mode by adding the following to your .emacs
;; file (customizing key bindings to your preference, naturally):
;;
;; (load "path/to/findclass.el")
;; (defun findclass-java-mode-hook ()
;;   (define-key java-mode-map "\C-c\C-i" 'import-class-at-symbol)
;;   (define-key java-mode-map "\C-c\C-j" 'open-class-at-symbol)
;;  )
;; (add-hook 'java-mode-hook 'findclass-java-mode-hook)
;;
;; findclass also works with Scala and ActionScript, so do the same for those
;; modes.
;;; Code:

(defvar findclass-jar "~/bin/findclass.jar"
  "The location of the findclass.jar.")

(defun open-class-at-symbol (class)
  "Locates and opens a buffer with the class identified by the
    symbol under the point as its argument."
  (interactive (list (read-from-minibuffer "Class: " (thing-at-point 'symbol))))
  (let* ((rebuild (if current-prefix-arg "-rebuild" ""))
         (command (concat "java -jar " findclass-jar " " rebuild " "
                          class " " (buffer-file-name)))
         (output (split-string (shell-command-to-string command)))
         )
    (if (string= (car output) "nomatch")
        (message (format "No match for class: %s" class))
      (find-file (car (cdr output)))
      (goto-line (string-to-number (car (cdr (cdr output)))))
      )
    )
  )

(defun import-class-at-symbol (class &optional arg)
  "Locates the class identified by the symbol under the point and
inserts an import statement for that class in the appropriate
position."
  (interactive (list (read-from-minibuffer "Class: " (thing-at-point 'symbol))))
  (let* ((rebuild (if current-prefix-arg "-rebuild" ""))
         (command (concat "java -jar " findclass-jar " " rebuild " -import "
                          class " " (buffer-file-name)))
         (output (split-string (shell-command-to-string command)))
         (term (if (string= mode-name "Scala") "" ";"))
         (spoint (point)) ;; save the point
         )
    (if (string= (car output) "nomatch")
        (message (format "No match for class: %s" class)))
    (if (string= (car output) "notneeded")
        (message (format "%s is already imported." class)))
    (if (string= (car output) "match")
        (let ((mclass (car (cdr output)))
              (mline (car (cdr (cdr output))))
              (mblank (car (cdr (cdr (cdr output)))))
              (madjust 0) (mneedadjust nil))
          ;; go to our insertion line
          (goto-line (string-to-number mline))
          (setq mneedadjust (<= (point) spoint))
          ;; insert the import statment and any necessary blank lines
          (if (string= mblank "preblank")
              (progn
                (insert "\n")
                (setq madjust (1+ madjust))))
          (insert (concat "import " mclass term "\n"))
          (setq madjust (+ madjust (length mclass)))
          (setq madjust (+ madjust 8)) ; 'import \n'
          (setq madjust (+ madjust (length term))) ; optional semicolon
          (if (string= mblank "postblank")
              (progn
                (insert "\n")
                (setq madjust (1+ madjust))))
          ;; finally adjust the point (if necessary) and restore it
          (if mneedadjust (setq spoint (+ spoint madjust)))
          (goto-char spoint)
          (message (format "Imported: %s" mclass))
          )
      )
    )
  )
