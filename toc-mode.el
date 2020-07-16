;;; document-outliner.el --- Manage outlines of pdf and djvu document  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Daniel Laurens Nicolai

;; Author: Daniel Laurens Nicolai <dalanicolai@gmail.com>
;; Keywords: tools, outlines, convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:



(provide 'document-outliner)
;;; document-outliner.el ends here

;;;; toc-extract and cleanup

;;; toc-cleanup
(defun toc-cleanup-dots ()
  (interactive)
  (beginning-of-buffer)
  (while (not (eobp))
    (re-search-forward "\\([\\. ]*\\)\\([0-9ivx]*\\) *$")
    (replace-match " \\2")
    (forward-line 1))
    )

(defun toc-cleanup-lines-contents-string (&optional arg)
  (interactive "nEnter line number of entry 'Contents': ")
  (when (called-interactively-p 'any)
    (next-line arg))
  (flush-lines "contents")
  )

(defun toc-cleanup-lines-roman-string (&optional arg)
  (interactive)
  (beginning-of-buffer)
  ;; (re-search-forward "^ *[ivx0-9\\.]+ *$" nil t)
  ;; (replace-match "")
  (while (not (eobp))
    (re-search-forward "^ *[ivx0-9\\.]* *$")
    (replace-match "")
    (forward-line 1))
  )

(defun toc-cleanup-blank-lines ()
  (interactive)
  (beginning-of-buffer)
  (flush-lines "^ *$")
  )

(defun toc-join-next-unnumbered-lines ()
  (interactive)
  (re-search-forward "[^0-9]\\s-*$" nil t)
   (join-line 1))

(defun toc-cleanup (startpage)
  (interactive)
  (beginning-of-buffer)
  (when (search-forward "contents" nil t)
    (replace-match (format "Contents %s" startpage)))
  (toc-cleanup-lines-contents-string)
  (toc-cleanup-dots)
  ;; (toc-cleanup-lines-roman-string)
  (toc-cleanup-blank-lines)
  (toc-join-next-unnumbered-lines)
  )

(defun get-index-levels (seperator)
  (let* ((string (thing-at-point 'line t))
         (sep (cond (seperator)
                    ("\\."))))
    (string-match (format "^\\([[:alnum:]]+%s\\)*" sep) string)
    (match-string 0 string)))

(defun toc-count-level-by-index (seperator)
  (let* ((index (get-index-levels seperator))
         (sep (cond (seperator)
                    ("\\.")))
         (string-list (split-string index sep t)))
    (length string-list)))

(defun toc-cleanup-set-level-by-index (&optional arg)
  "Automatic set indentation by number of seperators in index. By default uses dots as seperator. Prepend with universal argument (C-u) to enter different seperator."
  (interactive "P")
  (let ((seperator (if arg
                       (read-string
                        "Enter index seperator as regexp (escape with \\ if required): ")
                     nil)))
    (beginning-of-buffer)
    (while (re-search-forward "^\\s-+" nil t)
      (replace-match ""))
    (beginning-of-buffer)
    (while (not (eobp))
      (let* ((level (toc-count-level-by-index seperator)))
        (dotimes (x level) (insert " "))
        (forward-line 1)))))

;;; toc extract
(defun document-extract-pages-text (startpage endpage)
  (interactive "nEnter start-pagenumber for extraction: \nnEnter end-pagenumber for extraction: ")
  (let* ((source-buffer (current-buffer))
         (ext (url-file-extension (buffer-file-name (current-buffer))))
         (shell-command (cond ((string= ".pdf" ext) "pdftotext -f %s -l %s -layout %s -")
                              ((string= ".djvu" ext) "djvutxt --page=%s-%s %s")
                              (t (error "Buffer-filename does not have pdf or djvu extension"))))
         (text (shell-command-to-string 
                (format shell-command
                        startpage
                        endpage
                        (shell-quote-argument buffer-file-name))))
         (buffer (get-buffer-create (file-name-sans-extension (buffer-name)))))
    (switch-to-buffer buffer)
    (toc-cleanup-mode) ;; required before setting local variable
    (setq-local doc-buffer source-buffer)
    (insert text)
    ;; (kill-whole-line)
    ))


(defun toc-extract-pages (startpage endpage &optional arg)
  "Extract text and cleanup text from table of contents.
Use with the universal argument (C-u) omits cleanup to get the unprocessed text."
  (interactive "nEnter start-pagenumber for extraction: \nnEnter end-pagenumber for extraction: \nP")
  (document-extract-pages-text startpage endpage)
  (unless arg
    (toc-cleanup startpage)))

(defun toc-extract-outline ()
  (interactive)
  (let* ((source-buffer (current-buffer))
         (ext (url-file-extension (buffer-file-name (current-buffer))))
         (shell-command (cond ((string= ".pdf" ext) (if (executable-find "mutool")
                                                        "mutool show %s outline"
                                                      "mutool command is not found"))
                              ((string= ".djvu" ext) "djvused -e 'print-outline' %s")
                              (t (error "Buffer-filename does not have pdf or djvu extension"))))
         (text (shell-command-to-string 
                (format shell-command
                        (shell-quote-argument buffer-file-name))))
         (buffer (get-buffer-create (concat (file-name-sans-extension (buffer-name)) ".txt"))))
    (switch-to-buffer buffer)
    (setq-local doc-buffer source-buffer)
    (insert text)))

(defun toc-create-tablist-buffer ()
  (interactive)
  (toc-list doc-buffer))

;;;; toc major modes

(defvar toc-cleanup-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" 'toc-create-tablist-buffer)
    (define-key map "\C-c\C-j" 'toc-join-next-unnumbered-lines)
    map))

(define-derived-mode toc-cleanup-mode
  fundamental-mode "TOC-cleanup"
  "Major mode for cleaning up Table Of Contents"
   )

;;; toc tablist

(defun count-leading-spaces ()
  (interactive)
  (let ((start (string-match "^ *" (thing-at-point 'line)))
        (end (match-end 0)))
    (- end start)))

(defun toc-level ()
  (interactive)
  (let (levels)
    (beginning-of-buffer)
    (while (not (eobp))
      (let ((spaces (count-leading-spaces)))
        (unless (member spaces levels)
          (setq levels (append levels (list spaces))))
        (forward-line 1)))
    (progn levels)))

(defun toc-convert-to-tabulated-list ()
  (interactive)
  (beginning-of-buffer)
  (let (lines
        levels)
    (while (not (eobp))
      (let ((line-list (split-string (buffer-substring (line-beginning-position) (line-end-position))))
            (spaces (count-leading-spaces)))
        (unless (member spaces levels)
          (setq levels (append levels (list spaces))))
        (setq lines
              (append
               lines
               (list (list nil
                           (vector
                            (number-to-string (cl-position spaces levels))
                            (mapconcat 'identity (butlast line-list) " ")
                            (mapconcat 'identity (last line-list) " "))))))
        (forward-line)))
    lines
    ;; (progn lines)
    ))

(defun toc-increase ()
  (interactive)
  (tabulated-list-set-col
   "page"
   (number-to-string (+ (string-to-number (aref (tabulated-list-get-entry) 2)) 1))
   t))

(defun toc-decrease ()
  (interactive)
  (tabulated-list-set-col
   "page"
   (number-to-string (- (string-to-number (aref (tabulated-list-get-entry) 2)) 1))
   t))

(defun toc-increase-remaining ()
  (interactive)
  (save-excursion
    (while (not (eobp))
      (tabulated-list-set-col
       "page"
       (number-to-string (+ (string-to-number (aref (tabulated-list-get-entry) 2)) 1))
       t)
      (forward-line 1))))

(defun toc-decrease-remaining ()
  (interactive)
  (save-excursion
    (while (not (eobp))
      (tabulated-list-set-col
       "page"
       (number-to-string (- (string-to-number (aref (tabulated-list-get-entry) 2)) 1))
       t)
      (forward-line 1))))

(defun toc-tablist-follow ()
  (interactive)
  (let ((page (string-to-number (aref (tabulated-list-get-entry) 2))))
    (pop-to-buffer doc-buffer)
    (pdf-view-goto-page page)
    (other-window 1)))

(defun toc-scroll-pdf-other-window-page-up ()
  (interactive)
  (other-window 1)
  (pdf-view-next-page 1)
  (other-window 1))

(defun toc-scroll-pdf-other-window-page-down ()
  (interactive)
  (other-window 1)
  (pdf-view-previous-page 1)
  (other-window 1))

(defun toc-scroll-pdf-other-window-down ()
  (interactive)
  (other-window 1)
  (pdf-view-scroll-up-or-next-page 1)
  (other-window 1))

(defun toc-scroll-pdf-other-window-up ()
  (interactive)
  (other-window 1)
  (pdf-view-scroll-down-or-previous-page 1)
  (other-window 1))

(defvar toc-tabular-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [right] 'toc-increase-remaining)
    (define-key map [left] 'toc-decrease-remaining)
    (define-key map [S-right] 'toc-increase)
    (define-key map [S-left] 'toc-decrease)
    (define-key map [tab] 'toc-tablist-follow)
    (define-key map [S-down] 'toc-scroll-pdf-other-window-page-up)
    (define-key map [S-up] 'toc-scroll-pdf-other-window-page-down)
    (define-key map [C-down] 'toc-scroll-pdf-other-window-up)
    (define-key map [C-up] 'toc-scroll-pdf-other-window-down)
    (define-key map "\C-c\C-c" 'toc-tablist-to-pdfoutline)
    map))

(define-derived-mode toc-tabular-mode
  tabulated-list-mode "TOC"
  "Major mode for Table Of Contents."
  (setq-local tabulated-list-format [("level" 10 nil) ("name" 80 nil) ("page" 1 nil)])
  (tabulated-list-init-header))

(defun toc-list (buffer)
  (interactive)
  (let ((source-buffer buffer)
        (toc-tablist (toc-convert-to-tabulated-list)))
    (switch-to-buffer (concat (buffer-name) ".list"))
    (toc-tabular-mode)
    (setq-local doc-buffer source-buffer)
    (setq-local tabulated-list-entries toc-tablist)
    (tabulated-list-print)))

;;;; parse tablist to outline

(defvar toc-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" 'toc-add-to-pdf)
    map))

(define-derived-mode toc-mode
  fundamental-mode "TOC"
  "Major mode for editing pdf or djvu Table Of Contents source files"
  )


;;; pdf parse tablist to
(defun toc-tablist-to-pdfoutline ()
  (interactive)
  (beginning-of-buffer)
  (let (text)
    (while (not (eobp))
      (let ((v (tabulated-list-get-entry)))
        (setq text (concat text (format "%s %s %s\n" (aref v 0) (aref v 2) (aref v 1) )))
        (forward-line 1)))
    (switch-to-buffer (find-file (concat (file-name-sans-extension (buffer-name)) ".txt")))
    (erase-buffer)
    (toc-mode)
    (insert text)))

;;;; djvu parse tablist to outline

(defun toc-parse-djvused (level)
  (let* ((v (tabulated-list-get-entry))
         (sexp (list (aref v 1) (format "#%s" (aref v 2)))))
    (forward-line 1)
    (if (not (eobp))
      (let ((v (tabulated-list-get-entry)))
        (cond ((> (string-to-number (aref v 0)) level)
               (setq sexp
                     (list (append sexp (toc-parse-djvused (string-to-number (aref v 0)))))))
              ((= (string-to-number (aref v 0)) level)
                   (setq sexp (cons
                               sexp
                               (toc-parse-djvused (string-to-number (aref v 0))))))
              ((< (string-to-number (aref v 0)) level)
               (list sexp))))
      (list sexp))))

(defun toc-tablist-to-djvused ()
  (interactive)
  (beginning-of-buffer)
  (let* ((bookmarks '(bookmarks))
         (outline (append bookmarks (toc-parse-djvused 0))))
    (while (not (eobp))
      (setq outline (append outline (toc-parse-djvused 0))))
    (let ((text (format "%S" outline)))
      (switch-to-buffer (find-file (concat (file-name-sans-extension (buffer-name)) ".txt")))
      (erase-buffer)
      (insert text))))

;; (defun toc-tablist-to-toc-source ()
;;   (cond ((string= ".pdf" ext) "pdftotext -f %s -l %s -layout %s -")
;;         ((string= ".djvu" ext) "djvutxt --page=%s-%s %s")
;;         (t (error "Buffer-filename does not have pdf or djvu extension"))))


;;;; add outline to document
(defun toc-add-to-pdf ()
  "combine with add-toc-to-djvu in add-toc-to-document when ready"
  (interactive)
  (save-buffer)
  (call-process "pdfoutline" nil "*pdfoutline*" nil
                (concat (file-name-sans-extension (buffer-name)) ".pdf")
                (buffer-name)
                "pdfwithtoc.pdf"))

(defun toc-add-to-djvu ()
  "combine with add-toc-to-djvu in add-toc-to-document when ready"
  (interactive)
  (save-buffer)
  (shell-command (shell-quote-argument
                  (format
                  "djvused -s -e 'set-outline \"%s\"' %s"
                  (buffer-name)
                  (concat (file-name-sans-extension (buffer-name)) ".djvu")))))
  ;; (call-process "djvused" nil "*djvused*" nil
  ;;               "-s"
  ;;               "-e" 
  ;;               (format "'set-outline \"%s\"'" (buffer-file-name))
  ;;                (concat (file-name-sans-extension (buffer-name)) ".djvu")))

;;;; PDF-Tools functions (PDF-tools does not yet produce best layout, e.g. try pdf-extract-page-lines)

(defun pdf-extract-page-text (&optional arg)
  (interactive "P")
  (let* ((page (cond ((numberp arg) arg)
                     (arg (read-number "Enter pagenumber for extraction: "))
                     (t (pdf-view-current-page))))
         (regions (pdf-info-textregions page)))
    (print regions)))
    ;; (print (mapcar #'(lambda (region) (pdf-info-gettext page region)) regions))))

(defun line-edges (first last)
  (list (nth 0 first) (nth 1 first) (nth 2 last) (nth 3 last)))

(defun pdf-info-line-textregions (list)
  (let (line-regions
        (line list))
    (while line
      (let ((first (car line)))
        (while (and (car line) (= (nth 1 first) (nth 1 (car line))))
          (setq last (car line))
          (setq line (cdr line)))
        (setq line-regions (append line-regions (list (line-edges first last))))))
    (print line-regions)))

(defun pdf-info-line-textregions2 (regions)
  (let (line-regions
        (region (car regions)))
    (while regions
      (let ((r2 (car (cdr regions)))
            (r1 region))
        (if (equal (nth 1 r2) (nth 1 r1))
            (setq region (list (nth 0 r1) (nth 1 r1) (nth 2 r2) (nth 3 r2)))
          (progn (setq line-regions (append line-regions (list r1)))
                 (setq region r2)))
        (setq regions (cdr regions))))
    (print line-regions)))

(defun pdf-extract-page-lines (&optional arg)
  (interactive "P")
  (let* ((page (cond ((numberp arg) arg)
                     (arg (read-number "Enter pagenumber for extraction: "))
                     (t (pdf-view-current-page))))
         (regions (pdf-info-textregions page)))
    (print (pdf-info-gettext page (nth 1 (pdf-info-line-textregions regions)) 'line))))
    ;; (print (mapcar #'(lambda (region) (pdf-info-gettext page region)) (pdf-info-line-regions regions)))))
    ;; (mapcar '(lambda (region) (pdf-info-gettext page region)) regions)))



