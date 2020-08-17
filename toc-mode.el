;;; toc-mode.el --- Manage outlines of pdf and djvu document  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Daniel Laurens Nicolai

;; Author: Daniel Laurens Nicolai <dalanicolai@gmail.com>
;; Keywords: tools, outlines, convenience
;; Package-Requires: ((emacs "26.1"))
;; URL: https://github.com/dalanicolai/toc-mode


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

;; toc-mode.el is a package to create and add a Table of Contents to pdf and
;; djvu documents. It implements features to extract a Table of Contents from
;; the textlayer of a document or via OCR if that last option is necessary or
;; prefered. Subsequently this package implements various features to assist in
;; tidy up the extracted Table of Contents, adjust the pagenumbers and finally
;; parsing the Table of Contents into syntax that is understood by the
;; `pdfoutline' and `djvused' commands that are used to add the table of
;; contents to pdf- and djvu-files respectively.

;; Important: For djvu documents, only outlines with maximum level deepness of 1
;; are correctly parsed. For outlines with level deepness of 2 not all levels
;; are set to correct level because the algorithm in the toc--parse-djvused
;; function does not parse such files correctly yet (you are welcome to ‘repair’
;; the algorithm to produce the correct syntax as it is described on the djvused
;; website here).

;; Requirements: Currently the package requires the `pdftotext' (part of
;; poppler-utils), `pdfoutline' (part of fntsample) and `djvused' (part of
;; http://djvu.sourceforge.net/) command line utilities to be available.
;; Extraction with OCR requires the tesseract command line utility to be
;; available.

;; Usage: Extraction and adding contents to a document is done in 4 steps: 1
;; extraction 2 cleanup 3 adjust/correct pagenumbers 4 add TOC to document

;; 1. Extraction Open some pdf or djvu file in Emacs (pdf-tools and djvu package
;; recommended). Find the pagenumbers for the TOC. Then type M-x
;; toc-extract-pages, or M-x toc-extract-pages-ocr if doc has no text layer or
;; text layer is bad, and answer the subsequent prompts by entering the
;; pagenumbers for the first and the last page each followed by RET. For PDF
;; extraction with OCR, currently it is required to view all contents pages once
;; before extraction (toc-mode uses the cached file data). A buffer with the,
;; somewhat cleaned up, extracted text will open in TOC-cleanup mode. Prefix
;; command with the universal argument (C-u) to omit clean and get the raw text.
;; 2. TOC-Cleanup In this mode you can further cleanup the contents to create a
;; list where each line has the structure:

;; TITLE (SOME) PAGENUMBER

;; There can be any number of spaces between TITLE and PAGE. The correct
;; pagenumbers can be edited in the next step. A document outline supports
;; different levels and levels are automatically assigned in order of increasing
;; number of preceding spaces, i.e. the lines with the least amount of preceding
;; spaces are assigned level 0 etc., and lines with equal number of spaces get
;; assigned the same levels.

;; Contents   1
;; Chapter 1      2
;; Section 1 3
;; Section 1.1     4
;; Chapter 2      5

;; There are some handy functions to assist in the cleanup. C-c C-j jumps
;; automatically to the next line not ending with a number and joins it with the
;; next line. If the indentation structure of the different lines does not
;; correspond with the levels, then the levels can be set automatically from the
;; number of separatorss in the indices with M-x toc--cleanup-set-level-by-index.
;; The default separators is a . but a different separators can be entered by
;; preceding the function invocation with the universal argument (C-u). Some
;; documents contain a structure like

;; 1 Chapter 1    1
;; Section 1      2

;; Here the indentation can be set with M-x replace-regexp ^[^0-9] -> \& (where
;; there is a space character before the \&).

;; Type C-c C-c when finished

;; 3. TOC-tabular (adjust pagenumbers) This mode provides the functionality for
;; easy adjustment of pagenmumbers. The buffer can be navigated with the arrow
;; up/down keys. The left and right arrow keys will shift down/up all the page
;; numbers from the current line and below (combine with SHIFT for setting
;; individual pagenumbers).

;; The TAB key jumps to the pagenumber of the current line, while C-right/C-left
;; will shift all remaining page numbers up/down while jumping/scrolling to the
;; line its page in the document window. to the S-up/S-donw in the tablist
;; window will just scroll page up/down in the document window and, only for
;; pdf, C-up/C-down will scroll smoothly in that window.

;; Type C-c C-c when done.

;; 4. TOC-mode (add outline to document) The text of this buffer should have the
;; right structure for adding the contents to (for pdf’s a copy of) the original
;; document. Final adjusments can be done but should not be necessary. Type C-c
;; C-c for adding the contents to the document.

;; By default, the TOC is simply added to the original file. ONLY FOR PDF’s, if
;; the (customizable) variable toc-replace-original-file is nil, then the TOC is
;; added to a copy of the original pdf file with the path as defined by the
;; variable toc-destination-file-name. Either a relative path to the original
;; file directory or an absolute path can be given.
;;; Code:

;; Keybindings
;; all-modes (i.e. all steps)
;;  Key Binding        Description
;;   ~C-c C-c~         dispatch (next step)

;; toc-cleanup-mode
;;  ~C-c C-j~          toc--join-next-unnumbered-lines

;; toc-mode (tablist)
;;  ~TAB~              preview/jump-to-page
;;  ~right/left~       toc-in/decrease-remaining
;;  ~C-right/C-left~   toc-in/decrease-remaining and view page
;;  ~S-right/S-left~   in/decrease pagenumber current entry
;;  ~C-down/C-up~      scroll document other window (if document buffer shown)
;;  ~S-down/S-up~      full page scroll document other window ( idem )

(require 'pdf-tools nil t)
(require 'djvu nil t)
(require 'evil nil t)

(defvar djvu-doc-image)
(defvar doc-buffer)

(declare-function pdf-cache-get-image "pdf-cache")
(declare-function pdf-view-goto-page "pdf-view")
(declare-function pdf-view-next-page "pdf-view")
(declare-function pdf-view-previous-page "pdf-view")
(declare-function pdf-view-scroll-up-or-next-page "pdf-view")
(declare-function pdf-view-scroll-down-or-previous-page "pdf-view")
(declare-function djvu-goto-page "djvu")
(declare-function djvu-next-page "djvu")
(declare-function djvu-prev-page "djvu")
(declare-function djvu-scroll-up-or-next-page "djvu")
(declare-function djvu-scroll-down-or-previous-page "djvu")
(declare-function evil-scroll-page-down "evil-commands")
(declare-function evil-scroll-page-up "evil-commands")

(defgroup toc nil
  "Setting for the toc-mode package"
  :group 'data)

(defcustom toc-replace-original-file t
  "For PDF include TOC and replace old PDF file.
For DJVU the old DJVU file is replaced by default"
  :type 'boolean
  :group 'toc)

(defcustom toc-destination-file-name "pdfwithtoc.pdf"
  "Filename for new PDF if `toc-replace-original-file' is nil."
  :type 'file
  :group 'toc)

;;;; toc-extract and cleanup

;;; toc-cleanup
(defun toc--cleanup-dots ()
  "Remove dots between heading its title and page number."
  (interactive)
  (goto-char (point-min))
  (while (not (eobp))
    (re-search-forward "\\([\\. ]*\\)\\([0-9ivx]*\\) *$")
    (replace-match " \\2")
    (forward-line 1)))

(defun toc--cleanup-dots-ocr ()
  "Remove dots between heading its title and page number.
Like `toc--cleanup-dots' but more suited for use after OCR"
  (goto-char (point-min))
  (while (re-search-forward "\\([0-9\\. \\-]*\\)\\( [0-9]* *\\)$" nil t)
    (replace-match " \\2")))

(defun toc--cleanup-lines-contents-string ()
  "Delete all lines containing the string \"contents\"."
  (interactive)
  (flush-lines "contents"))

(defun toc--cleanup-lines-roman-string ()
  "Delete all lines that contain only linefeeds and/or blanks and/or roman numerals."
  (interactive)
  (goto-char (point-min))
  (while (not (eobp))
    (re-search-forward "^[\f ]*[ivx0-9\\.]* *$")
    (replace-match "")
    (forward-line 1)))

(defun toc-cleanup-blank-lines ()
  "Delete all empty lines."
  (interactive)
  (goto-char (point-min))
  (flush-lines "^ *$"))

(defun toc--join-next-unnumbered-lines ()
  "Search from point for first occurence of line not ending with Western numerals."
  (interactive)
  (re-search-forward "[^0-9]\\s-*$" nil t)
  (join-line 1))

(defun toc--jump-next-overindexed-index ()
  "Jump to next line with unwanted dot behind its index."
  (interactive)
  (re-search-forward "^[0-9\\.]*\\. " nil t))

(defun toc--cleanup (startpage &optional arg)
  "Cleanup extracted Table Of Contents by running a series of cleanup functions."
  (interactive)
  (goto-char (point-min))
  (when (search-forward "contents" nil t)
    (replace-match (format "Contents %s" startpage)))
  (toc--cleanup-lines-contents-string)
  (if arg
      (toc--cleanup-dots-ocr)
    (toc--cleanup-dots))
  (toc--cleanup-lines-roman-string)
  (toc-cleanup-blank-lines))

(defun toc--get-section-inidices (separators)
  "Determine index part of current line. Counting the number of SEPARATORS."
  (let* ((string (thing-at-point 'line t))
         (sep (cond (separators)
                    ("\\."))))
    (string-match (format "^\\([[:alnum:]]+%s\\)*" sep) string)
    (match-string 0 string)))

(defun toc--count-level-by-index (separators)
  "Determine level of current line in TOC tree by counting SEPARATORS."
  (let* ((index (toc--get-section-inidices separators))
         (sep (cond (separators)
                    ("\\.")))
         (string-list (split-string index sep t)))
    (length string-list)))

(defun toc--cleanup-set-level-by-index (&optional arg)
  "Automatic set indentation by number of separatorss in index.
By default uses dots as separators. Prepend with universal
ARG (\\[universal-argument]) to enter different separators."
  (interactive "P")
  (let ((separators (if arg
                        (read-string
                         "Enter index separators as regexp (escape with \\ if required): ")
                      nil)))
    (goto-char (point-min))
    (while (re-search-forward "^\\s-+" nil t)
      (replace-match ""))
    (goto-char (point-min))
    (while (not (eobp))
      (let* ((level (toc--count-level-by-index separators)))
        (dotimes (_x level) (insert " "))
        (forward-line 1)))))

;;; toc extract
(defun toc--document-extract-pages-text (startpage endpage)
  "Extract text from text layer of current document from STARTPAGE to ENDPAGE."
  (let* ((source-buffer (current-buffer))
         (ext (url-file-extension (buffer-file-name (current-buffer))))
         (default-process-coding-system
           (cond ((string= ".pdf" ext)'(windows-1252-unix . utf-8-unix))
                 ((string= ".djvu" ext) '(utf-8-unix . utf-8-unix))))
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
    (when (fboundp 'flyspell-mode)
      flyspell-mode)
    (setq-local doc-buffer source-buffer)
    (insert text)))

;;;###autoload
(defun toc-extract-pages (arg)
  "Extract text from text layer of current document and cleanup.
Extract from STARTPAGE to ENDPAGE. Use with the universal
ARG (\\[universal-argument]) omits cleanup to get the unprocessed
text."
  (interactive "P")
  (let ((mode (derived-mode-p 'pdf-view-mode 'djvu-read-mode)))
    (if mode
        (let* ((startpage (read-string "Enter start-pagenumber for extraction: "))
               (endpage (read-string "Enter end-pagenumber for extraction: ")))
          (toc--document-extract-pages-text startpage endpage)
          (unless arg
            (toc--cleanup startpage)))
      (message "Buffer not in pdf-view- or djvu-read-mode"))))


;;;###autoload
(defun toc-extract-pages-ocr (arg)
  "Extract via OCR text of current document and cleanup.
Extract from STARTPAGE to ENDPAGE. Use with the universal
ARG (\\[universal-argument]) omits cleanup to get the
unprocessed text."
  (interactive "P")
  (let ((mode (derived-mode-p 'pdf-view-mode 'djvu-read-mode)))
    (if mode
        (let* ((page (string-to-number
                      (read-string "Enter start-pagenumber for extraction: ")))
               (endpage (string-to-number
                         (read-string "Enter end-pagenumber for extraction: ")))
               (source-buffer (current-buffer))
               (ext (url-file-extension (buffer-file-name (current-buffer))))
               (buffer (file-name-sans-extension (buffer-name))))
          (while (<= page (+ endpage))
            (let ((file (cond ((string= ".pdf" ext)
                               (make-temp-file "pageimage"
                                               nil
                                               (number-to-string page)
                                               (pdf-cache-get-image page 600)))
                              ((string= ".djvu" ext)
                               (djvu-goto-page page)
                               (make-temp-file "pageimage"
                                               nil
                                               (number-to-string page)
                                               (image-property djvu-doc-image :data))))))
              (call-process "tesseract" nil (list buffer nil) nil file "stdout" "--psm" "6")
              (setq page (1+ page))))
          (switch-to-buffer buffer)
          (toc-cleanup-mode) ;; required before setting local variable
          (when (fboundp 'flyspell-mode)
            (flyspell-mode))
          (setq-local doc-buffer source-buffer)
          (unless arg
            (toc--cleanup page t)))
      (message "Buffer not in pdf-view- or djvu-read-mode"))))

;;;###autoload
(defun toc-extract-outline ()
  "Extract Table of Contents attached to current document."
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

(defun toc--create-tablist-buffer ()
  "Create tablist buffer, from cleaned up Table of Contents buffer, for easy page number adjustment."
  (interactive)
  (toc--list doc-buffer))

;;;; toc major modes

(defvar toc-cleanup-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" #'toc--create-tablist-buffer)
    (define-key map "\C-c\C-j" #'toc--join-next-unnumbered-lines)
    map))

(define-derived-mode toc-cleanup-mode
  fundamental-mode "TOC-cleanup"
  "Major mode for cleaning up Table Of Contents
\\{toc-cleanup-mode-map}")

;;; toc tablist

(defun toc-count-leading-spaces ()
  "Count number of leading spaces on current line."
  (interactive)
  (let ((start (string-match "^ *" (thing-at-point 'line)))
        (end (match-end 0)))
    (- end start)))

(defun toc--convert-to-tabulated-list ()
  "Parse and prepare content of current buffer for `toc-tabular-mode'."
  (interactive)
  (goto-char (point-min))
  (let (lines
        levels)
    (while (not (eobp))
      (let ((line-list (split-string (buffer-substring (line-beginning-position) (line-end-position))))
            (spaces (toc-count-leading-spaces)))
        (unless (member spaces levels)
          (setq levels (append levels (list spaces))))
        (setq lines
              (append
               lines
               (list (list nil
                           (vector
                            (number-to-string (cl-position spaces levels))
                            (mapconcat #'identity (butlast line-list) " ")
                            (mapconcat #'identity (last line-list) " "))))))
        (forward-line)))
    lines))

(defun toc--increase ()
  "Increase pagenumber of current entry."
  (interactive)
  (tabulated-list-set-col
   "page"
   (number-to-string (+ (string-to-number (aref (tabulated-list-get-entry) 2)) 1))
   t))

(defun toc--decrease ()
  "Decrease pagenumber of current entry."
  (interactive)
  (tabulated-list-set-col
   "page"
   (number-to-string (- (string-to-number (aref (tabulated-list-get-entry) 2)) 1))
   t))

(defun toc--increase-remaining ()
  "Increase pagenumber of current entry and all entries below."
  (interactive)
  (save-excursion
    (while (not (eobp))
      (tabulated-list-set-col
       "page"
       (number-to-string (+ (string-to-number (aref (tabulated-list-get-entry) 2)) 1))
       t)
      (forward-line 1))))

(defun toc--decrease-remaining ()
  "Decrease pagenumber of current entry and all entries below."
  (interactive)
  (save-excursion
    (while (not (eobp))
      (tabulated-list-set-col
       "page"
       (number-to-string (- (string-to-number (aref (tabulated-list-get-entry) 2)) 1))
       t)
      (forward-line 1))))

(defun toc--tablist-follow ()
  "Preview pagenumber of current line in separate document buffer."
  (interactive)
  (let ((ext (url-file-extension (buffer-file-name doc-buffer)))
        (page (string-to-number (aref (tabulated-list-get-entry) 2))))
    (pop-to-buffer doc-buffer)
    (cond ((string= ".pdf" ext) (pdf-view-goto-page page))
          ((string= ".djvu" ext) (djvu-goto-page page)))
    (other-window 1)))

(defun toc--increase-remaining-and-follow ()
  "Increase pagenumber of current entry and all entries below and preview page in separate document buffer."
  (interactive)
  (toc--increase-remaining)
  (toc--tablist-follow))

(defun toc--decrease-remaining-and-follow ()
  "Decrease pagenumber of current entry and all entries below and preview page in separate document buffer."
  (interactive)
  (toc--decrease-remaining)
  (toc--tablist-follow))

(defun toc--scroll-other-window-page-up ()
  "Scroll page up in document buffer from current buffer."
  (interactive)
  (other-window 1)
  (let ((ext (url-file-extension (buffer-file-name (current-buffer)))))
    (cond ((string= ".pdf" ext) (pdf-view-next-page 1))
          ((string= ".djvu" ext) (djvu-next-page 1))))
  (other-window 1))

(defun toc--scroll-other-window-page-down ()
  "Scroll page down in document buffer from current buffer."
  (interactive)
  (other-window 1)
  (let ((ext (url-file-extension (buffer-file-name (current-buffer)))))
    (cond ((string= ".pdf" ext) (pdf-view-previous-page 1))
          ((string= ".djvu" ext) (djvu-prev-page 1))))
  (other-window 1))

(defun toc--scroll-pdf-other-window-down ()
  "Scroll down in document buffer from current buffer."
  (interactive)
  (other-window 1)
  (let ((ext (url-file-extension (buffer-file-name (current-buffer)))))
    (cond ((string= ".pdf" ext) (pdf-view-scroll-up-or-next-page 1))
          ((string= ".djvu" ext) (djvu-scroll-up-or-next-page))))
  (other-window 1))

(defun toc--scroll-pdf-other-window-up ()
  "Scroll up in document buffer from current buffer."
  (interactive)
  (other-window 1)
  (let ((ext (url-file-extension (buffer-file-name (current-buffer)))))
    (cond ((string= ".pdf" ext) (pdf-view-scroll-down-or-previous-page 1))
          ((string= ".djvu" ext) (djvu-scroll-down-or-previous-page))))
  (other-window 1))

(defvar toc-tabular-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [right] #'toc--increase-remaining)
    (define-key map [left] #'toc--decrease-remaining)
    (define-key map [S-right] #'toc--increase)
    (define-key map [S-left] #'toc--decrease)
    (define-key map [C-right] #'toc--increase-remaining-and-follow)
    (define-key map [C-left] #'toc--decrease-remaining-and-follow)
    (define-key map [tab] #'toc--tablist-follow)
    (define-key map [S-down] #'toc--scroll-other-window-page-up)
    (define-key map [S-up] #'toc--scroll-other-window-page-down)
    (define-key map [C-down] #'toc--scroll-pdf-other-window-down)
    (define-key map [C-up] #'toc--scroll-pdf-other-window-up)
    (define-key map "\C-c\C-c" #'toc--tablist-to-toc-source)
    (define-key map "\C-c\C-c" #'toc--tablist-to-toc-source)
    (when (featurep 'evil-commands)
      (define-key map "\S-j" #'evil-scroll-page-down)
      (define-key map "\S-k" #'evil-scroll-page-up))
    map))

(define-derived-mode toc-tabular-mode
  tabulated-list-mode "TOC-tabular"
  "Major mode for Table Of Contents.
\\{toc-tabular-mode-map}"
  (setq-local tabulated-list-format [("level" 10 nil) ("name" 80 nil) ("page" 1 nil)])
  (tabulated-list-init-header))

(defun toc--list (buffer)
  "Create, BUFFER, new tabular-mode-buffer for easy pagenumber adjusment."
  (interactive)
  (let ((source-buffer buffer)
        (toc-tablist (toc--convert-to-tabulated-list)))
    (switch-to-buffer (concat (buffer-name) ".list"))
    (when (fboundp 'golden-ratio-mode)
      (golden-ratio-mode))
    (toc-tabular-mode)
    (setq-local doc-buffer source-buffer)
    (setq-local tabulated-list-entries toc-tablist)
    (tabulated-list-print)))

;;;; parse tablist to outline

(defvar toc-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" #'toc--add-to-doc)
    map))

(define-derived-mode toc-mode
  fundamental-mode "TOC"
  "Major mode for editing pdf or djvu Table Of Contents source files
\\{toc-mode-map}")


;;; pdf parse tablist to
(defun toc--tablist-to-pdfoutline ()
  "Parse and prepare tablist-mode-buffer to source input.
Displays results in a newlycreated buffer for use as source input
to `pdfoutline' shell command."
  (interactive)
  (goto-char (point-min))
  (let ((source-buffer doc-buffer)
        text)
    (while (not (eobp))
      (let ((v (tabulated-list-get-entry)))
        (setq text (concat text (format "%s %s %s\n" (aref v 0) (aref v 2) (aref v 1) )))
        (forward-line 1)))
    (switch-to-buffer (find-file (concat (file-name-sans-extension (buffer-name)) ".txt")))
    (erase-buffer)
    (toc-mode)
    (setq-local doc-buffer source-buffer)
    (insert text)))

;;; djvu parse tablist to outline
(defun toc--tablist-to-djvused ()
  (interactive)
  (let ((source-buffer doc-buffer)
        (buff (get-buffer-create
               (concat
                (file-name-sans-extension (buffer-name))
                ".txt"))))
    (with-current-buffer buff
      (insert "(bookmarks "))
    (goto-char (point-min))
    (while (not (eobp))
      (let* ((v (tabulated-list-get-entry))
             (level-current (string-to-number (aref v 0)))
             (sexp (list (aref v 1) (format "#%s" (aref v 2))))
             (v-next (progn (forward-line) (tabulated-list-get-entry)))
             (level-next (when v-next (string-to-number (aref v-next 0)))))
        (if level-next
            (with-current-buffer buff
              (cond ((= level-next level-current)
                     (insert (format "(\"%s\" \"%s\") " (car sexp) (nth 1 sexp))))
                    ((> level-next level-current)
                     (insert (format "(\"%s\" \"%s\" " (car sexp) (nth 1 sexp))))
                    ((< level-next level-current)
                     (insert (format "(\"%s\" \"%s\")" (car sexp) (nth 1 sexp)))
                     (let ((level-diff (- level-current level-next)))
                       (while (> level-diff 0)
                         (insert ")")
                         (setq level-diff (1- level-diff)))))))
          (forward-line))))
    (forward-line -1)
    (let ((v (tabulated-list-get-entry)))
          (switch-to-buffer buff)
          (insert (format " (\"%s\" \"#%s\"))" (aref v 1) (aref v 2)))
          (toc-mode)
          (setq-local doc-buffer source-buffer))))

(defun toc--tablist-to-toc-source ()
  "Parse and prepare, from tablist-mode-buffer, a new buffer for use as source input to `pdfoutline' or `djvused' shell command."
  (interactive)
  (let ((ext (url-file-extension (buffer-file-name doc-buffer))))
    (cond ((string= ".pdf" ext) (toc--tablist-to-pdfoutline))
          ((string= ".djvu" ext) (print "this is DJVU") (toc--tablist-to-djvused))
          (t (error "Buffer-source-file does not have pdf or djvu extension")))))


;;;; add outline to document
(defun toc--add-to-pdf ()
  "Combine with add-toc-to-djvu in add-toc-to-document when ready."
  (interactive)
  (save-buffer)
  (call-process "pdfoutline" nil "*pdfoutline*" nil
                (concat (file-name-sans-extension (buffer-name)) ".pdf")
                (buffer-name)
                (if toc-replace-original-file
                    (concat (file-name-sans-extension (buffer-name)) ".pdf")
                  toc-destination-file-name)))

(defun toc--add-to-djvu ()
  "Combine with add-toc-to-djvu in add-toc-to-document when ready."
  (interactive)
  (save-buffer)
  (shell-command (shell-command-to-string
                  (format
                   "djvused -s -e \"set-outline '%s'\" %s"
                   (buffer-name)
                   (shell-quote-argument
                    (concat (file-name-sans-extension (buffer-name)) ".djvu"))))))


(defun toc--add-to-doc ()
  "Add Table Of Contents to original document.
The text of the current buffer is passed as source input to either the
`pdfoutline' or `djvused' shell command."
  (interactive)
  (let ((ext (url-file-extension (buffer-file-name doc-buffer))))
    (cond ((string= ".pdf" ext) (toc--add-to-pdf))
          ((string= ".djvu" ext) (toc--add-to-djvu)))))

(provide 'toc-mode)

;;; toc-mode.el ends here
