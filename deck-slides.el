;;; deck-slides.el --- Creating deck using Markdown and Google Slides  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  USAMI Kenta

;; Author: USAMI Kenta <tadsan@zonu.me>
;; Keywords: multimedia
;; URL: https://github.com/zonuexe/deck-slides.el
;; Version: 0.0.1
;; Package-Requires: ((emacs "30.1"))
;; License: GPL-3.0-or-later

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

;; Interface to https://github.com/k1LoW/deck

;;; Code:
(require 'xdg)

(defvar deck-slides-lighter " deck")

(defgroup deck-slides nil
  "Creating deck using Markdown and Google Slides."
  :group 'multimedia)

(defcustom deck-slides-executable (executable-find "deck")
  "Path to the deck executable."
  :type 'string)

(defcustom deck-slides-cache-file (expand-file-name "deck-slides.eld" user-emacs-directory)
  "File to cache Google Slides IDs associated with Markdown files."
  :type '(choice file (const :tag "No cache" nil)))

(defcustom deck-slides-code-block-to-image-command nil
  "Command strings and expression language for converting code blocks into images.
See https://github.com/k1LoW/deck?tab=readme-ov-file#code-blocks-to-images."
  :type '(choice string (const :tag "" nil))
  :safe (lambda (v) (or (null v) (stringp v))))

(defvar-local deck-slides-id nil
  "Google Slides ID for the current buffer.")

(defvar-local deck-slides-layout-names nil
  "Cached list of layout names for the current slide ID.")

(eval-and-compile
  (defconst deck-slides-separator "\n---\n"
    "Separator used between slides in the Markdown file."))

;; Utility functions
(defun deck-slides--command-line (&rest args)
  "Build the command line string for the deck executable with ARGS."
  (mapconcat #'shell-quote-argument (cons deck-slides-executable args) " "))

;; Internal functions
(defun deck-slides-read-cache ()
  "Read the association list of file paths and Google Slides IDs from cache."
  (when (and deck-slides-cache-file (file-exists-p deck-slides-cache-file))
    (with-temp-buffer
      (insert-file-contents deck-slides-cache-file)
      (condition-case nil
          (read (current-buffer))
        (end-of-file
         (warn "Failed to read the projects list file due to unexpected EOF"))))))

(defun deck-slides-save-cache (ids)
  "Save the association list IDS into cache file `deck-slides-cache-file'."
  (when deck-slides-cache-file
    (with-temp-buffer
      (insert ";;; -*- lisp-data -*-\n")
      (let ((print-length nil)
            (print-level nil))
        (pp ids (current-buffer)))
      (write-region nil nil deck-slides-cache-file nil 'silent))))

(defconst deck-slides--presentation-url-pattern
  (eval-when-compile
    (rx "https://docs.google.com/presentation/"
        (? "u/" (in "0-9") "/")
        "d/"
        (group (+ (not "/"))))))

(defconst deck-slides--presentation-id-pattern
  (eval-when-compile
    (rx bos (+ (in "a-zA-Z0-9-_")) eos)))

(defun deck-slides-current-buffer-id-and-register ()
  "Get the Google Slides ID for the current buffer.
If not set, prompt the user and store it."
  (unless deck-slides-id
    (let ((stored-ids (deck-slides-read-cache))
          new-id)
      (if-let* ((current-id (alist-get buffer-file-name stored-ids nil nil #'equal)))
          (setq deck-slides-id current-id)
        (setq new-id (read-string "Input Google Slides presentation ID or URL: "))
        (save-match-data
          (when (string-match deck-slides--presentation-url-pattern new-id)
            (setq new-id (match-string-no-properties 1 new-id))))
        (unless (string-match-p deck-slides--presentation-id-pattern new-id)
          (user-error "Invalid presentation ID"))
        (setq deck-slides-id new-id)
        (deck-slides-save-cache (cons (cons buffer-file-name new-id) stored-ids)))))
  deck-slides-id)

(defun deck-slides--fetch-ls-layous (id)
  "Fetch layout names for the given slide ID using the deck command."
  (save-match-data
    (split-string (string-trim-right (shell-command-to-string (deck-slides--command-line "ls-layouts" id))))))


;; Commands
;;;###autoload
(defun deck-slides-apply (id)
  "Apply all slides change to Google Slides ID.
When called non-interactively, ID must be provided."
  (interactive (list (deck-slides-current-buffer-id-and-register)))
  (let ((default-directory (expand-file-name "~")))
    (message "%s" (shell-command-to-string
                   (if deck-slides-code-block-to-image-command
                       (deck-slides--command-line "apply" buffer-file-name "--presentation-id" id "-c" deck-slides-code-block-to-image-command)
                     (deck-slides--command-line "apply"buffer-file-name "--presentation-id" id))))))

;;;###autoload
(defun deck-slides-apply-watch (id)
  "Apply all slides change to Google Slides ID.
When called non-interactively, ID must be provided."
  (interactive (list (deck-slides-current-buffer-id-and-register)))
  (let ((default-directory (expand-file-name "~")))
    (compile
     (if deck-slides-code-block-to-image-command
         (deck-slides--command-line "apply" buffer-file-name "--watch" "--presentation-id" id  "-c" deck-slides-code-block-to-image-command)
       (deck-slides--command-line "apply" buffer-file-name "--watch" "--presentation-id" id )))))

;;;###autoload
(defun deck-slides-ls-layouts (id &optional force-update)
  "Show layout names by slide ID.

`ls-layouts' results are cached by default.
When FORCE-UPDATE is non-NIL, the cache is refreshed."
  (interactive (list (deck-slides-current-buffer-id-and-register)
                     (not (null current-prefix-arg))))
  (when (or force-update (null deck-slides-layout-names))
    (setq deck-slides-layout-names (deck-slides--fetch-ls-layous id)))
  (message "layout-names: %S" deck-slides-layout-names))

;;;###autoload
(defun deck-slides-find-credentials-json ()
  "Find deck `credentials.json' file."
  (interactive)
  (find-file (expand-file-name "deck/credentials.json" (xdg-data-home))))


;; Minor mode
;;;###autoload
(define-minor-mode deck-slides-mode
  "Minor mode for interacting with deck CLI.  Enables auto-apply on idle."
  :lighter deck-slides-lighter)

(provide 'deck-slides)
;;; deck-slides.el ends here
