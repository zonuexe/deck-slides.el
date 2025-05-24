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
  "."
  :type 'string)

(defvar-local deck-slides-id nil)

(defvar-local deck-slides-layout-names nil)

(eval-and-compile
  (defconst deck-slides-separator "\n---\n"))

;; Utility functions
(defun deck-slides--command-line (&rest args)
  "ARGS."
  (mapconcat #'shell-quote-argument (cons deck-slides-executable args) " "))

;; Internal functions
(defun deck-slides-current-buffer-id-and-register ()
  ""
  (unless deck-slides-id
    (let ((new-id (read-string "Input Google slides ID: ")))
      (setq deck-slides-id new-id)))
  deck-slides-id)

(defun deck-slides--fetch-ls-layous (id)
  "ID."
  (split-string (string-trim-right (shell-command-to-string (deck-slides--command-line "ls-layouts" id)))))

(defun deck-slides--get-current-page ()
  "."
  (save-match-data
    (save-mark-and-excursion
      (save-restriction
        (widen)
        (when (search-forward (eval-when-compile deck-slides-separator) nil t)
          (goto-char (match-end 0)))
        (let ((current-position (point))
              (buffer-end-position (point-max)))
          (goto-char (point-min))
          (named-let loop ((page 0))
            (if (and (<= (point) current-position)
                     (not (eq (point) buffer-end-position))
                     (re-search-forward (eval-when-compile (rx-to-string `(or ,deck-slides-separator buffer-end)))
                                        nil t))
                (loop (1+ page))
              page)))))))

;; Commands
(defun deck-slides-apply (id)
  "ID."
  (interactive (list (deck-slides-current-buffer-id-and-register)))
  (let ((default-directory (expand-file-name "~")))
    (message "%s" (shell-command-to-string (deck-slides--command-line "apply" id buffer-file-name)))))

(defun deck-slides-apply-only-current-page (id)
  "ID."
  (interactive (list (deck-slides-current-buffer-id-and-register)))
  (let ((default-directory (expand-file-name "~"))
        (page (number-to-string (deck-slides--get-current-page))))
    (message "%s" (shell-command-to-string
                   (deck-slides--command-line "apply" "-p" page id buffer-file-name)))))

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
(define-minor-mode deck-slides-mode
  "."
  :lighter deck-slides-lighter)

(provide 'deck-slides)
;;; deck-slides.el ends here
