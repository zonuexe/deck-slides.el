;;; deck-slides.el --- Creating deck using Markdown and Google Slides  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  USAMI Kenta

;; Author: USAMI Kenta <tadsan@zonu.me>
;; Keywords: multimedia
;; URL: https://github.com/zonuexe/deck-slides.el
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1") (yaml "1.2.0"))
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
(require 'json)
(require 'yaml nil t)
(require 'nadvice)
(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))

(defvar deck-slides-lighter " deck")

(defgroup deck-slides nil
  "Creating deck using Markdown and Google Slides."
  :group 'multimedia)

(defcustom deck-slides-executable (executable-find "deck")
  "Path to the deck executable."
  :type 'string)

(defcustom deck-slides-cursor-re (eval-when-compile (rx "`!!'"))
  "A regexp that searches for the position where the cursor moves when inserting."
  :type 'regexp)

(defcustom deck-slides-comment-template "<!-- `!!' -->\n"
  "Templete for Markdown comment block."
  :type 'string)

(defcustom deck-slides-override-markdown-page-commands t
  "Whether to override markdown page commands with standard page commands.
When non-NIL, `markdown-backward-page' and `markdown-forward-page' are
overridden with `backward-page' and `forward-page' respectively when
deck-slides-mode is enabled."
  :type 'boolean)

(defvar-local deck-slides-id nil
  "Google Slides ID for the current buffer.")

(defvar-local deck-slides-layout-names nil
  "Cached list of layout names for the current slide ID.")

(defvar-local deck-slides--last-page-delimiter nil)

(eval-and-compile
  (defconst deck-slides-separator "\n\n---\n"
    "Separator used between slides in the Markdown file."))

;; Utility functions
(defun deck-slides--command-line (&rest args)
  "Build the command line string for the deck executable with ARGS."
  (mapconcat #'shell-quote-argument (cons deck-slides-executable args) " "))

(defun deck-slides--this-buffer-has-frontmatter ()
  "Check if the current buffer has YAML frontmatter at the beginning.
Returns non-nil if the buffer starts with \"---\" on a line by itself."
  (save-excursion
    (save-match-data
      (save-restriction
        (widen)
        (goto-char (point-min))
        (looking-at-p "---\n")))))

(defsubst deck-slides--parse-yaml-string (string)
  "Parse YAML string STRING and return it as a plist.
Returns nil if yaml-parse-string is not available.
The YAML is parsed with object-type \\='plist and sequence-type \\='list."
  (when (fboundp 'yaml-parse-string)
    (yaml-parse-string string :object-type 'plist :sequence-type 'list)))

(defun deck-slides--parse-frontmatter ()
  "Extract YAML frontmatter from the current buffer and parse it.
Returns the parsed YAML as an S-expression, or nil if no frontmatter is found.
The frontmatter is the content between the first \"---\" and the next \"---\" at
the beginning of lines."
  (when (deck-slides--this-buffer-has-frontmatter)
    (save-excursion
      (save-match-data
        (save-restriction
          (widen)
          (goto-char (point-min))
          (forward-line 1) ; Skip the first "---"
          (let ((start (point)))
            (when (re-search-forward "^---$" nil t)
              (let ((end (match-beginning 0)))
                (deck-slides--parse-yaml-string (buffer-substring-no-properties start end))))))))))

(defvar deck-slides--template-pair-cache '())

(defun deck-slides--get-template-pair ()
  "Return `deck-slides-comment-template' split pair.
The result is cached in `deck-slides--template-pair-cache'."
  (or (alist-get deck-slides-comment-template deck-slides--template-pair-cache nil nil #'equal)
      (let* ((parts (save-match-data (split-string deck-slides-comment-template deck-slides-cursor-re t)))
             (result (cl-case (length parts)
                       (1 (list deck-slides-comment-template ""))
                       (2 parts)
                       (t (list (car parts) (string-join (cdr parts) deck-slides-cursor-re))))))
        (push (cons deck-slides-comment-template result) deck-slides--template-pair-cache)
        result)))

(eval-and-compile
  (defconst deck-slides-re-comment-start (rx bol "<!--" (+ (syntax whitespace))))
  (defconst deck-slides-re-comment-end (rx (+ (syntax whitespace)) "-->")))

(defmacro deck-slides--with-page-beginning (body)
  "Execute BODY with point positioned at the beginning of the current page.
This macro saves excursion and restriction, widens the buffer,
moves to the beginning of the current page, then executes BODY."
  `(save-excursion
     (save-restriction
       (widen)
       (backward-page)
       ,body)))

(defun deck-slides--get-page-config-with-point ()
  "Get page configuration from current page as a plist.
Searches for JSON objects in HTML comments between `<!--' and `-->'."
  (save-match-data
    (let ((bound (save-excursion
                   (forward-page)
                   (point))))
      (cl-loop
       while (re-search-forward deck-slides-re-comment-start bound t)
       thereis
       (let* ((beg (match-end 0))
              (end (save-excursion (when (re-search-forward deck-slides-re-comment-end bound t)
                                     (match-beginning 0))))
              comment-content)
         (when end
           (setq comment-content
                 (string-trim (buffer-substring-no-properties beg end)))
           (if (string= comment-content "")
               (cl-values beg end nil)
             (when (and (string-prefix-p "{" comment-content)
                        (string-suffix-p "}" comment-content))
               (condition-case nil
                   (cl-values
                    beg end
                    (json-parse-string comment-content :object-type 'plist))
                 (error nil))))))))))

(defsubst deck-slides--get-page-config ()
  "Get page configuration from the current page as a plist.
This function moves to the beginning of the current page and searches
for JSON objects in HTML comments. Returns the parsed JSON as a plist,
or nil if no valid JSON configuration is found."
  (nth 2 (deck-slides--with-page-beginning (deck-slides--get-page-config-with-point))))

(defun deck-slides--update-page-config (new-plist &optional del-keys)
  "Update page configuration by modifying the JSON object in HTML comments.
NEW-PLIST is a plist of key-value pairs to add or update.
DEL-KEYS is an optional list of keys to remove from the configuration.
The function modifies the JSON object in place within the HTML comment."
  (deck-slides--with-page-beginning
   (if-let* ((page-config (deck-slides--get-page-config-with-point)))
       (cl-multiple-value-bind (beg end json-value) page-config
         (message "before: %S new-plist: %S" json-value new-plist)
         (goto-char beg)
         (delete-region beg end)
         (cl-loop for (key value) on new-plist by #'cddr
                  do (setf (plist-get json-value key) value))
         (cl-loop for key in del-keys
                  do (cl-remf json-value key))
         (message "after: %S" json-value)
         (insert (json-encode-plist json-value)))
     (deck-slides-insert-comment)
     (insert (json-encode-plist new-plist)))))

;; Internal functions
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
    (if-let* ((current-id (plist-get (deck-slides--parse-frontmatter) :presentationID)))
        (setq deck-slides-id current-id)
      (let ((new-id (read-string "Input Google Slides presentation ID or URL: ")))
        (save-match-data
          (when (string-match deck-slides--presentation-url-pattern new-id)
            (setq new-id (match-string-no-properties 1 new-id))))
        (unless (string-match-p deck-slides--presentation-id-pattern new-id)
          (user-error "Invalid presentation ID"))
        (setq deck-slides-id new-id))))
  deck-slides-id)

(defun deck-slides--fetch-ls-layous (id)
  "Fetch layout names for the given slide ID using the deck command."
  (save-match-data
    (thread-first
      (deck-slides--command-line "ls-layouts" "--presentation-id" id)
      (shell-command-to-string)
      (string-trim-right)
      (split-string))))

(defun deck-slides-layout-list (id &optional force-update)
  "Return layout names by slide ID.

`ls-layouts' results are cached by default.
When FORCE-UPDATE is non-NIL, the cache is refreshed."
  (when (or force-update (null deck-slides-layout-names))
    (setq deck-slides-layout-names (deck-slides--fetch-ls-layous id)))
  deck-slides-layout-names)

;; Commands
;;;###autoload
(defun deck-slides-apply (id)
  "Apply all slides change to Google Slides ID.
When called non-interactively, ID must be provided."
  (interactive (list (deck-slides-current-buffer-id-and-register)))
  (let ((default-directory (expand-file-name "~")))
    (message "%s" (shell-command-to-string
                   (deck-slides--command-line "apply"buffer-file-name "--presentation-id" id)))))

;;;###autoload
(defun deck-slides-apply-watch (id)
  "Apply all slides change to Google Slides ID.
When called non-interactively, ID must be provided."
  (interactive (list (deck-slides-current-buffer-id-and-register)))
  (let ((default-directory (expand-file-name "~")))
    (compile (deck-slides--command-line "apply" buffer-file-name "--watch" "--presentation-id" id))))

;;;###autoload
(defun deck-slides-new (file base title)
  "Create a new presentation using the `deck' command.
FILE is the path to the new Markdown file for the presentation.
BASE is an optional base presentation ID to copy from.
TITLE is the title for the new presentation.
When called interactively, prompts for file path, base ID, and title."
  (interactive (list (expand-file-name
                      (read-file-name "Select new Markdown file for presentation: "
                                      default-directory nil nil))
                     (unless (eq 1 (prefix-numeric-value current-prefix-arg))
                       (read-string "Enter base presentation ID: "))
                     (read-string "Enter presentation title: ")))
  (let* ((args (append (list file)
                       (when base
                         (list "--base" base))
                       (unless (member title '(nil ""))
                         (list "--title" title))))
         (output (shell-command-to-string (apply #'deck-slides--command-line "new" (delete #'null args)))))
    (find-file file)
    (message "%s" output)))

;;;###autoload
(defun deck-slides-ls-layouts (id &optional force-update)
  "Show layout names by slide ID.

`ls-layouts' results are cached by default.
When FORCE-UPDATE is non-NIL, the cache is refreshed."
  (interactive (list (deck-slides-current-buffer-id-and-register)
                     (not (null current-prefix-arg))))
  (message "layout-names: %S" (deck-slides-layout-list id force-update)))

;;;###autoload
(defun deck-slides-set-page-layout (layout-name)
  "Set the LAYOUT-NAME for the current page.
When called interactively, prompts for the layout name from available options.
The layout name is set as a JSON comment in the page configuration."
  (interactive (let* ((id (deck-slides-current-buffer-id-and-register))
                      (layout-names (deck-slides-layout-list id)))
                 (list (when layout-names (completing-read "Choose layout name: " layout-names)))))
  (deck-slides--update-page-config (list :layout layout-name)))

;;;###autoload
(defun deck-slides-insert-page (layout-name)
  "Insert a slide separator with the specified LAYOUT-NAME.
When called interactively, prompts for the layout name from available options.
The layout name is inserted as a JSON comment after the slide separator."
  (interactive (let* ((id (deck-slides-current-buffer-id-and-register))
                      (layout-names (unless (eq 1 (prefix-numeric-value current-prefix-arg))
                                      (deck-slides-layout-list id))))
                 (list (when layout-names (completing-read "Choose layout name: " layout-names)))))
  (beginning-of-line)
  (unless (looking-at-p "---\n")
    (end-of-line))
  (let* ((separator deck-slides-separator))
    (cond
     ((looking-back "\n\n" 2) (setq separator (string-trim-left separator)))
     ((looking-back "\n" 1) (setq separator (concat "\n" (string-trim-left separator)))))
    (when (looking-at-p "\n\n")
      (setq separator (string-trim-right separator)))
    (insert separator)
    (when layout-name
      (insert "\n")
      (deck-slides-set-page-layout layout-name))))

;;;###autoload
(defun deck-slides-duplicate-page ()
  "Duplicate the current page with its content.
Creates a copy of the current page by copying all content from the current
page separator to the next page separator (or end of buffer).
The duplicated content is inserted after the current page."
  (interactive)
  (unless (or (looking-back deck-slides-separator 3)
              (save-excursion
                (beginning-of-line)
                (looking-at-p "---\n")))
    (backward-page))
  (let* ((beg (point)))
    (forward-page)
    (save-excursion
      (insert (buffer-substring beg (point))))))

;;;###autoload
(defun deck-slides-insert-comment ()
  "Insert a comment template at point with cursor positioned at the cursor marker.
The template is split by `deck-slides-cursor-re' and the cursor is positioned
between the head and tail parts."
  (interactive)
  (cl-multiple-value-bind (head tail)
      (deck-slides--get-template-pair)
    (insert head)
    (save-excursion
      (insert tail))))

;;;###autoload
(defun deck-slides-toggle-freeze-page ()
  "Toggle the freeze state of the current page.
If the page has `{\"freeze\": true}', sets it to false.
If the page has `{\"freeze\": false}' or no freeze key, sets it to true."
  (interactive)
  (let* ((page-config (deck-slides--get-page-config))
         (current-freeze (plist-get page-config :freeze))
         (new-freeze (if (eq current-freeze t) :false t)))
    (deck-slides--update-page-config (list :freeze new-freeze))))

;;;###autoload
(defun deck-slides-toggle-ignore-page ()
  "Toggle the ignore state of the current page.
If the page has `{\"ignore\": true}', sets it to false.
If the page has `{\"ignore\": false}' or no ignore key, sets it to true."
  (interactive)
  (let* ((page-config (deck-slides--get-page-config))
         (current-ignore (plist-get page-config :ignore))
         (new-ignore (if (eq current-ignore t) :false t)))
    (deck-slides--update-page-config (list :ignore new-ignore))))

;;;###autoload
(defun deck-slides-toggle-skip-page ()
  "Toggle the skip state of the current page.
If the page has `{\"skip\": true}', sets it to false.
If the page has `{\"skip\": false}' or no skip key, sets it to true."
  (interactive)
  (let* ((page-config (deck-slides--get-page-config))
         (current-skip (plist-get page-config :skip))
         (new-skip (if (eq current-skip t) :false t)))
    (deck-slides--update-page-config (list :skip new-skip))))

;;;###autoload
(defun deck-slides-move-page-up ()
  "Move the current page up by swapping with the previous page."
  (interactive)
  (let ((current-line-num (line-number-at-pos))
        relative-line-num)
    (when (looking-back deck-slides-separator 1)
      (forward-line 1))
    (deck-slides--with-page-beginning
     (let* ((current-beg (point))
            (current-end (save-excursion (forward-page) (point)))
            (current-content (buffer-substring-no-properties current-beg current-end)))
       (setq relative-line-num (- current-line-num (line-number-at-pos)))
       (goto-char current-beg)
       (delete-region current-beg current-end)
       (backward-page)
       (insert current-content)))
    (backward-page)
    (backward-page)
    (forward-line relative-line-num)))

;;;###autoload
(defun deck-slides-move-page-down ()
  "Move the current page down by swapping with the next page."
  (interactive)
  (let ((current-line-num (line-number-at-pos))
        relative-line-num)
    (when (looking-back deck-slides-separator 1)
      (forward-line 1))
    (deck-slides--with-page-beginning
     (let* ((current-beg (point))
            (current-end (save-excursion (forward-page) (point)))
            (current-content (buffer-substring-no-properties current-beg current-end)))
       (setq relative-line-num (- current-line-num (line-number-at-pos)))
       (goto-char current-end)
       (forward-page)
       (delete-region current-beg current-end)
       (insert current-content)))
    (forward-page)
    (forward-line relative-line-num)))

;;;###autoload
(defun deck-slides-find-credentials-json ()
  "Find deck `credentials.json' file."
  (interactive)
  (find-file (expand-file-name "deck/credentials.json" (xdg-data-home))))

;;;###autoload
(defun deck-slides-find-config-files ()
  "Find deck configuration files."
  (interactive)
  (let ((dir (expand-file-name "deck" (xdg-config-home))))
    (unless (file-directory-p dir)
      (mkdir dir t))
    (find-file (expand-file-name "" (xdg-data-home)))))

;;;###autoload
(defun deck-slides-open-browser (&optional id)
  "Open Google Slides presentation in browser by ID."
  (interactive)
  (let ((args (append (list deck-slides-executable "open")
                      (if id
                          (list "--presentation-id" id)
                        (list buffer-file-name)))))
    (message "%s" (shell-command-to-string (mapconcat #'shell-quote-argument args " ")))))

;;;###autoload
(defun deck-slides-doctor ()
  "Run `deck doctor' command in a comint buffer to diagnose deck configuration."
  (interactive)
  (compile (mapconcat #'shell-quote-argument (list deck-slides-executable "doctor") " ")))

;; Helper functions for advices
;;;###autoload
(defun deck-slides-auto-activate-mode ()
  "Auto-activate deck-slides-mode for files with presentationID in frontmatter."
  (when (and (derived-mode-p 'markdown-mode)
             (save-excursion
               (goto-char (point-min))
               (looking-at-p "---\n.*presentationID:")))
    (deck-slides-mode)))

;; Minor mode
(defvar-keymap deck-slides-map
  :doc "Keymap for deck-slides-mode."
  "C-c RET" #'deck-slides-insert-page
  "C-c C-c RET" #'deck-slides-insert-page
  "C-c C-c d" #'deck-slides-duplicate-page
  "C-c C-c f" #'deck-slides-toggle-freeze-page
  "C-c C-c i" #'deck-slides-toggle-ignore-page
  "C-c C-c s" #'deck-slides-toggle-skip-page
  "C-c C-c C-v" #'deck-slides-move-page-down
  "C-c C-c M-v" #'deck-slides-move-page-up
  "C-c C-c <down>" #'deck-slides-move-page-down
  "C-c C-c <up>" #'deck-slides-move-page-up
  "C-c C-c ;" #'deck-slides-insert-comment)

;;;###autoload
(define-minor-mode deck-slides-mode
  "Minor mode for interacting with deck CLI.  Enables auto-apply on idle."
  :keymap deck-slides-map
  :lighter deck-slides-lighter
  (if deck-slides-mode
      (progn
        (setq-local deck-slides--last-page-delimiter page-delimiter)
        (setq-local page-delimiter deck-slides-separator)
        (when deck-slides-override-markdown-page-commands
          (advice-add 'markdown-backward-page :override #'backward-page)
          (advice-add 'markdown-forward-page :override #'forward-page)))
    (when deck-slides--last-page-delimiter
      (setq-local page-delimiter deck-slides--last-page-delimiter)
      (setq-local deck-slides--last-page-delimiter nil))
    (when deck-slides-override-markdown-page-commands
      (advice-remove 'markdown-backward-page #'backward-page)
      (advice-remove 'markdown-forward-page #'forward-page))))

(provide 'deck-slides)
;;; deck-slides.el ends here
