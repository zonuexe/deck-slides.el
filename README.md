# deck-slides.el

Emacs integration of [k1LoW/deck] command.

## Install

Evaluating the following Lisp code will install the latest package:

```elisp
(package-vc-install
'(deck-slides :url "https://github.com/zonuexe/deck-slides.el.git"
              :main-file "deck-slides.el"))
```

As a developer, I have only checked it on Emacs 30.2, but it may work on earlier versions, so if you are interested, please send Pull Requests.

## Usage

### Edit Credentials File

To use the `deck` command, you need to set up your own developer project from the Google Cloud console and save it as `credentials.json`. You can visit this file from Emacs using `M-x deck-slides-find-credentials-json`.

### Apply Markdown to Google Slides

Open the Markdown file you want to apply as slides and run `M-x deck-slides-apply` or `M-x deck-slides-apply-watch`. When running for the first time, it's recommended to verify that `deck apply` works correctly in the terminal first.

> [!TIP]
> Slide pages must be separated by `---` on independent lines. Other symbols or characters of different lengths are not recognized as page separators.

The watch command process runs in the `*compilation*` buffer. To stop watching, kill the `*compilation*` buffer.

### Check Slide Layout Names

Run `M-x deck-slides-ls-layouts` to list the layout names of slides corresponding to the file you're editing.

> [!TIP]
> The results of this command are cached as buffer-local variables. The cache is discarded when you run `C-u M-x deck-slides-ls-layouts` or kill the buffer.

## Commands

### Core Commands

#### `deck-slides-apply`
Apply all slides changes to Google Slides. When called interactively, prompts for the presentation ID if not already set.

#### `deck-slides-apply-watch`
Apply slides changes and watch for file modifications. Runs in the `*compilation*` buffer. Use `C-g` to stop watching.

#### `deck-slides-ls-layouts`
List available layout names for the current presentation. Results are cached by default. Use `C-u` prefix to force refresh.

### Page Management Commands

#### `deck-slides-insert-page`
Insert a new slide separator (`---`) at the current position. When called interactively, prompts for a layout name to apply to the new page.

#### `M-x deck-slides-duplicate-page`
Duplicate the current page with its content.

#### `deck-slides-set-page-layout`
Set the layout for the current page. When called interactively, prompts for a layout name from available options.

#### `deck-slides-insert-comment`
Insert a comment template at the current point with cursor positioned at the cursor marker (`!!`).

### Page Configuration Commands

#### `deck-slides-toggle-freeze-page`
Toggle the freeze state of the current page. If the page has `{"freeze": true}`, sets it to false. If the page has `{"freeze": false}` or no freeze key, sets it to true.

#### `deck-slides-toggle-ignore-page`
Toggle the ignore state of the current page. If the page has `{"ignore": true}`, sets it to false. If the page has `{"ignore": false}` or no ignore key, sets it to true.

#### `deck-slides-toggle-skip-page`
Toggle the skip state of the current page. If the page has `{"skip": true}`, sets it to false. If the page has `{"skip": false}` or no skip key, sets it to true.

### Utility Commands

#### `deck-slides-find-credentials-json`
Open the deck credentials.json file for editing.

#### `deck-slides-find-config-files`
Open the deck configuration directory.

#### `deck-slides-open-browser`
Open the current Google Slides presentation in your default browser.

#### `deck-slides-doctor`
Run `deck doctor` command in a comint buffer to diagnose deck configuration issues.

### Key Bindings

When `deck-slides-mode` is enabled, the following key bindings are available:

- `C-c RET` - Insert a new page (`deck-slides-insert-page`)
- `C-c C-c RET` - Insert a new page (`deck-slides-insert-page`)
- `C-c C-c f` - Toggle freeze state (`deck-slides-toggle-freeze-page`)
- `C-c C-c i` - Toggle ignore state (`deck-slides-toggle-ignore-page`)
- `C-c C-c s` - Toggle skip state (`deck-slides-toggle-skip-page`)
- `C-c C-c ;` - Insert comment template (`deck-slides-insert-comment`)

## 💡Tips

### Auto-activate deck-slides-mode

You can automatically activate `deck-slides-mode` when opening Markdown files that have a presentation ID in their frontmatter.

Add this advice to your Emacs configuration:

```elisp
(with-eval-after-load 'markdown-mode
  (advice-add 'markdown-mode :after #'deck-slides-auto-activate-mode))
```

This will automatically activate `deck-slides-mode` for any Markdown file that starts with:

```yaml
---
presentationID:
```

### Auto-open browser before watch mode

You can automatically open the Google Slides presentation in your browser when starting watch mode.

Add this advice to your Emacs configuration:

```elisp
(with-eval-after-load 'deck-slides
  (advice-add 'deck-slides-apply-watch :before #'deck-slides-open-browser-current-buffer))
```

This will automatically open the presentation in your browser whenever you start `deck-slides-apply-watch`, making it easier to see the changes in real-time.

## Copyright

This package is licensed under the GPL.

        deck-slides.el - Creating deck using Markdown and Google Slides
        Copyright (C) 2025  USAMI Kenta

        This program is free software: you can redistribute it and/or modify
        it under the terms of the GNU General Public License as published by
        the Free Software Foundation, either version 3 of the License, or
        (at your option) any later version.

        This program is distributed in the hope that it will be useful,
        but WITHOUT ANY WARRANTY; without even the implied warranty of
        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
        GNU General Public License for more details.

        You should have received a copy of the GNU General Public License
        along with this program.  If not, see <https://www.gnu.org/licenses/>.

[k1LoW/deck]: https://github.com/k1LoW/deck
