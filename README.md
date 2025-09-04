# deck-slides.el

Emacs integration of [k1LoW/deck] command.

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


## Configuration

> [!CAUTION]
> These Emacs-specific settings will be removed in the next release.
> They can be set in front matter or in the configuration file.

### Cache File

The `deck-slides-cache-file` variable controls where Google Slides IDs associated with Markdown files are cached. By default, it stores the cache in `deck-slides.eld` in your Emacs user directory.

You can customize the cache file location:

```elisp
(setopt deck-slides-cache-file "~/.config/emacs/deck-slides-cache.eld")
```

To disable caching entirely, set it to `nil`:

```elisp
(setopt deck-slides-cache-file nil)
```

### Code Blocks to Images

You can configure `deck-slides-code-block-to-image-command` to automatically convert code blocks into images when applying slides to Google Slides.

This feature uses the same functionality as the [deck CLI's code blocks to images feature](https://github.com/k1LoW/deck?tab=readme-ov-file#code-blocks-to-images).

Example configuration:

```elisp
(setopt deck-slides-code-block-to-image-command "...")
```

This setting can also be saved in a `.dir-locals.el` file for directory-specific configuration.

```elisp
((gfm-mode . ((deck-slides-code-block-to-image-command . "if [ {{lang}} = \"mermaid\" ]; then mmdc -i - -o output.png --quet; else silicon {{lang == \"\" ? \"-l md\" : \"-l \" + lang}} --background '#fff0' --output output.png; fi; cat output.png"))))
```

> [!WARNING]
> This string is expanded as a shell string by [expr-lang](https://expr-lang.org/).  ***Please set it at your own risk.***

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
