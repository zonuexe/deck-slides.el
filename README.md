# deck-slides.el

Emacs integration of [k1LoW/deck] command.

## Configuration

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
