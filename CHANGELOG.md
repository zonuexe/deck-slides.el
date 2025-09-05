# Changelog

All notable changes of the `deck-slides.el` are documented in this file using the [Keep a Changelog](https://keepachangelog.com/) principles.

<!--
## Unreleased

### Changed
-->

## [0.1.0] - 2025-09-05

### Added

 * Implement following commands:
   * `deck-slides-find-config-files`
   * `deck-slides-open-browser`
   * `deck-slides-doctor`
 * Add page-oriented editing commands:
   * `deck-slides-insert-page`
   * `deck-slides-insert-comment`
 * Add page configuration modify commands:
   * `deck-slides-toggle-freeze-page`
   * `deck-slides-toggle-ignore-page`
   * `deck-slides-toggle-skip-page`
   * `deck-slides-set-page-layout`
 * Add `deck-slides-override-markdown-page-commands` custom variable.
 * Add `deck-slides-override-markdown-page-commands` custom variable.

### Changed

 * Follow changes to the `--presentation-id` argument. ([k1LoW/deck#189])
 * Supports reading configuration from front matter YAML.
 * [deck-slides-mode] the `markdown-(backward|forward)-page` command is replaced with the `(backward|forward)-page` command.
   * Set `deck-slides-override-markdown-page-commands` to `NIL` to opt out.

### Removed

* Remove `deck-slides-apply-only-current-page` as `apply --watch` has been significantly improved.

[k1LoW/deck#189]: https://github.com/k1LoW/deck/pull/189

## [0.0.3] - 2025-06-23

### Added

 * Add `deck-slides-cache-file` custom variable to store slide IDs. ([#1])
 * Implement `deck-slides-apply-watch` command. ([#2])
 * Add `deck-slides-code-block-to-image-command` custom variable. ([#2])

### Changed

 * The deck-slides-get-id command now transparently caches slide IDs per file. ([#1])

[#1]: https://github.com/zonuexe/deck-slides.el/pull/1
[#2]: https://github.com/zonuexe/deck-slides.el/pull/2

## [0.0.2] - 2025-05-25

### Added

 * Implement `deck-slides-find-credentials-json` command

### Fixed

 * Fix a bug where `deck-slides--fetch-ls-layous` corrupted match-data.

## [0.0.1] - 2025-04-25

### Added

 * Implement following commands:
   * `deck-slides-apply`
   * `deck-slides-apply-only-current-page`
   * `deck-slides-ls-layouts`
 * Implement `deck-slides-mode`
