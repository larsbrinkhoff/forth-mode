## Programming language mode for Forth.

[![Build Status](https://travis-ci.org/larsbrinkhoff/forth-mode.svg)](https://travis-ci.org/larsbrinkhoff/forth-mode)
[![MELPA](https://melpa.org/packages/forth-mode-badge.svg)](https://melpa.org/#/forth-mode)

Features in progress:

- Recognises definitions and moves by balanced expressions.
- Interact with a Forth session: enter commands, load files, evalutate
  expressions.
- Display stack comment when moving the cursor over a word.
- Edit block files.
- Tab completion.
- Query a running Forth about words, search order, etc.

### Installation

MELPA:  

    M-x package-install forth-mode

Manual:

    git clone http://github.com/larsbrinkhoff/forth-mode DIR
    
    # Add to .emacs
    (add-to-list 'load-path "DIR")
    (require 'forth-mode)
    (require 'forth-block-mode)
    (require 'forth-interaction-mode)

### Usage

To enable Forth major mode, type `M-x forth-mode`.  The file
extensions `.f`, `.fs`, `.fth`, and `.4th` are recognised
automatically.

To start an interactive Forth session, type `M-x run-forth`.

Key bindings:

- `C-M-a`, `C-M-e` - beginning / end of colon definition.
- `C-M-f`, `C-M-b` - forward / backward expression (not very useful yet).
- `C-M-h` - mark colon definition.
- `C-c C-l` - load file.
- `C-c C-r` - evaluate region.
- `C-c C-k` - kill interactive Forth.
- `M-TAB`, `C-M-i` - complete-symbol.
