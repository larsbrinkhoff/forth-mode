## Programming language mode for Forth.

[![MELPA](https://melpa.org/packages/forth-mode-badge.svg)]
(https://melpa.org/#/forth-mode)

Features in progress:

- Recognises definitions and moves by balanced expressions.
- Interact with a Forth session: enter commands, load files, evalutate
  expressions.
- Display stack comment when moving the cursor over a word.
- Edit block files.

Planned:

- Tab completion.
- Query a running Forth about words, search order, etc.

### Installation

MELPA:  

    M-x package-install forth-mode

Manual:

    git clone http://github.com/larsbrinkhoff/forth-mode DIR
    
    # Add to .emacs
    (pushnew "DIR" load-path)
    (require 'forth-mode)
    (require 'forth-block-mode)
    (require 'forth-interacton-mode)

### Usage

To enable Forth major mode, type `M-x forth-mode`.  The file
extensions `.f`, `.fs`, `.fth`, and `.4th` are recognised
automatically.

To start an interactive Forth session, type `M-x run-forth`.
