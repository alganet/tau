ταυ
===

A friendly emacs distribution (under development) for desktop and terminal.

#### GUI

![](https://i.imgur.com/UpDRBrB.png)

#### Terminal (with [supporting theme](https://github.com/arcticicestudio/nord-emacs#installation))

![](https://i.imgur.com/WI7pkNx.png)

## Installation

 1. Install Emacs 25 or later.
 2. `git clone https://github.com/alganet/tau/ ~/.emacs.d`
 3. Run `emacs`. It will install the remaining packages on the first run.

## Goals

 - **Recent keybinding standards**
 
   Emacs was here before any keybinding standards across modern editors. We try
   to comply with Sublime, Atom and VSCode standards as a default.
   
 - **GUI and Terminal interfaces**
 
   As any Emacs, it works both from the terminal and a dedicated GUI. ταυ on
   the terminal has almost the same keybindings as the GUI counterpart, making
   it very friendly.
   
 - **Features and Languages**
 
   Shipped with popular languages and features such as multiple cursors and go
   to anywhere.
   
 - **Nice theme and UI**
 
   We care about appearance.

## What is missing?

 - OS X keybindings (Ctrl ones do work on Mac though).
 - Several keybindings to standard emacs or plugin commands.
 - Better TAB handling on auto-complete minibuffers.
 - More mileage.

## What does work?

 - Multiple Cursors (Ctrl+D, ESC to quit)
 - Go to anywhere (inside a git project, Ctrl+P)
 - Open file or folder (Ctrl+O)
 - Find in project (inside a git project, Ctrl+Shift+F)
 - Find in current file (Ctrl+F)
 - Switch Tabs (Ctrl+Tab or Ctrl+T)
 - Switch Tabs Back (Ctrl+Shift+Tab or Ctrl+R)
 - Execute command (Ctrl+K, Ctrl+P or Ctrl+Shift+P)
 - PHP, HTML+CSS+JS, JavaScript, Python, Markdown, Shell Script, Lisp, Go.
