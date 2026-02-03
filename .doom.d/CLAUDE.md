# Doom Emacs Configuration Guide

## Build/Test/Lint Commands
- **Sync Doom**: `doom sync` - Run after modifying packages.el or init.el
- **Reload Config**: `M-x doom/reload` - Reload configuration without restarting
- **Format/Indent**: Use clang-format for C/C++ (automatic with hook setup)
- **Compile**: `C-c RET` in C/C++ modes to run recompile
- **Debug**: Use dap-mode for debugging with preconfigured templates

## Code Style Guidelines
- **Indentation**: 4 spaces basic offset, tab-width set to 8
- **Line Length**: 120 characters (fill-column)
- **C/C++ Style**: Auto-formatted with clang-format
- **Naming Convention**: Standard Emacs lisp naming (hyphen-separated)
- **Error Handling**: Use `ignore-errors` for non-critical operations
- **Comments**: Keep comments concise and descriptive

## Project Organization
- **init.el**: Controls Doom modules and their load order
- **config.org**: Personal configuration settings in org format
- **packages.el**: Additional package declarations
- **+functions.el**: Custom helper functions
- **snippets/**: Code templates for various modes

## Environment
- **Emacs Config Directory**: `~/.config/emacs`
- **Doom Config Directory**: `/Users/andreas/tools/.doom.d`

This configuration emphasizes AI first development using claude code.
