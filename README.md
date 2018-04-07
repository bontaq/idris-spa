### Building

### Emacs

- Need to update emacs exec path with:
    (setenv "PATH" (concat (getenv "PATH") ":/home/ian/.cabal/bin"))
    (setq exec-path (append exec-path '("/home/ian/.cabal/bin")))
