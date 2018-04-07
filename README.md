### About

### Building

```bash
idris --codegen Javascript -p idrisscript Main.idr -o Main.html
```

### Emacs

* Need to update emacs exec path with:

```elisp
    (setenv "PATH" (concat (getenv "PATH") ":/home/ian/.cabal/bin"))
    (setq exec-path (append exec-path '("/home/ian/.cabal/bin")))
```
