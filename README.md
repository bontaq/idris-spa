### About

### Building

Before you can build `Main.idr`, make sure the deps are installed:
```bash
git submodule init
git submodule update
cd deps/IdrisScript
make install
```
then
```bash
idris --codegen Javascript -p idrisscript Main.idr -o Main.html
```
you may also have to update your `PATH` to include wherever idrisscript was installed

### Emacs

* Need to update emacs exec path with:

```elisp
    (setenv "PATH" (concat (getenv "PATH") ":/home/ian/.cabal/bin"))
    (setq exec-path (append exec-path '("/home/ian/.cabal/bin")))
```
