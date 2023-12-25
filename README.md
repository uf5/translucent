# Translucent
Translucent is a Lisp to Python transpiler written in Haskell.
Please note that this is a rewrite branch. For the old version (which has more features but has slightly uglier code) go visit the "master" branch.
## Note
The transpiler itself translates Lisp code into [Python AST](https://docs.python.org/3/library/ast.html). Then it serializes the resulting AST into JSON, which can then be used to reconstruct the Python program.
### Usage:
```sh
cabal run | tail -n 1 | python3 py/reconstruct_from_json.py
# or if you are using nix
nix run | tail -n 1 | python3 py/reconstruct_from_json.py
```
## Example
```clojure
(if (do (print "a")
        (print "b")
        (print "c")
        (= 1 1))
    (do (print "yes")
        (print (do (print "d")
                   (print "e")
                   "ok")))
    (print "no"))
```
Will turn into
```python
print('a')
print('b')
print('c')
if 1 == 1:
    print('yes')
    print('d')
    print('e')
    print('ok')
else:
    print('no')
```
