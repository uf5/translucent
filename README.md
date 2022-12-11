# Translucent
WIP Clojure-flavored Lisp -> Python transpiler.
## Note
The transpiler itself translates Lisp code into [Python AST](https://docs.python.org/3/library/ast.html). Then it serializes it into JSON, which can then be used to reconstruct the Python program.
### Usage:
```sh
stack run | tail -n 1 | python3 py/reconstruct_from_json.py
```
## Example
TODO
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
## Quirks
### Keywords
Because of the way keywords work in Python and in my Lisp dialect, they can be used as strings everywhere except for function arguments.
Keywords in functions must have a value associated with them. This value can also be a string represented by a keyword.
Examples:
```clojure
{:key1 :value1 :key2 :value}
```
and
```clojure
(print "hello " :end :world)
```
are fine, but
```clojure
(print :hello)
```
will result in
```
(print :hi)
1:1:
  |
1 | (print :hi)
  | ^
Keyword expression has no value
```
