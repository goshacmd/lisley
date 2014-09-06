# Lisley

Clojure-like Lisp parser/interpreter implemented in Haskell.

```clojure
$ lisley
> (map #(* 42) [1 2 3])
(42 84 126)
> (apply +)
```

### To Do

* proper scoping
* multi-arity functions
* multi-argument anonymous function literal
* proper vectors; sets, hash maps
* `filter`, etc...
* multi-arity `+`, `-`, etc...
* primitive IO functions
* macros

## License

[MIT](LICENSE).
