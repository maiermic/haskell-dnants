# haskell-dnants

A simple ant colony simulation to teach and learn state machines and
a cellular automata. A Haskell port of [this][cpp-dnants] C++ implementation. 

Use [Stack][Stack] to build and run the project, for example:

```
stack build
stack exec haskell-dnants-exe -- --grid=21 -t simple -t simple -t simple
```


[Stack]: https://docs.haskellstack.org/en/stable/README/
[cpp-dnants]:https://github.com/fuchsto/dnants/