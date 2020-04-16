# GAS & eqaf

`eqaf` is a small library which wants to provide a /constant-time/ equal function to protect such computation
against a timing attack. Currently, the distribution comes with a tool `check/check.exe` which benchmarks
our function and introspects results.

For more information about this tool, you should check the README.md of `eqaf`.

However, as noticed in our Continuous Integration, we can not fully rely on this tool where recorded samples
are disturbed by the underlying scheduler of the operating-system, the virtualization, the CPU cache, etc.
From all of these parameters, it's hard to really know time spend by our `equal` function.

By this way, it's hard to _check_ if our equal function respects our predicate: the _constant-time_.

## Introspection of the GAS assembler

By this fact, when we want to improve `eqaf`, we mostly introspect GAS assembler and see if any `jump` don't
leak any information (such as which character starts to differ from the secret value). However, this kind of
development requires that user trusts on us - and should not trust on the CI as we don't really trust on the
Continuous Integration.

This _little_ project wants to go further about our introspection and it provides a way to parse and execute
a GAS assembler. At the end, we want to simulate the execution of our `equal` function. By this way, we are
able to record correctly samples without noises (such as the cache).

## Fuzz it

The goal is to fuzz the execution of our `equal` function into our Virtual Machine and see that for any
random inputs, we surely spend a theoric and expected amount of _tick_. An GAS instruction spends 1 tick.

## Critics

However, even if this approach is interesting for us, it __does not__ assert our predicates. At least, we need
to trust on our Virtual Machine which is naive and smaller than a real CPU. By this fact, abstract simulation can
help us but it's not a proof that `eqaf` provides a /constant-time/ function.

The current project is a bit huge comparing to `eqaf` and the trade-off is not really interesting to integrate
such tool into our distribution of `eqaf`. However, the provided binary is useful enough to go further. It's why
it's not a part of the initial `eqaf` distribution.

## How to use it?

Currently, the project is small and only test `Eqaf.equal` (the first implemented function). You must `pin` the project and it provides a tool, `gas`, which launchs the fuzzer on `Eqaf.equal`:

```sh
$ opam pin add eqaf-gas https://github.com/dinosaure/gas-eqaf.git
$ opam install eqaf-gas
$ gas
```

# Conclusion

At least, the project is interesting but it still is experimental and does not respect any release cycle or
roadmap. In others words, it's a toy.
