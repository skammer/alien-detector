# A Datastar + http-kit starter

## Running the example

- repl:

```bash
clojure -M:repl -m nrepl.cmdline --middleware "[cider.nrepl/cider-middleware]"
```

- main:

```bash
clojure -M -m hello-httpkit
```

# How to use

* edit invader definitions in ./known-invaders and changes will propagate to
  connected instances
* write to ./radar-scans/000.txt to get update current radar readings

# Issues

* It reconnects when tab becomes active (either you switch tabs, maximize
  a minimized window or wake up your laptop). Reconnection itself is not
  an issue, but it does trigger sounds and it's mildly annoying.
* fully saturated with field (all cells are true) will trigger a discovery of any
  invader. Kinda makes sense, since anything can fit in there, but it's also
  an edge case which might need to be handled differently in real system.
  Maybe a separate radar jamming detector?
