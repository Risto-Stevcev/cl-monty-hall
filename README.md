# cl-monty-hall

Tests the solution to the [Monty Hall Problem](https://en.m.wikipedia.org/wiki/Monty_Hall_problem)
using Monte Carlo

## Setup

```sh
$ make
```

## Results

```lisp
(run 3 :num-rounds 1000000)
;;(:KEEP ((T . 334409) (NIL . 665591)))
;;(:SWITCH ((T . 666442) (NIL . 333558)))

(run 4 :num-rounds 1000000)
;;(:KEEP ((T . 250303) (NIL . 749697)))
;;(:SWITCH ((T . 749986) (NIL . 250014)))

(run 5 :num-rounds 1000000)
;;(:KEEP ((T . 200124) (NIL . 799876)))
;;(:SWITCH ((T . 799602) (NIL . 200398)))

(run 20 :num-rounds 1000000)
;;(:KEEP ((T . 49878) (NIL . 950122)))
;;(:SWITCH ((T . 949733) (NIL . 50267)))

(run 200 :num-rounds 1000000)
;;(:KEEP ((T . 4897) (NIL . 995103)))
;;(:SWITCH ((T . 994996) (NIL . 5004)))
```

## License

See [LICENSE](./LICENSE)
