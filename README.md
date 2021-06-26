# cl-monty-hall

Tests the solution to the [Monty Hall Problem](https://en.m.wikipedia.org/wiki/Monty_Hall_problem) using Monte Carlo

## Setup

```sh
$ make
```

## Results

```lisp
; The probability looks like it's how it's stated in the original problem,
; 2/3 change you get the car if you switch and 1/3 change if you don't switch:
(run 3 :num-rounds 1000000)
;(:KEEP ((T . 333019) (NIL . 666981)))
;(:SWITCH ((T . 666849) (NIL . 333151)))

; However, the difference between choosing the door and not doesn't make much difference at a large
; enough number:
(run 4 :num-rounds 1000000)
;(:KEEP ((T . 249201) (NIL . 750799)))
;(:SWITCH ((T . 375739) (NIL . 624261)))

(run 5 :num-rounds 1000000)
;(:KEEP ((T . 199814) (NIL . 800186)))
;(:SWITCH ((T . 266971) (NIL . 733029)))

(run 20 :num-rounds 1000000)
;(:KEEP ((T . 49776) (NIL . 950224)))
;(:SWITCH ((T . 52837) (NIL . 947163)))

(run 200 :num-rounds 1000000)
;(:KEEP ((T . 5083) (NIL . 994917)))
;(:SWITCH ((T . 5100) (NIL . 994900)))
```

## License

See [LICENSE](./LICENSE)
