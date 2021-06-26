(defpackage #:monty-hall
   (:use #:cl #:arrow-macros))

(in-package #:monty-hall)

(defun list-of-integers-p (list)
  "Return t if LIST is non nil and contains only strings."
  (and (consp list)
       (every #'integerp list)))

(deftype list-of-integers ()
  `(satisfies list-of-integers-p))

(deftype list-of ()
  `(satisfies consp))

(declaim (ftype (function (integer list-of-integers) integer) random-with-exclusions))
(defun random-with-exclusions (n exclusions)
  "Returns a random number within the range that isn't in the list"
  (let ((random-number (-> n random (+ 1))))
    (if (member random-number exclusions)
        (random-with-exclusions n exclusions)
        random-number)))

(declaim (ftype (function (integer boolean) t) play))
(defun play (num-doors swap-choice)
  "Play the Monty Hall game"
  (let* (; 1. Initialize with car randomly placed somewhere
         (car-door (-> num-doors random (+ 1)))
         ; 2. Guest picks a door (simulate randomly missing the car)
         (guest-door-choice (-> num-doors random (+ 1)))
         ; 3. Host picks a door (chooses a door with a goat)
         (host-goat-door-choice (random-with-exclusions num-doors `(,car-door ,guest-door-choice)))
         ; Host points to another door that the guest can swap
         (host-swap-door-choice (random-with-exclusions num-doors `(,guest-door-choice ,host-goat-door-choice))))
    (if swap-choice (= car-door host-swap-door-choice)
        (= car-door guest-door-choice))))

(declaim (ftype (function (hash-table t) hash-table) sort-into-buckets))
(defun sort-into-buckets (acc x)
  "Sorts a list of values into buckets with the tally"
  (let ((count (gethash x acc)))
    (setf (gethash x acc) (+ (or count 0) 1))
    acc))

(declaim (ftype (function (list-of) hash-table) score))
(defun score (list)
  "Get the total scores from a given list of rounds"
  (reduce #'sort-into-buckets list :initial-value (make-hash-table)))

(declaim (ftype (function (function list-of) list-of) sort-alist))
(defun sort-alist (f alist)
  "Sorts an alist by key given a function f"
  (stable-sort alist f :key #'car))

; Simulate N rounds where guest won't swap vs will swap
(declaim (ftype (function (&key (:num-doors integer) (:switch-choice boolean) (:num-rounds integer)) list-of) test))
(defun test (&key num-doors switch-choice (num-rounds 10000))
  (let ((rounds (make-list num-rounds)))
    (->>
      rounds
      (map 'list #'(lambda (_) (play num-doors switch-choice)))
      score
      alexandria:hash-table-alist
      (sort-alist #'(lambda (a b) (when a 1 0))))))

(declaim (ftype (function (integer &key (:num-rounds integer)) t) run))
(defun run (num-doors &key (num-rounds 10000))
  (print
   `(:keep ,(test :num-doors num-doors :switch-choice nil :num-rounds num-rounds)))
  (print
   `(:switch ,(test :num-doors num-doors :switch-choice t :num-rounds num-rounds)))
  (terpri))

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
