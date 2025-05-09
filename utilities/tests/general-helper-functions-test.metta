!(register-module! ../../../metta-moses)

! (import! &self metta-moses:utilities:general-helpers)
! (bind! &newSpace (new-space))

!(assertEqualToResult (~= 1 2) (True))
!(assertEqualToResult (~= 1 1) (False))

!(assertEqualToResult (~= Something Nothing) (True))
!(assertEqualToResult (~= Something Something) (False))
!(assertEqualToResult (~= Something something) (True))

;; ;; Tests for the function 'concatTuple' ;; FIXED failing tests ;; TODO change 'concatTuple' to '++'
!(assertEqualToResult (++ () (3 4)) ((3 4)))
!(assertEqualToResult (++ (1 2) (3 4)) ((1 2 3 4)))
!(assertEqualToResult (++ (1 2) ()) ((1 2)))
!(assertEqualToResult (++ () ()) (()))

;; Tests for the function 'Not'
!(assertEqualToResult (Not (Not (Not A))) ((NOT A)))
!(assertEqualToResult (Not (Not A)) (A))

;; Tests for the function 'isMember'
!(assertEqualToResult (isMember 1 ()) (False))
!(assertEqualToResult (isMember 2 (2 3 4 5)) (True))
!(assertEqualToResult (isMember 3 (1 2 4 5)) (False))


! (assertEqual (=== (setDifference (AND a b (NOT a) c (OR a b)) (a b (NOT a) (OR a b)) ) (AND c)) True)
! (assertEqual (=== (setDifference (AND a b (NOT a) c (OR a b)) (a c (NOT c) (NOT a)) ) (AND b (OR a b))) True)
! (assertEqual (=== (setDifference (AND a b (NOT a) c (OR a b)) (a) )  (AND b (NOT a) c (OR a b))) True)
! (assertEqual (=== (setDifference (AND a b (NOT a) c (OR a b v) v) (b (NOT a) v) ) (AND a c (OR a b v))) True)

;; ;; Tests for findAndReplace function ;; FIXED failing Test case
;;
!(assertEqual (=== (findAndReplace A Z (B C A (A B C) F G A)) (B C Z (A B C) F G Z)) True)
!(assertEqual (=== (findAndReplace (A B C) Z (B C A (A B C) F G A)) (B C A Z F G A)) True)
!(assertEqual (=== (findAndReplace G 1 (B C A (A B C) F G A)) (B C A (A B C) F 1 A)) True)
!(assertEqual (=== (findAndReplace Z 1 (B C A (A B C) F G A)) (B C A (A B C) F G A)) True)
!(assertEqual (=== (findAndReplace A Z (A C A (A B C) A G A)) (Z C Z (A B C) Z G Z)) True)
!(assertEqual (=== (findAndReplace F (A B C) (B C A (A B C) F G A)) (B C A (A B C) (A B C) G A)) True)

;; Tests for until function

;; These are functions used for testing helper functions.

;; It returns True only if the input is zero.
(: isZero (-> Number Bool))
(= (isZero $x) (== $x 0))

;; It will return True if greater than 10.
(: greaterThanTen (-> Number Bool))
(= (greaterThanTen $x) (> $x 10))

;; It will decrement the input by 1.
(: decrement (-> Number Number))
(= (decrement $x) (- $x 1))

;; It will increment the input by 1.
(: increment (-> Number Number))
(= (increment $x) (+ $x 1))

;; A function that turns every atom to unit
(: transformToUnit (-> Atom Atom))
(= (transformToUnit $x) ())

;; Test for dummy function of until
!(assertEqual (isZero 1) False)
!(assertEqual (isZero 0) True)

!(assertEqual (greaterThanTen 8) False)
!(assertEqual (greaterThanTen 0) False)
!(assertEqual (greaterThanTen 11) True)

!(assertEqual (isUnit (foo bar bae)) False)
!(assertEqual (isUnit ()) True)

!(assertEqual (decrement 1) 0)
!(assertEqual (decrement -1) -2)
!(assertEqual (decrement 100000) 99999)

!(assertEqual (increment 1) 2)
!(assertEqual (increment -1) 0)
!(assertEqual (increment 100000) 100001)

;; Test for actual until function
!(assertEqual (until isZero decrement 10) 0)
!(assertEqual (until greaterThanTen increment 0) 11)

;; FIXED: Failing Test case
!(assertEqual (until isUnit cdr-atom (foo bar bae)) ())
!(assertEqual (until hasOneAtom cdr-atom (foo bar bae)) (bae))

;; Test for map function

!(assertEqual (map increment (1 2 3 4)) (2 3 4 5))
!(assertEqual (map decrement (1 2 3 4)) (0 1 2 3))
!(assertEqual (map transformToUnit (A B C (A C D) E)) (() () () () ()))
!(assertEqual (map isUnit (A B C (A C D) E)) (False False False False False))
!(assertEqual (map isUnit (map transformToUnit (A B C (A C D) E))) (True True True True True))
!(assertEqual (map isZero (map decrement (1 2 3 4))) (True False False False))
!(assertEqual (map isZero (1 2 3 4)) (False False False False))
!(assertEqual (map isZero (0 1 2 3 4)) (True False False False False))
!(assertEqual (map hasOneAtom (A B C (A C D) E)) (False False False False False))
!(assertEqual (map greaterThanTen (1 10 2 23 32 7 8 43 0 2)) (False False False True True False False True False False))
!(assertEqual (map greaterThanTen (1 10 2 23 32 7 8 43 0)) (False False False True True False False True False))

;; Test count atom function
!(assertEqual (count expr (bop expr expr bop bop expr x y a expr u expr z a)) 5)
!(assertEqual (count bop (bop expr expr bop bop expr x y a expr u expr z a)) 3)
!(assertEqual (count x (bop expr expr bop bop expr x y a expr u expr z a)) 1)
!(assertEqual (count u (bop expr expr bop bop expr x y a expr u expr z a)) 1)
!(assertEqual (count f (bop expr expr bop bop expr x y a expr u expr z a)) 0)
!(assertEqual (count z (bop expr expr bop bop expr x y a expr u expr z a)) 1)
!(assertEqual (count expr ()) 0)
!(assertEqual (count expr (expr)) 1)
!(assertEqual (count expr (bop)) 0)

;; Test sum function
!(assertEqual (sum (1 2 3 4 5 6 7 8 9 10)) 55)
!(assertEqual (sum (1 1 1 1)) 4)
!(assertEqual (sum (1)) 1)

;; Test curry function
!(assertEqual (((curry +) 1) 2) 3)
!(assertEqual (((curry -) 5) 4) 1)

;; ;; Test filter function ;; FIXED: Failing Test case
!(assertEqual (collapse (filter isUnit (9 10 () 5 4 ()))) (() ()))
!(assertEqual (collapse (filter hasOneAtom (9 10 (Hello) 5 4 (Nop) ()))) ((Hello) (Nop)))
!(assertEqual (collapse (filter isZero (9 10 0 5 4 0))) (0 0))
!(assertEqual (collapse (filter greaterThanTen (9 10 12 5 4 28))) (12 28))

;; Test wrapper function for filter function
!(assertEqual (wrapper isZero 0) 0)
!(assertEqual (wrapper isZero 1) (empty))
!(assertEqual (wrapper isUnit 1) (empty))
!(assertEqual (wrapper isUnit ()) ())


;; Test for add list to space
!(addListToSpace &newSpace (1 2 3 4 5))
!(assertEqual (collapse (get-atoms &newSpace)) (1 2 3 4 5))
!(addListToSpace &newSpace (6 7 8))
!(assertEqual (collapse (get-atoms &newSpace)) (1 2 3 4 5 6 7 8))
!(addListToSpace &newSpace (9 10))
!(assertEqual (collapse (get-atoms &newSpace)) (1 2 3 4 5 6 7 8 9 10))

;; Test for checking the all function.
!(assertEqual (all (True True True True)) True)
!(assertEqual (all (True True False True)) False)
!(assertEqual (all (True True True False)) False)


;; Test foldr function
!(assertEqual (foldr + 0 (1 2 3)) 6)
!(assertEqual (foldr + 0 (1 2 3)) 6)

;; Test isSymbol function
!(assertEqual (isSymbol A) True)
!(assertEqual (isSymbol (A)) False)
!(assertEqual (isSymbol ()) False)
!(assertEqual (isSymbol (A B)) False)

;; Test isExpression function
!(assertEqual (isExpression A) False)
!(assertEqual (isExpression (A)) True)
!(assertEqual (isExpression ()) True)
!(assertEqual (isExpression (A B)) True)

;; Test isUnit function
!(assertEqual (isUnit A) False)
!(assertEqual (isUnit (A)) False)
!(assertEqual (isUnit ()) True)
!(assertEqual (isUnit (A B)) False)

;; ;; Test hasOneAtom function ;; FIXED: Failing Test case
!(assertEqual (hasOneAtom A) False)
!(assertEqual (hasOneAtom (A)) True)
!(assertEqual (hasOneAtom ()) False)
!(assertEqual (hasOneAtom (A B)) False)
!(assertEqual (hasOneAtom (foo bar bae)) False)
!(assertEqual (hasOneAtom (foo)) True)
!(assertEqual (hasOneAtom ()) False)


!(bind! &space (new-space))
!(add-atom &space A)
!(add-atom &space C)
!(add-atom &space C)

;; FIXED: Failing Test case
!(assertEqualToResult (let $_ (update-atom &space A B) (get-atoms &space)) (B C C))
!(assertEqualToResult (let $_ (update-atom &space D E) (get-atoms &space)) (B C C E))
!(assertEqualToResult (let $_ (update-atom &space C 1) (get-atoms &space)) (B 1 C E))
!(assertEqualToResult (let $_ (clearSpace &space) (get-atoms &space)) ())

;; ;; FIXED: Failing Test case
!(assertEqual (appendAtom 1 ()) (1))
!(assertEqual (appendAtom 1 (1 2)) (1 2 1))
!(assertEqual (appendAtom A (B C)) (B C A))
!(assertEqual (appendAtom (A) (B C)) (B C (A)))

!(assertEqual (max 1 2) 2)
!(assertEqual (max 2 4) 4)

;; ;; FIXED: Failing Test case
!(assertEqual (gen 1 (0)) (1 0))
!(assertEqual (gen 3 (0)) (3 2 1 0))

!(assertEqual (decons (A B C)) (A (B C)))
!(assertEqual (decons ()) (if False ((car-atom ()) (cdr-atom ())) (Error Unit Or Symbol cannot be deconstructed)))
!(assertEqual (decons (A)) (A ()))

;; ;; FIXED: Failing Test case
!(assertEqual (zip (A B C) (1 2 3)) ((A 1) (B 2) (C 3)))
!(assertEqual (zip (A B) (1 2 3)) ((A 1) (B 2)))

!(assertEqual (zipWith == (A B C) (A 2 3)) (True False False))
!(assertEqual (zipWith == (B C) (A 2 3)) (False False))
!(assertEqual (zipWith == () (A 2 3)) ())
!(assertEqual (zipWith == () ()) ())

;; ;; FIXED: Failing Test case
!(assertEqual (len (A B C D)) 4)
!(assertEqual (len ()) 0)
!(assertEqual (len (A)) 1)
!(assertEqual (len A) (size-atom A))

!(assertEqual (replaceByIndex (A B C) 0 D) (D B C))
!(assertEqual (replaceByIndex (1 2 3 4) 4 D) (1 2 3 4))
!(assertEqual (replaceByIndex (1 2 3 4) 3 D) (1 2 3 D))

!(assertEqual (selectByIndex (1 2 3 4) 2) 3)
!(assertEqual (selectByIndex (1 2 3 4) 3) 4)
!(assertEqual (selectByIndex (1 2 3 4) 0) 1)

!(assertEqual (compareElements (A B C) (1 2 3)) (False False False))
!(assertEqual (compareElements (A B C) (A B C)) (True True True))
!(assertEqual (compareElements (1 2 3) (1 2 3)) (True True True))

!(assertEqual (splitAt 2 (A B C D E)) ((A B) (C D E)))
!(assertEqual (splitAt 0 (A B C D E)) (() (A B C D E)))
!(assertEqual (splitAt 5 (A B C D E)) ((A B C D E) ()))

;; ;; FIXED: Failing Test case.
!(assertEqual (sort (2 3 1 4) 4) (1 2 3 4))
!(assertEqual (sort (2 3 1 4) 4 >=) (4 3 2 1))

!(assertEqual (take 1 (A B C)) (A))
!(assertEqual (take 2 (A B C)) (A B))
!(assertEqual (take 2 ()) ())

!(assertEqual (drop 1 (A B C)) (B C))
!(assertEqual (drop 2 (A B C)) (C))
!(assertEqual (drop 2 ()) ())

;; Test selectByIndex

!(assertEqual (selectByIndex (0 1 2 3 4) 2) 2)
!(assertEqual (selectByIndex (0 1 2 3 4) 3) 3)
!(assertEqual (selectByIndex (0 1 2 3 4) 5) (Error (Index out of range) ()))

;; Test unNest

!(assertEqual (collapse (unNest (AND A (AND B C)))) (AND A AND B C))
!(assertEqual (collapse (unNest (1 2 (3 4 (5))))) (1 2 3 4 5))

;; Test cleanSpace

!(bind! &__ (new-space))

!(add-reduct &__ (superpose ((A B C) atom 2 4 (2 1))))

!(assertEqual 
        (let () (cleanSpace &__) 
            (collapse (get-atoms &__))) (empty))

;; Test repeat

!(assertEqual (repeat A 3) (A A A))
!(assertEqual (repeat (== 1 2) 0) ())
!(assertEqual (repeat (== 1 2) 2) (False False))
!(assertEqual (repeat 5 4) (5 5 5 5))
!(assertEqual (repeat 1 3) (1 1 1))

;; Test update-atom
!(add-atom &__ (A 2 3))
!(let () (collapse (update-atom  &__ (A 2 3) (A 3 4))) (assertEqual (match &__ $x $x) (A 3 4))) ;; FIX: Possibly failing test

;; Test flatten

!(assertEqual (flatten (AND A (AND A B))) (AND A AND A B))
!(assertEqual (flatten (AND A (AND (OR A (NOT C)) (OR B D)))) (AND A AND OR A (NOT C) OR B D))

;; Test notNt 
!(bind! NTS (start expr bop uop term))

!(assertEqual (notMember AND NTS) True)
!(assertEqual (notMember A NTS) True)
!(assertEqual (notMember expr NTS) False)

;; Test map`

(= (add $x $y) (+ $x $y))

!(assertEqual (map` add 2 (1 2 3 4 5)) (3 4 5 6 7))
!(assertEqual (map` notMember (1 2 3) (2 4 1 3)) (False True False False))

;; Test isValidExp

!(assertEqual (isValidExp A (start expr bop uop term)) True)
!(assertEqual (isValidExp expr (start expr bop uop term)) False)
!(assertEqual (isValidExp (AND A B) (start expr bop uop term)) True)
!(assertEqual (isValidExp (OR (AND B null) (NOT B)) (start expr bop uop term)) True)
