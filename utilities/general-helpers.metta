;; Helper functions that are going to be useful for writing any metta code

(: ~= (-> Atom Atom Bool))
(= (~= $a $b) (not (== $a $b)))

;; INFO: Uncomment for the use of MeTTaLog.
;; (: >= (-> Number Number Bool))
;; (= (>= $a $b) (or (== $a $b) (> $a $b)))

;Function to cocatinate two tuples (A B) (C D) ==> (A B C D) ;; can also be used with atoms
(: ++ (-> Expression Expression Expression))
(= (++ $x $y)
   (case ((isSymbol $x) (isSymbol $y))
      (
        ((True True)  ($x $y))
        ((True False) (cons-atom $x $y))
        ((False True) (appendAtom $y $x))
        ((False False)  (foldr cons-atom $y $x)))))

; a helper function to the isConsistentExp function.
; a function which simplifies nested logical negations by reducing them to their simplest form. 
(= (Not $a)
    (if (== (get-metatype $a) Symbol)
        (NOT $a)
        (if (== (get-metatype $a) Expression)
            (case $a
                (
                    ( (NOT $b) (if (== (get-metatype $b) Symbol) $b (Not $b)))
                ))
    False )))

; a helper function to the isConsistentExp function.
; a function which checks if an element is member of a tuple.
(: isMember (-> $a $aa Bool))
(= (isMember $x $list)
    (not (== (collapse (intersection (superpose ($x)) (superpose $list))) ()))
)

; This function assumes it receives a set1 and set2 expression as parameters.
; It then removes similar elements found in set2 that already exist in set1.
; The function uses isMember from general helpers functions.

(: setDifference (-> Expression Expression Expression))
(= (setDifference $set1 $set2) 
    (if (== $set1 ()) () ; Base case: return empty set when $set1 is empty
        (let* (
            ($head (car-atom $set1))  ; Extract the head of set1
            ($tail (cdr-atom $set1))  ; Extract the tail of set1
            ($newtail (setDifference $tail $set2))  ; Recursively process the tail
        )
            ; Check if the $head is in set2, if so, skip it

            (if (isMember $head $set2)
                $newtail 
                ; Otherwise, add the head to the new tail, avoiding unnecessary parentheses
                (if (== (get-metatype $newtail) Expression) (cons-atom $head $newtail) $head)  ; Avoid wrapping the head if it's already properly structured
            ))))

;; A helper function for the findAndReplace function.
;; It handles the replacement of a single atom.
(= (replace $old $new $current) (if (== $old $current) $new $current))

;; A function to replace a specific atom
;;    with a new one from a list of atoms.
;; INFO: This function needs to change so that it can preserve the
;;        operator's position when MeTTa version is upgraded.
(= (findAndReplace $old $new $list) 
    ;; (let*
    ;;   (
    ;;     (() (println! ""))
    ;;     (() (println! (==> === Inside findAndReplace === <==)))
    ;;     (() (println! (Parameters ==> Old: $old New: $new List: $list)))
    ;;     (() (println! (Result ==> (collapse (replace $old $new (superpose $list))))))
    ;;     (() (println! ""))
    ;;   )
    ;   (collapse (replace $old $new (superpose $list)))
      (reverseExp (collapse (replace $old $new (superpose $list))) ())
    ;; )
)

;; A function that behaves like a do while loop.
;; It executes the functions in the given list and returns the last result.
(: until (-> (-> $a Bool) (-> $a $a) $a $a))
(= (until $predicate $function $x) (if ($predicate $x) $x (let $x' ($function $x) (until $predicate $function $x'))))

;; A foldleft function like the implementation in Haskell.
;; It takes a binary function that expects two atoms and
;;    apply it recursively on a nested atom.
(: foldl (-> (-> $a $b $d) $b $c $d))
(= (foldl $f $acc $tuple)
   (if (== $tuple ())
       $acc
       (let*
            (
                ($next (car-atom $tuple))
                ($tail (cdr-atom $tuple))
            )
            (foldl $f ($f $next $acc) $tail)
       )
    ;    (let $next (car-atom $tuple) (foldl $f ($f $next $acc) (cdr-atom $tuple)))
   )
)

;; Fold a tuple from right to left
(: foldr (-> (-> $a $b $b) $d $c $d)) ;; changed the parameterized type of second argument from $b to $d
(= (foldr $f $i $xs)
   (if (== $xs ())
       $i
       (let* (($h  (car-atom $xs)) ; added isSymbol checks not to encounter any errors
              ($t (cdr-atom $xs)) ; added isSymbol checks not to encounter any errors
              ($ft (foldr $f $i $t)))
         ($f $h $ft))))

;; Apply a given function to every element of a tuple
(: map (-> (-> $a $b) $c $d))
(= (map $f $xs)
   (if (== $xs ())
       ()
       (let* (($h (car-atom $xs))
              ($t (cdr-atom $xs))
              ($fh ($f $h))
              ($ft (map $f $t)))
         (cons-atom $fh $ft))))

;; Function: wrapper
;; Description: This function applies a predicate to an atom and returns the atom if the predicate is true, otherwise returns an empty expression.
;; Parameters:
;;   - $predicate: A function that takes an atom and returns a boolean value.
;;   - $x: The atom to which the predicate is applied.
;; Returns:
;;   - Expression: Returns the atom if the predicate evaluates to true, otherwise returns an empty expression.
(:wrapper (-> (-> $a Bool) $aa $aa))
(= (wrapper $predicate $x) (if (let $bool ($predicate $x) $bool) $x (empty)))

;; Filter function given a predicate 
(: filter (-> (-> $a Bool) $b $b))
(= (filter $predicate $list)
   (wrapper $predicate (superpose $list)))

;; INFO: Uncommnet the following two functions when using MeTTaLog.
;; (:wrapperB (-> (-> $a $b Bool) $a $b $b))
(= (wrapperB $predicate $x $y) (if ($predicate $x $y) $y (empty)))

(: filterB (-> (-> $a $b Bool) $a $bb $bb))
(= (filterB $predicate $x $list)
   (wrapperB $predicate $x (superpose $list)))

;; A function to remove all elements found in $common from a $tuple
(= (removeElement $common $tuple)
   (reverseExp (collapse (subtraction (superpose $tuple) (superpose $common))) ()))

; ;; A function that returns True if any of the input atoms are True and False otherwise
; ;; Example: input ==> (True True False), output ==> True
; ;;          input ==> (False False False), output ==> False
; ;;          input ==> (False True False), output ==> True
(: any (-> Atom Bool))
(= (any $bools) (isMember True $bools))

;; A trick to define `curry` in MeTTa without `lambda`
(: curry (-> (-> $a $b $c) (-> $a (-> $b $c))))
(= (((curry $f) $x) $y) ($f $x $y))

; ;; Add numbers in an atom list
(= (sum $list) (foldr + 0 $list))

;; Function to count atom occrence in a list of atoms.
(= (count $atom $list)
   (if (== $list ())
        0
        (sum (collapse (if (== $atom (superpose $list)) 1 (empty))))))

;; A function to check if a given atom is symbol.
(: isSymbol (-> Expression Bool))
(= (isSymbol $expr) (== (get-metatype $expr) Symbol))

;; A function to check if a given atom is expression.
(: isExpression (-> Atom Bool))
(= (isExpression $expr) (== (get-metatype $expr) Expression))

;; A function to check if a given atom is unit or empty tuple.
(: isUnit (-> $a Bool))
(= (isUnit $expr) (== $expr ()))

;; A function to check if a given atom is an expression 
;;   with has one atom in it.
(: hasOneAtom (-> $a Bool))
(= (hasOneAtom $expr) (if (== (get-metatype $expr) Expression) (== (size-atom $expr) 1) False))

;; Function to replace a given atom from a space 
(: update-atom (-> Grounded Atom Atom (->)))
(= (update-atom $space $oatom $natom) (let $_ (remove-atom $space $oatom) (add-atom $space $natom)))

;; Function to replace a given atom from a space. 
(: appendAtom (-> Atom Atom Atom))
(= (appendAtom $a $atom) (foldr cons-atom ($a) $atom))

;; Return the maximum of two numbers.
(= (max $x $y) (if (< $x $y) $y $x))

;; A function to return a tuple of numbers from natural to a given number.
;; $x is the upperbound
;; $acc is the lowerbound wrapped with as a tuple.
;;    Example: (gen 5 (0)) => (5 4 3 2 1 0) 
(= (gen $x $acc)
    (let $top (car-atom $acc)
    (if (== $top $x)
       $acc
       (let $lastInserted (car-atom $acc) (gen $x (cons-atom (+ 1 $lastInserted) $acc))))))

; ;; A function to return head and tail of an atom in a tuple.
(= (decons $atom)
   (if (not (or (isSymbol $atom) (isUnit $atom)))
       ((car-atom $atom) (cdr-atom $atom))
       (Error Unit Or Symbol cannot be deconstructed)))

;; Function to create a tuple of tuples given two equal length list of atoms.
(= (zip $a $b)
   (if (or (== $a ()) (== $b ()))
       ()
       (let*
          (
            (($x $xs) (decons $a))
            (($y $ys) (decons $b))
            ($rest (zip $xs $ys))
          )
          (cons-atom ($x $y) $rest)))
)

;; Similar to zip but instead of just creating a tuple of atoms,
;;  it performes an operation of $f on them and returns the resulting list of atoms.
(: zipWith (-> (-> $a $b $c) $d $e $f))
(= (zipWith $f $xs $ys)
   (if (== $xs ())                      ; We assume (arity $xs) == (arity $ys)
       ()
       (let* ((($xs-hd $xs-tl) (decons $xs))
              (($ys-hd $ys-tl) (decons $ys))
              ($head ($f $xs-hd $ys-hd))
              ($tail (zipWith $f $xs-tl $ys-tl)))
         (cons-atom $head $tail))))

;; Finds length of a tuple 
(= (len $expr) (size-atom $expr))
    

;; A function to replace an atom at a specific index position with $val -- 0 indexed counting is implied
(: replaceByIndex (-> Expression Number $t Expression))
(= (replaceByIndex $expr $index $new)
    (if (== $expr ())
        $expr
        (let*
            (
                ($first (car-atom $expr))
                ($tail (cdr-atom $expr))
            )
            (if (== $index 0)
                (if (== $tail ())
                    ($new)
                    (cons-atom $new $tail)
                )
                (let $c (replaceByIndex $tail (- $index 1) $new)
                    (cons-atom $first $c)
                )))))

;; Select atom by index and returns
(= (selectByIndex $expr $index)
    (if (== $expr ())
        (Error (Index out of range) ())
        (if (== $index 0)
            (car-atom $expr)
            (let $tail (cdr-atom $expr)
                (selectByIndex $tail (- $index 1))
            ))))

;; TODO: Use zipWith instead
;; A function that compares tuples element by element.
(= (compareElements $first $second)
   (zipWith == $first $second)
)

;; Function to empty a space with contents.
(= (clearSpace $space)
   (collapse (let $content (get-atoms $space) (remove-atom $space $content))))
   
;; Function that merges two sorted lists while keeping them sorted.
(= (merge $xs $ys) (merge $xs $ys <=))
(= (merge $xs $ys $key)
    (case ($xs $ys)
          (
            ((() $ys) $ys)
            (($xs ()) $xs)
            (($xs $ys)
              (let*
                  (
                    (($x $xss) (decons $xs))
                    (($y $yss) (decons $ys))
                  )
                  (if ($key $x $y)
                      (let $t (merge $xss $ys $key) (cons-atom $x $t))
                      (let $t (merge $xs $yss $key) (cons-atom $y $t))))))))

;; A function to split a list in to two at an index so that everything
;;    until that index without including that index as first tuple and 
;;    the rest as the second.
(: splitAt (-> Number Expression (Atom Atom)))
(= (splitAt $n $list)
   (case ($n $list)
     (
       ((0 $list) (() $list))
       (($_ ()) (() ()))
       (($n $list)
          (let*
             (
               ;; (() (println! (Splitting $list at: $n)))
               (($x $xs) (decons $list))
               (($left $right) (splitAt (- $n 1) $xs))
               ;; (() (println! (From splitAt Left: $left Right: $right)))
             )
             ((cons-atom $x $left) $right))))))

;; Merge sort function. Takes length to gain 
;;  performance by avoiding recomputation of index.
(= (sortNestedTuple $list reverse) (sort $list (len $list) >=))

(= (sort $list $len) (sort $list $len <=)) 
(= (sort $list $len $key)
   (if (or (isUnit $list) (hasOneAtom $list))
       $list
       (let* (
                ;; (() (println! (Sorting $list len: $len)))
                ($halfL ((py-atom math.floor) (/ $len 2)))
                (($left $right) (splitAt $halfL $list))
                ;; (() (println! (From sort Left: $left Right: $right)))
             )
             (merge (sort $left $halfL $key) (sort $right (- $len $halfL) $key) $key))))

;; A function to take an atom and repeat it n times.
(: repeat (-> Atom Number Atom))
(= (repeat $a $n)
   (if (== $n 0)
       ()
       (let $t (repeat $a (- $n 1)) (cons-atom $a $t))
   )
)

(: take (-> Number Atom Atom))
(= (take $n $a)
    (case ($n $a)
          (
            (($n ()) ())
            ((0 $a) ())
            (($n $a)
              (let*
                (
                  (($h $t) (decons $a))
                  ($r (take (- $n 1) $t)))
                (cons-atom $h $r))))))

;; A function to remove the frist n elements of a tuple 
;;   and returns the rest.
(: drop (-> Number Atom Atom))
(= (drop $n $a)
    (case ($n $a)
      (
        (($n ()) ())
        ((0 $a) $a)
        (($n $a)
          (let*
            (
              (($h $t) (decons $a))
              ($r (drop (- $n 1) $t)))
            $r)))))

;; non-deterministic definitio of length

(= (length $expr)
    (sum (collapse (let $a (superpose $expr) 1)))
)

;; isLiteral -- determines if the given atom is a literal or not

(= (isLiteral $a)
    (if (or (== (get-metatype $a) Grounded) (== (get-metatype $a) Symbol))
        True
        (if (== (car-atom $a) NOT)
            True
            False )))

;; minOfTuple -- minimum of tuple of numeric expressions

(= (minOfTuple $expr)
    (let ($f $t) (decons $expr)
        (if (== $t ())
            $f
            (min $f (minOfTuple $t))
        )))

;; unNest -- extracts elements from nested expressions 

(= (unNest $exp)
    (let $el (superpose $exp)
        (if (isLiteral $el)
            $el
            (unNest $el))))
;; flatten -- returns one tuple from a nested tuple of tuple of tuples ...

(= (flatten $exp) (collapse (unNest $exp)))


(= (min $x $y) (if (<= $x $y) $x $y))

; ;; maxOfTuple -- finds maximum of tuple of numbers

; ; (= (maxOfTuple $expr)
; ;     (let*
; ;         (
; ;             ($f (car-atom $expr))
; ;             ($t (cdr-atom $expr)))

; ;         (if (== $t ())
; ;             $f
; ;             (max $f (maxOfTuple $t)))))


;; Add numbers in an atom list
; (= (sum $list) (foldr + 0 $list))
; (= (sum $list)
;     (if (== $list ())
;         0
;         (let*
;             (
;                 ($f (car-atom $list))
;                 ($t (cdr-atom $list))
;             )
;             (if (== $t ())
;                 $f
;                 (+ $f (sum $t)) ))))

; (= (getMaxWithKey $tuple)
;     (let $max 
;         (collapse 
;             (let*
;                 (
;                     ($t (superpose $tuple))
;                     ($maxKey (let $keys (collapse (let $el (superpose $tuple) (car-atom $el))) (maxOfTuple $keys)))
;                     ; (() (println! (maxxx $maxKey)))
;                 ) 
;                 (unify $t ($maxKey $genId $iId $i) ($maxKey $genId $iId $i) (empty))))
;         (car-atom $max)))

;; map` -- a modified map functinality with, i.e., a map that takes a function which has two inputs

(: map` (-> (-> $a $a $b) $a $c $d))
(= (map` $f $x $xs)
   (if (== $xs ())
       ()
       (let* (($h (car-atom $xs))
              ($t (cdr-atom $xs))
              ($fh ($f $h $x))
              ($ft (map` $f $x $t)))
         (cons-atom $fh $ft))))

;; notNt -- check if a symbol is not a member of the non-terminal set

(= (notMember $symbol $expr) (not (isMember $symbol $expr)))

;; isValidExp -- to check if the boolean expression produced by genPhen is a valid bolean expression
;;          $exp -- an expression which is being checked for validity
;;          $parent -- an expression against which the check is being made -- set of non-terminals in this case.

(= (isValidExp $exp $parent)
    (if (== (get-metatype $exp) Symbol)
        (notMember $exp $parent)
        (let*
            (
                ($f (flatten $exp))
                ($t (map` notMember $parent $f))
            ) 
            (all $t)
        )))
;; This function adds list to space
(= (addListToSpace $space $list)
    (add-reduct $space (superpose $list))
)

;; a function that checks that all of the expressions are true.
(: all (-> Expression Bool))
(= (all $expressions) (if (isMember False $expressions) False True))


;; Name:                        ===
;; Description:                 A helper function to evaluate equality of two expressions ignoring ordering of litral children.
;;                              The grounded equality operator, ==, will return `False` when comparing (AND A B) and (AND B A)
;;                              === returns `True`.
;;                              Used when looking for the POA in the parent's list of children.
(= (=== $x $y)
   (if (and (== (get-metatype $x) Symbol) (== (get-metatype $y) Symbol))
       (== $x $y)
       (== (collapse (union (subtraction (superpose $x) (superpose $y)) (subtraction (superpose $y) (superpose $x)))) ())))

;; Name :                       reverseExp
;; Description:                 A function to reverse a given expression. This is needed b/c
;;                              new version of metta (0.2.3) is giving unpredicted order when
;;                              using superpose.
;;                              Eg: given ((AND E C) B A OR) => (OR A B (AND E C))
;;                              NOTE: Must use collapse at the begining of your function call
;; Usage: (collapse (reversExp ((AND E C) B A OR)))
(= (reverseExp $exp $i)
  (if (== $exp ())
    $i
    (let*
      (
        (($h $t) (decons-atom $exp))
        ; ($s (if (isSymbol $h) (cons-atom $h $i) (++ ($h) $i)))
        ($s (cons-atom $h $i))
      )
      (reverseExp $t $s)
    )
  )
)