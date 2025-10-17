!(register-module! ../metta-moses)

! (import! &self metta-moses:reduct/boolean-reduct:rte-helpers)
! (import! &self metta-moses:reduct/boolean-reduct:cut-unnecessary-or)
! (import! &self metta-moses:reduct/boolean-reduct:cut-unnecessary-and)
! (import! &self metta-moses:reduct/boolean-reduct:n-ary-propagate-not)
! (import! &self metta-moses:reduct/boolean-reduct:n-ary-gather-junctors)
! (import! &self metta-moses:reduct/boolean-reduct:delete-inconsistent-handle)
! (import! &self metta-moses:reduct/boolean-reduct:zero-constraint-subsumption)
! (import! &self metta-moses:reduct/boolean-reduct:one-constraint-subsumption)
! (import! &self metta-moses:reduct/boolean-reduct:promote-common-constraints)
! (import! &self metta-moses:reduct/boolean-reduct:reduce-to-elegance)

! (import! &self metta-moses:deme:merge-demes)
! (import! &self metta-moses:deme:create-deme)
! (import! &self metta-moses:deme:deme-id-creation)
! (import! &self metta-moses:deme:score-deme)
!(import! &self metta-moses:deme:expand-deme)


! (import! &self metta-moses:scoring:fitness)
! (import! &self metta-moses:scoring:bscore) 
! (import! &self metta-moses:scoring:cscore) 
! (import! &self metta-moses:scoring:complexity-based-scorer)

! (import! &self metta-moses:moses:neighborhood-sampling)

! (import! &self metta-moses:representation:representation)
! (import! &self metta-moses:representation:knob-representation)
! (import! &self metta-moses:representation:instance) 
! (import! &self metta-moses:representation:lsk)
! (import! &self metta-moses:representation:knob-mapper)
! (import! &self metta-moses:representation:logical-probe) 
! (import! &self metta-moses:representation:build-logical)
! (import! &self metta-moses:representation:build-knobs)
! (import! &self metta-moses:representation:sample-logical-perms) 
! (import! &self metta-moses:representation:add-logical-knobs)
!(import! &self metta-moses:representation:create-representation)

! (import! &self metta-moses:utilities:general-helpers)
! (import! &self metta-moses:utilities:list-methods)
! (import! &self metta-moses:utilities:ordered-set)
! (import! &self metta-moses:utilities:pair)
! (import! &self metta-moses:utilities:tree)
! (import! &self metta-moses:utilities:map)
! (import! &self metta-moses:utilities:nodeId)
! (import! &self metta-moses:utilities:ordered-multimap)
! (import! &self metta-moses:utilities:python-treehelpers)
! (import! &self metta-moses:utilities:lazy-random-selector)

!(import! &self metta-moses:optimization/hillclimbing:cross-top-one)
!(import! &self metta-moses:optimization/hillclimbing:hill-climbing-helpers)
!(import! &self metta-moses:feature-selection:feature-selection-helpers)
!(import! &self metta-moses:optimization/hillclimbing:cross-top-one-helpers)

! (import! &self metta-moses:metapopulation:exemplar-type)
! (import! &self metta-moses:metapopulation:metapopulation)


(= (getDisk (mkDeme (mkRep (mkKbMap (mkDscKbMp $idxMp) (mkDscMp $disc)) $tree) $instSet $id))
    $disc
)
(= (getTTableParams (mkTruthTableBScore $cpxCoeff $size $iTable))
    ($cpxCoeff $size $iTable)
)
(= (getITableParams (mkITable $rows $labels))
    ($rows $labels)
)

; (: isScored (-> (ScoredInstance Cscore) Bool))
; (= (isScored (mkSInst (mkPair $instance $score))) (~= $score (worstCscore)))

;; NOTE!!!!!!!
;; merger -- this is a function implemented as part if the main crossTopOne in optimization/hillclimbing/cross-top-one.metta but results in error unless this is imported in the test file
(: merger (-> Expression Instance Instance Expression))
(= (merger $targetInstances $baseInst $referenceInst)
    (collapse (let (mkSInst (mkPair $targetInst $score)) (superpose $targetInstances)
        (mkSInst (mkPair (mergeInstance $targetInst $baseInst $referenceInst) (worstCscore))))))



; !(bind! deme (mkDeme (mkRep (mkKbMap (mkDscKbMp (ConsMap ((mkNodeId (1)) 0) (ConsMap ((mkNodeId (2)) 1) NilMap))) (mkDscMp (ConsMMap ((mkDiscSpec 3) (mkLSK (mkDiscKnob (mkKnob (mkNodeId (1))) (mkMultip 3) (mkDiscSpec 0) (mkDiscSpec 0) Nil) )) (ConsMMap ((mkDiscSpec 3) (mkLSK (mkDiscKnob (mkKnob (mkNodeId (2))) (mkMultip 3) (mkDiscSpec 0) (mkDiscSpec 0) Nil))) NilMMap)))) (mkTree (mkNode AND) (Cons (mkNullVex (Cons (mkTree (mkNode A) Nil) Nil)) (Cons (mkNullVex (Cons (mkTree (mkNode B) Nil) Nil)) Nil)))) (mkSInstSet Nil) (mkDemeId "1")))
; !(bind! table (createTruthTableBScore 2 (mkITable
;                          (Cons (Cons False (Cons False (Cons False Nil))) 
;                          (Cons (Cons True (Cons False (Cons False Nil))) 
;                          (Cons (Cons False (Cons True (Cons False Nil)))
;                          (Cons (Cons True (Cons True (Cons True Nil))) Nil))))
;                          (Cons A (Cons B (Cons O Nil))))))

; !(bind! itable (mkITable
;                          (Cons (Cons False (Cons False (Cons False Nil))) 
;                          (Cons (Cons True (Cons False (Cons False Nil))) 
;                          (Cons (Cons False (Cons True (Cons False Nil)))
;                          (Cons (Cons True (Cons True (Cons True Nil))) Nil))))
;                          (Cons A (Cons B (Cons O Nil)))))


(= (hillClimbing $POASet $tTableBscorer $centerInst $scorer) 
  (case $scorer
    ((applyComplexityBasedScore
        (let*
          (($disc (getDisk $POASet))
            (($cpxCoeff $size $iTable) (getTTableParams $tTableBscorer)))
          (hillClimbing (1 3 0) (False False False $centerInst (worstCscore) (* -1 (pow-math 10 308)) 0 0 1) 
                        $POASet
                        ($disc $cpxCoeff $size $iTable)
                        $centerInst
                        estimateNeighborhood crossover 
                        sampleNewInstances transform)))
      (applyMutualInformationBasedScore
        (let*
          (

            (() (println! $tTableBscorer))
            (($rows $labels) (getITableParams $tTableBscorer))
            (() (println! (iTable: $tTableBscorer)))
            (() (println! (Rows: $rows)))
            (() (println! (Labels: $labels)))
          )
          (hillClimbing (3 3 1) (False False False $centerInst 0 0 0 0 1) 
              $POASet 
              ($labels $rows () ())
              $centerInst
              estimateNeighborhoodFs crossoverFs 
              sampleNewInstancesFs transformFs
          ))))))



(= (hillClimbing ($maxDist $minXoverNeighbors $bestPossibleScore) ;; Constant parameters
                 ($alreadyXover $lastChance $_ $prevCenter $bestSscore $bestScore $currentNInstances $d $i)
                 $POASet
                 ($neighborhood $rowOCoeff $size $iTable)
                 $centerInst
                 $estimateFunction $crossoverFunction $samplingFunction $transformFunction) ;; Helper functions

    (let*
    (
        (() (println! (Iteration $i running)))
        (() (println! ""))

        ($totalNNeighbors ($estimateFunction $d $neighborhood))

        (() (println! (Estimated neighbors: $totalNNeighbors)))
        (() (println! ""))

        ($nNewNeighbors (min $totalNNeighbors 2))

        ($largeNbh (>= $totalNNeighbors $minXoverNeighbors))
        ($xOver (and (> $i 2) (and (not $alreadyXover) (or $largeNbh $lastChance)))) ;; INFO: Has to be greater than 2 minimum.
        (() (println! (Crossover: $xOver)))
        (() (println! (size: $size)))
        (() (println! ($totalNNeighbors : $nNewNeighbors : $prevCenter : $POASet : $d)))
        (((mkDeme $rep $updatedInstSet $id) $newInstances) (case ($xOver $size)
                                                              (
                                                                ((True ()) (let ($updatedInstSet $newInstances) ($crossoverFunction $currentNInstances $nNewNeighbors $prevCenter $POASet) 
                                                                              ((mkDeme () $updatedInstSet ()) $newInstances)))
                                                                ((True $notEmpty)  ($crossoverFunction $currentNInstances $nNewNeighbors $prevCenter $POASet))
                                                                ((False ()) (let ($updatedInstSet $newInstances) ($samplingFunction $totalNNeighbors $nNewNeighbors $prevCenter (mkITable $rowOCoeff $neighborhood) $POASet $d)
                                                                              ((mkDeme () $updatedInstSet ()) $newInstances)))
                                                                ((False $notEmpty) ($samplingFunction $totalNNeighbors $nNewNeighbors $prevCenter $POASet $d))
                                                              )
                                                          ))
        (() (println! (Newinstances: $updatedInstSet)))
        (() (println! ""))

        ((mkSInstSet $scoredInstances) (if (or (== $rep ()) (== $id ())) ($transformFunction $updatedInstSet (mkITable $rowOCoeff $neighborhood))
                                          ($transformFunction $updatedInstSet $rep $iTable $rowOCoeff)))

        ($updatedDeme (if (or (== $rep ()) (== $id ())) 
                          (mkSInstSet $scoredInstances) 
                          (mkDeme $rep (mkSInstSet $scoredInstances) $id)))
        
        (() (println! (Scored instances: $scoredInstances)))
        (() (println! ""))

        ((mkSInst $newBestInstPair) (List.foldl ((curry2 returnBest) <) (mkSInst (mkPair $centerInst $bestSscore)) $scoredInstances))
        ($newBestScore (if (or (== $rep ()) (== $id ())) 
                          (Pair.second $newBestInstPair)
                          (getPenScore (Pair.second $newBestInstPair)))) ;; This could be the original best candidate or a new one.
        ($hasImproved (> $newBestScore (+ $bestScore 0.5))) ;; Hard coded the score_improved function for now. This is automated in the C++ version.
        
        (() (println! (Best instance: $newBestInstPair)))
        (() (println! (New best score: $newBestScore)))
        (() (println! ""))

        (() (println! (hasImproved: $hasImproved)))
        ($nextCenterInst (if $hasImproved (Pair.first $newBestInstPair) $centerInst))
        ; (() (println!  (new inst center: $nextCenterInst)))
        ($nextDistance (if $hasImproved 1 (if (not $xOver) (+ 1 $d) $d)))
        
        (() (println! (NextDistance: $nextDistance)))


        (($newCenter $newDeme ($newXover $newLastChance $newHasImproved $newPrevCenter $newBestSscore $nextNewBestScore $newCurrentNInstance $newDistance $newI))
                              (if (and (== $totalNNeighbors 1) (== $nextDistance 1))
                                  (hillClimbing ($maxDist $minXoverNeighbors $bestPossibleScore)
                                                ((and $xOver (not $hasImproved)) $lastChance $hasImproved $centerInst (Pair.second $newBestInstPair) $newBestScore (+ $currentNInstances $newInstances) $nextDistance (+ 1 $i)) ;; Updated state
                                                $updatedDeme
                                                ($neighborhood $rowOCoeff $size $iTable)
                                                $nextCenterInst
                                                $estimateFunction $crossoverFunction $samplingFunction $transformFunction)
                                  ($nextCenterInst $updatedDeme ((and $xOver (not $hasImproved)) $lastChance $hasImproved $centerInst (Pair.second $newBestInstPair) $newBestScore (+ $currentNInstances $newInstances) $nextDistance $i))))
        
        ($scoreToCompareBelow (if (or (== $rep ()) (== $id ())) $newBestSscore (getScore $newBestSscore)))
        (() (println! (Found newBestSscore After (+ 1 $i) iteration is: $newBestSscore $scoreToCompareBelow)))
        (() (println! ""))
    )
    (if (<= $bestPossibleScore $scoreToCompareBelow)
          (trace! (Terminating because of best possible score: $bestPossibleScore found $scoreToCompareBelow) ($newCenter $newDeme ($newXover $newLastChance $newHasImproved $newPrevCenter $newBestSscore $nextNewBestScore $newCurrentNInstance $newDistance $newI)))
          (let ($finalCenter $finalDeme ($finalXover $finalLastChance $finalHasImproved $finalPrevCenter $finalBestCscore $finalNewBestScore $finalCurrentNInstance $finalDistance $finalI))
            ;; TODO: The C++ version checks the hyper parameter hc_params.crossover before getting here.
            ;; TODO: The C++ version checks for the hc_params.widen_search too. For now, we're assuming it is always true. 
            ;;       Hence the omission of the distance check from the C++ Hence the omission of the distance check from the C++
            (if (and (and (not $newHasImproved) (not $newLastChance)) (not $newXover))
                ;; If we just did the nearest neighbors and found no improvement,
                ;;  then try again one last time, we may get lucky.
                (hillClimbing ($maxDist $minXoverNeighbors $bestPossibleScore)
                              ($newXover True $newHasImproved $newPrevCenter $newBestSscore $nextNewBestScore $newCurrentNInstance $newDistance (+ 1 $newI)) ;; Updated state
                              $newDeme
                              ($neighborhood $rowOCoeff $size $iTable)
                              $newCenter
                              $estimateFunction $crossoverFunction $samplingFunction $transformFunction)
                (trace! (Continue to next iteration because of no more last chance) ($newCenter $newDeme ($newXover $newLastChance $newHasImproved $newPrevCenter $newBestSscore $nextNewBestScore $newCurrentNInstance $newDistance $newI))) )
 
            (if (< $maxDist $finalDistance)
                (trace! (Terminating because of maximum distance reached) ($finalCenter $finalDeme ($finalXover False $hasImproved $finalPrevCenter $finalBestCscore $finalNewBestScore $finalCurrentNInstance $finalDistance $finalI))) ;; I don't know why I didn't do this earlier but incrementing is needed here.
                (hillClimbing ($maxDist $minXoverNeighbors $bestPossibleScore)
                              ($finalXover False $hasImproved $finalPrevCenter $finalBestCscore $finalNewBestScore $finalCurrentNInstance $finalDistance (+ 1 $finalI)) ;; Updated state
                              $finalDeme
                              ($neighborhood $rowOCoeff $size $iTable)
                              $finalCenter
                              $estimateFunction $crossoverFunction $samplingFunction $transformFunction))
          )
    ) 
    )          
)




; !(assertEqual
;    (let*
;      (
;        (($instance (mkDeme $rep (mkSInstSet $instSet) $id) $state) (hillClimbing deme table (mkInst (Cons 0 (Cons 0 Nil))) applyComplexityBasedScore))
;        ($instanceCount (List.length $instSet))
;        ($scoredElems (List.map isScored $instSet))
;      )
;      ((>= $instanceCount 4) (List.any $scoredElems)))
;    (True True))


; !(assertEqual
;    (let*
;      (
;        (($instance (mkSInstSet $instSet) $state) (hillClimbing (mkSInstSet Nil) itable (mkInst (Cons 1 (Cons 0 Nil))) applyMutualInformationBasedScore))
;        ($instanceCount (List.length $instSet))
;        ($scoredElems (List.map isScored $instSet))
;      )
;      ((>= $instanceCount 4) (List.any $scoredElems)))
;    (True True))

; !(assertEqual 
; (mkDeme (mkRep (mkKbMap (mkDscKbMp (ConsMap ((mkNodeId (1)) 0) (ConsMap ((mkNodeId (2)) 1) NilMap))) (mkDscMp (ConsMMap ((mkDiscSpec 3) (mkLSK (mkDiscKnob (mkKnob (mkNodeId (1))) (mkMultip 3) (mkDiscSpec 0) (mkDiscSpec 0) Nil))) (ConsMMap ((mkDiscSpec 3) (mkLSK (mkDiscKnob (mkKnob (mkNodeId (2))) (mkMultip 3) (mkDiscSpec 0) (mkDiscSpec 0) Nil))) NilMMap)))) (mkTree (mkNode AND) (Cons (mkNullVex (Cons (mkTree (mkNode A) Nil) Nil)) (Cons (mkNullVex (Cons (mkTree (mkNode B) Nil) Nil)) Nil)))) (mkSInstSet (Cons (mkSInst (mkPair (mkInst (Cons 0 (Cons 0 Nil))) (mkCscore -4 0 0.0 0.0 -4.0))) Nil)) (mkDemeId "1"))
; (mkDeme (mkRep (mkKbMap (mkDscKbMp (ConsMap ((mkNodeId (1)) 0) (ConsMap ((mkNodeId (2)) 1) NilMap))) (mkDscMp (ConsMMap ((mkDiscSpec 3) (mkLSK (mkDiscKnob (mkKnob (mkNodeId (1))) (mkMultip 3) (mkDiscSpec 0) (mkDiscSpec 0) Nil))) (ConsMMap ((mkDiscSpec 3) (mkLSK (mkDiscKnob (mkKnob (mkNodeId (2))) (mkMultip 3) (mkDiscSpec 0) (mkDiscSpec 0) Nil))) NilMMap)))) (mkTree (mkNode AND) (Cons (mkNullVex (Cons (mkTree (mkNode A) Nil) Nil)) (Cons (mkNullVex (Cons (mkTree (mkNode B) Nil) Nil)) Nil)))) (mkSInstSet (Cons (mkSInst (mkPair (mkInst (Cons 0 (Cons 0 Nil))) (mkCscore -4 0 0.0 0.0 -4.0))) Nil)) (mkDemeId "1"))
; )

(= (simulated-annealing $POASet $tTableBscorer $initCenter $hyperParams $scorer) 
  (case $scorer
    ((applyComplexityBasedScore
        (let*
          (($disc (getDisk $POASet))
            (($cpxCoeff $size $iTable) (getTTableParams $tTableBscorer)))
          (simulated-annealing $POASet
                        ($disc $cpxCoeff $size $iTable)
                        $initCenter
                        (1 3 0) $hyperParams
                        (False False $initCenter (worstCscore) (* -1 (pow-math 10 308)) 0 0 1)
                        estimateNeighborhood crossover 
                        sampleNewInstances transform)))
      (applyMutualInformationBasedScore
        (let*
          (

            (() (println! $tTableBscorer))
            (($rows $labels) (getITableParams $tTableBscorer))
            (() (println! (iTable: $tTableBscorer)))
            (() (println! (Rows: $rows)))
            (() (println! (Labels: $labels)))
          )
          (simulated-annealing $POASet
              ($labels $rows () ())
              $initCenter
              (3 3 1) $hyperParams
              (False False $initCenter 0 0 0 0 1) 
              estimateNeighborhoodFs crossoverFs 
              sampleNewInstancesFs transformFs
          ))))))

(= (simulated-annealing $POASet 
                         ($neighborhood $rowOCoeff $size $iTable)
                         $initCenter
                         ($maxDist $minXoverNeighbors $bestPossibleScore) ;; $constParams
                         ($Temp $finalTemp $coolRate $isInit $maxIter) ;; $hyperParams
                         ($alreadyXover $lastChance $prevCenter $bestSscore $bestScore $currentNInstances $d $i) ;; $params
                         $estimateFunction $crossoverFunction $samplingFunction $transformFunction
                         )
    (if (or (<= $Temp $finalTemp) (<= $maxIter 0))
          ($prevCenter $POASet ($prevCenter $bestSscore $bestScore $currentNInstances $d $i))
          (let*
            (
              ;; Estimate Neighborhood
              (() (println! (Iteration: $i , Temperature: $Temp)))
              (() (println! ""))
              ; (() (println! $hyper))
              ; (() (println! ($rows $labels)))
              ($totalNNeighbors ($estimateFunction $d $neighborhood))

              (() (println! (Estimated neighbors: $totalNNeighbors)))
              (() (println! ""))

              ($nNewNeighbors (min $totalNNeighbors 2))

              ($largeNbh (>= $totalNNeighbors $minXoverNeighbors))

              ($xOver (and (> $i 2) (and (not $alreadyXover) (or $largeNbh $lastChance))))
              (() (println! (Crossover: $xOver)))
              ;; Generate a new instance using crossover
              
              (((mkDeme $rep $updatedInstSet $id) $newInstances) (case ($xOver $size)
                                                                    (
                                                                      ((True ()) (let ($updatedInstSet $newInstances) ($crossoverFunction $currentNInstances $nNewNeighbors $prevCenter $POASet) 
                                                                                    ((mkDeme () $updatedInstSet ()) $newInstances)))
                                                                      ((True $notEmpty)  ($crossoverFunction $currentNInstances $nNewNeighbors $prevCenter $POASet))
                                                                      ((False ()) (let ($updatedInstSet $newInstances) ($samplingFunction $totalNNeighbors $nNewNeighbors $prevCenter (mkITable $rowOCoeff $neighborhood) $POASet $d)
                                                                                    ((mkDeme () $updatedInstSet ()) $newInstances)))
                                                                      ((False $notEmpty) ($samplingFunction $totalNNeighbors $nNewNeighbors $prevCenter $POASet $d))
                                                                    )))
              (() (println! (Newinstances: $updatedInstSet)))
              (() (println! ""))

              ;; Score the new instance
              ((mkSInstSet $scoredInstances) (if (or (== $rep ()) (== $id ())) ($transformFunction $updatedInstSet (mkITable $rowOCoeff $neighborhood))
                                          ($transformFunction $updatedInstSet $rep $iTable $rowOCoeff)))

              ($updatedPOASet (if (or (== $rep ()) (== $id ())) 
                          (mkSInstSet $scoredInstances) 
                          (mkDeme $rep (mkSInstSet $scoredInstances) $id)))
        

              (() (println! (Scored instances: $scoredInstances)))
              (() (println! ""))

              ((mkSInst $newBestInstPair) (List.foldl ((curry2 returnBest) <) (mkSInst (mkPair $centerInst $bestSscore)) $scoredInstances))
              ($newBestScore (if (or (== $rep ()) (== $id ())) 
                                (Pair.second $newBestInstPair)
                                (getPenScore (Pair.second $newBestInstPair)))) ;; This could be the original best candidate or a new one.
              ($hasImproved (> $newBestScore (+ $bestScore 0.5))) ;; Hard coded the score_improved function for now. This is automated in the C++ version.
        
              (() (println! (Best instance: $newBestInstPair)))
              (() (println! (New best score: $newBestScore)))
              (() (println! (Has Improved: $hasImproved)))
              (() (println! ""))

              ; ;; Calculate the energy of the current and new instance, and the acceptanceProb
              ($pScore (if (== $rep ()) $bestSscore (getPenScore $bestSscore)))
              ($energy (getEnergy $pScore))
              ($newEnergy (getEnergy $newBestScore))
              ($acceptanceProb (acceptanceProb $energy $newEnergy $Temp))
              ($randomFloat (rand-random)) ;; using random.random instead of numpy.random.rand
              ; ;; Actual acceptance probabitlity which is the product of the intensity and acceptance probability.
              ($actualAcceptanceProb (* $acceptanceProb 0.85))

              (() (println! (Energy: $energy)))
              (() (println! (New energy: $newEnergy)))
              (() (println! (Acceptance probability: $acceptanceProb)))
              (() (println! (Random float: $randomFloat)))
              (() (println! (Actual acceptance probability: $actualAcceptanceProb)))
              (() (println! ""))

              ; ;; Check for acceptance
              ($isAccepted (> $actualAcceptanceProb $randomFloat))
              
              (() (println! (Is accepted: $isAccepted)))
              (() (println! (New instances: $newInstances)))
              (() (println! ""))
              
              ($newPOASet (if $isAccepted $updatedPOASet $POASet))
              ($newCenter (if $isAccepted (Pair.first $newBestInstPair) $prevCenter))
              ($newDistance (if $hasImproved 1 (if (not $xOver) (+ 1 $d) $d)))
              ($newBestSscore (if $isAccepted (Pair.second $newBestInstPair) $bestSscore))
              ($newBestScore (if $isAccepted $newBestScore $bestScore))
              ($updatedNumInstances (+ $currentNInstances $newInstances))
              ($newTemp (- $Temp (* $coolRate $Temp)))
              ($newIterCount (- $maxIter 1))

                          
            )
            (simulated-annealing $newPOASet 
                                ($neighborhood $rowOCoeff $size $iTable)
                                $initCenter
                                ($maxDist $minXoverNeighbors $bestPossibleScore) ;; $constParams
                                ($newTemp $finalTemp $coolRate False $newIterCount) ;; $hyperParams
                                ((and $xOver (not $hasImproved)) $lastChance $newCenter $newBestSscore $newBestScore $updatedNumInstances $newDistance (+ 1 $i)) ;; $params
                                $estimateFunction $crossoverFunction $samplingFunction $transformFunction)
          )
          )
)

; (: getEnergy (-> Cscore Number))
(= (getEnergy $score)
  (* -1 $score)
  ; (if $isDeme
  ;     (let $pScore (getPenScore $score) (* -1 $pScore))
  ;     (* -1 $score)
  ; )
)

(: acceptanceProb (-> Number Number Number Number))
(= (acceptanceProb $energy1 $energy2 $Temp)
  (if (< $energy2 $energy1)
    1.0
    (math-exp (/ (- $energy1 $energy2) $Temp))
  )
)

!(bind! rand-random (py-atom random.random))
!(bind! math-exp (py-atom math.exp))

!(bind! itable (mkITable
                         (Cons (Cons False (Cons False (Cons False Nil))) 
                         (Cons (Cons True (Cons False (Cons False Nil))) 
                         (Cons (Cons False (Cons True (Cons False Nil)))
                         (Cons (Cons True (Cons True (Cons True Nil))) Nil))))
                         (Cons A (Cons B (Cons O Nil)))))

!(bind! deme (mkDeme (mkRep (mkKbMap (mkDscKbMp (ConsMap ((mkNodeId (1)) 0) (ConsMap ((mkNodeId (2)) 1) NilMap))) (mkDscMp (ConsMMap ((mkDiscSpec 3) (mkLSK (mkDiscKnob (mkKnob (mkNodeId (1))) (mkMultip 3) (mkDiscSpec 0) (mkDiscSpec 0) Nil) )) (ConsMMap ((mkDiscSpec 3) (mkLSK (mkDiscKnob (mkKnob (mkNodeId (2))) (mkMultip 3) (mkDiscSpec 0) (mkDiscSpec 0) Nil))) NilMMap)))) (mkTree (mkNode AND) (Cons (mkNullVex (Cons (mkTree (mkNode A) Nil) Nil)) (Cons (mkNullVex (Cons (mkTree (mkNode B) Nil) Nil)) Nil)))) (mkSInstSet Nil) (mkDemeId "1")))
!(bind! table (createTruthTableBScore 2 (mkITable
                         (Cons (Cons False (Cons False (Cons False Nil))) 
                         (Cons (Cons True (Cons False (Cons False Nil))) 
                         (Cons (Cons False (Cons True (Cons False Nil)))
                         (Cons (Cons True (Cons True (Cons True Nil))) Nil))))
                         (Cons A (Cons B (Cons O Nil))))))

(: isScored (-> (ScoredInstance Cscore) Bool))
(= (isScored (mkSInst (mkPair $instance $score))) (~= $score (worstCscore)))


!(assertEqual 
    !(let*
       (
            (($instance (mkDeme $rep (mkSInstSet $instSet) $id) $state) (simulated-annealing deme table (mkInst (Cons 0 (Cons 0 Nil))) (20 0.01 0.1 True 5) applyComplexityBasedScore))
            ($instanceCount (List.length $instSet))
            ($scoredElems (List.map isScored $instSet))
       )
       ((>= $instanceCount 4) (List.any $scoredElems)) 
    )
(True True)
)

!(assertEqual 
    !(let*
       (
            (($instance (mkSInstSet $instSet) $state) (simulated-annealing (mkSInstSet Nil) itable (mkInst (Cons 1 (Cons 0 Nil))) (20 0.01 0.1 True 5) applyMutualInformationBasedScore))
            ($instanceCount (List.length $instSet))
            ($scoredElems (List.map isScored $instSet))
       )
       ((>= $instanceCount 4) (List.any $scoredElems)) 
    )
(True True)
)