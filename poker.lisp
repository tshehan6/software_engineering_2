(in-package "ACL2")
(include-book "io-utilities" :dir :teachpacks)
(include-book "structs")
(include-book "cards")

(defun quickSortHelper (h lesser greater pivot value)
  (if (not(consp h))
      (list lesser greater)
      (let ((front (car h)))
        (if (equal value "value")
            (if (<= (card-value front) (card-value pivot))
                (quickSortHelper (cdr h) 
                                     (cons front lesser) 
                                     greater 
                                     pivot
                                     value)
                (quickSortHelper (cdr h) 
                                     lesser 
                                     (cons front greater) 
                                     pivot
                                     value))
            (if (equal value "suit") ;TODO sort cards in here by value as well
                (if (<= (card-suit front) (card-suit pivot))
                    (quickSortHelper (cdr h) 
                                         (cons front lesser) 
                                         greater 
                                         pivot
                                         value)
                    (quickSortHelper (cdr h) 
                                         lesser 
                                         (cons front greater) 
                                         pivot
                                         value))
                (if (equal value "hands")
                    (if (<= (car(hand-handRank front)) (car(hand-handRank pivot)))
                        (quickSortHelper (cdr h)
                                             (cons front lesser)
                                             greater
                                             pivot
                                             value)
                        (quickSortHelper (cdr h)
                                             lesser
                                             (cons front greater)
                                             pivot
                                             value))
                    nil))))))

(defun quickSort (h value)
  (if (or (not(consp  h)) (<= (length h) 1))
      h
      (let* ((lesser '())
             (greater '())
             (pivotIndex (ceiling (length h) 2))
             (pivot (Nth pivotIndex h))
             (landg (reverse (quickSortHelper 
                              (remove-equal pivot h) 
                              lesser 
                              greater 
                              pivot 
                              value))))
        (append (quickSort(first landg) value)
                (list pivot)
                (quickSort(second landg) value)))))

(defun isHighCard (h)
  (mv 1 (card-value (car h))))

(defun isPair (h)
  (if (not (consp (cdr h)))
      nil
      (if (equal (card-value (car h)) (card-value (cadr h)))
          (mv 2 (card-value (car h)))
          (isPair (cdr h)))))

(defun isTwoPair (h)
  (if (not (consp (cddr h)))
      nil
      (let* ((firstPair (isPair (list (first h) (second h))))
             (secondPair (isPair (cddr h))))
        (if(and(isPair (list (first h) (second h))) (isPair(cddr h)))
           (mv 3 (card-value (car h)) (cadr secondPair))
           (isTwoPair (cdr h))))))

(defun isThreeKind (h)
  (if (not (consp (cddr h)))
      nil
      (let* ((firstCard (first h))
             (firstCardRank (card-value firstCard))
             (secondCard (second h))
             (thirdCard (third h)))
        (if (and (equal firstCardRank (card-value secondCard))
                 (equal firstCardRank (card-value thirdCard)))
            (mv 4 firstCardRank)
            (isThreeKind (cdr h))))))

(defun isFlush (h)
  (let* ((sortedBySuit (quickSort h "suit"))
         (firstCard (first sortedBySuit))
         (firstCardSuit (card-suit firstCard))
         (secondCard (second sortedBySuit))
         (thirdCard (third sortedBySuit))
         (fourthCard (fourth sortedBySuit))
         (fifthCard (fifth sortedBySuit)))
    (if (not (consp (cddddr sortedBySuit)))
        nil
        (if (and (equal firstCardSuit (card-suit secondCard))
                 (equal firstCardSuit (card-suit thirdCard))
                 (equal firstCardSuit (card-suit fourthCard))
                 (equal firstCardSuit (card-suit fifthCard)))
            (mv 5 (card-value firstCard)) ;TODO will need better tie breaking figures
            (isFlush (cdr sortedBySuit))))))

(defun isStraight (h)
  (if (not (consp (cddddr h)))
      nil
      (let* ((highCardValue (card-value (first h)))
             (lowCardValue (card-value (fifth h))))
        (if (equal 4 (- highCardValue lowCardValue))
            (mv 6 highCardValue)
            (isStraight (cdr h))))))

(defun isFullHouse (h)
  (if (not (consp (cdddr h)))
      nil
      (let* ((firstThreeKind (isThreeKind (list (first h) (second h) (third h))))
             (firstPair (isPair (cdddr h)))
             (secondThreeKind (isThreeKind (cddr h)))
             (secondPair (isPair (list (first h) (second h)))))
        (if (and firstThreeKind firstPair)
            (mv 7 (cadr firstThreeKind) (cadr firstPair))
            (if (and secondThreeKind secondPair)
                (mv 7 (cadr secondThreeKind) (cadr secondPair))
                (isFullHouse (cdr h)))))))

(defun isFourKind (h)
  (if (not (consp (cdddr h)))
      nil
      (let* ((firstCard (first h))
             (firstCardRank (card-value firstCard))
             (secondCard (second h))
             (thirdCard (third h))
             (fourthCard (fourth h)))
        (if (and (equal firstCardRank (card-value secondCard))
                 (equal firstCardRank (card-value thirdCard))
                 (equal firstCardRank (card-value fourthCard)))
            (mv 8 firstCardRank)
            (isFourKind (cdr h))))))

(defun isStraightFlush (h)
  (if (not (consp (cddddr h)))
      nil
      (let* ((sortedBySuit (quickSort h "suit"))
             (firstCard (first sortedBySuit))
             (secondCard (second sortedBySuit))
             (thirdCard (third sortedBySuit))
             (fourthCard (fourth sortedBySuit))
             (fifthCard (fifth sortedBySuit))
             (fiveCardHand (quickSort(list firstCard secondCard
                                               thirdCard fourthCard
                                               fifthCard)
                                         "value")))
        (if (and (isFlush fiveCardHand) (isStraight fiveCardHand))
            (mv 9 (card-value (first fiveCardHand)))
            (isStraightFlush (cdr h))))))
;(defun isGameOver (gamestate))
;(defun isHandOver (gamestate))
;(defun rankHands (hands))

(defun getHandRank (hand)
  (let* ((straightFlush (isStraightFlush hand))
         (fourKind (isFourKind hand))
         (fullHouse (isFullHouse hand))
         (straight (isStraight hand))
         (flush (isFlush hand))
         (threeKind (isThreeKind hand))
         (twoPair (isTwoPair hand))
         (pair (isPair hand))
         (highCard (isHighCard hand)))
    (if straightFlush
        straightFlush
        (if fourKind
            fourKind
            (if fullHouse
                fullHouse
                (if straight
                    straight
                    (if flush
                        flush
                        (if threeKind
                            threeKind
                            (if twoPair
                                twoPair
                                (if pair
                                    pair
                                    highCard))))))))))

(defconst *tester* 
  (let* ((cards (quickSort (list *D12* *S9* *C5* *H4* *D3* *D2* *D1*) "value"))
         (handRank (getHandRank cards)))
    (hand cards handRank)))
(defconst *tester2* 
  (let* ((cards (quickSort (list *D12* *C9* *D5* *H4* *D11* *C2* *D1*) "value"))
         (handRank (getHandRank cards)))
    (hand cards handRank)))
(defconst *tester3* 
  (let* ((cards (quickSort (list *H12* *D9* *D5* *H4* *D8* *D2* *D1*) "value"))
         (handRank (getHandRank cards)))
    (hand cards handRank)))
(defconst *tester4* 
  (let* ((cards (quickSort (list *D12* *H9* *D5* *H4* *C12* *D2* *H1*) "value"))
         (handRank (getHandRank cards)))
    (hand cards handRank)))
;*tester*
;*tester2*
;*tester3*
;*tester4*

(quickSort (list *tester* *tester2* *tester3* *tester4*) "hands")
;(quickSort (hand-cards *tester*) "suit")

;(isHighCard (hand-cards *tester*))
;(isPair (hand-cards *tester*))
;(isTwoPair (hand-cards *tester*))
;(isThreeKind (hand-cards *tester*))
;(isFlush (hand-cards *tester*))
;(isStraight (hand-cards *tester*))
;(isFullHouse (hand-cards *tester*))
;(isFourKind (hand-cards *tester*))
;(isStraightFlush (hand-cards *tester*))
;(getHandRank (hand-cards *tester*))