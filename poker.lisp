(in-package "ACL2")
(include-book "io-utilities" :dir :teachpacks)
(include-book "structs")
(include-book "cards")

(defun handQuickSortHelper (h lesser greater pivot value)
  (if (not(consp h))
      (list lesser greater)
      (let ((front (car h)))
        (if (equal value "rank")
            (if (<= (card-rank front) (card-rank pivot)) 
                (handQuickSortHelper (cdr h) 
                                     (cons front lesser) 
                                     greater 
                                     pivot
                                     value)
                (handQuickSortHelper (cdr h) 
                                     lesser 
                                     (cons front greater) 
                                     pivot
                                     value))
            (if (equal value "suit") ;TODO sort cards in here by rank as well
                (if (<= (card-suit front) (card-suit pivot))
                                    (handQuickSortHelper (cdr h) 
                                     (cons front lesser) 
                                     greater 
                                     pivot
                                     value)
                (handQuickSortHelper (cdr h) 
                                     lesser 
                                     (cons front greater) 
                                     pivot
                                     value))
                nil)))))

(defun handQuickSort (h value)
  (if (or (not(consp  h)) (<= (length h) 1))
      h
      (let* ((lesser '())
             (greater '())
             (pivotIndex (ceiling (length h) 2))
             (pivot (Nth pivotIndex h))
             (landg (reverse (handQuickSortHelper 
                              (remove-equal pivot h) 
                              lesser 
                              greater 
                              pivot 
                              value))))
        (append (handQuickSort(first landg) value)
                (list pivot)
                (handQuickSort(second landg) value)))))

(defun isHighCard (h)
  (mv 1 (card-rank (car h))))

(defun isPair (h)
  (if (not (consp (cdr h)))
      nil
      (if (equal (card-rank (car h)) (card-rank (cadr h)))
          (mv 2 (card-rank (car h)))
          (isPair (cdr h)))))

(defun isTwoPair (h)
  (if (not (consp (cddr h)))
      nil
      (let* ((firstPair (isPair (list (first h) (second h))))
             (secondPair (isPair (cddr h))))
        (if(and(isPair (list (first h) (second h))) (isPair(cddr h)))
           (mv 3 (card-rank (car h)) (cadr secondPair))
           (isTwoPair (cdr h))))))

(defun isThreeKind (h)
  (if (not (consp (cddr h)))
      nil
      (let* ((firstCard (first h))
             (firstCardRank (card-rank firstCard))
             (secondCard (second h))
             (thirdCard (third h)))
        (if (and (equal firstCardRank (card-rank secondCard))
                 (equal firstCardRank (card-rank thirdCard)))
            (mv 4 firstCardRank)
            (isThreeKind (cdr h))))))

(defun isFlush (h)
  (let* ((sortedBySuit (handQuickSort h "suit"))
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
            (mv 5 (card-rank firstCard)) ;TODO will need better tie breaking figures
            (isFlush (cdr sortedBySuit))))))

(defun isStraight (h)
  (if (not (consp (cddddr h)))
      nil
      (let* ((highCardValue (card-rank (first h)))
             (lowCardValue (card-rank (fifth h))))
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
             (firstCardRank (card-rank firstCard))
             (secondCard (second h))
             (thirdCard (third h))
             (fourthCard (fourth h)))
        (if (and (equal firstCardRank (card-rank secondCard))
                 (equal firstCardRank (card-rank thirdCard))
                 (equal firstCardRank (card-rank fourthCard)))
            (mv 8 firstCardRank)
            (isFourKind (cdr h))))))

(defun isStraightFlush (h)
  (if (not (consp (cddddr h)))
      nil
      (let* ((sortedBySuit (handQuickSort h "suit"))
             (firstCard (first sortedBySuit))
             (secondCard (second sortedBySuit))
             (thirdCard (third sortedBySuit))
             (fourthCard (fourth sortedBySuit))
             (fifthCard (fifth sortedBySuit))
             (fiveCardHand (handQuickSort(list firstCard secondCard
                                               thirdCard fourthCard
                                               fifthCard)
                                         "rank")))
        (if (and (isFlush fiveCardHand) (isStraight fiveCardHand))
            (mv 9 (card-rank (first fiveCardHand)))
            (isStraightFlush (cdr h))))))

(defconst *tester* (hand (handQuickSort (list *D12* *D9* *D5* *D4* *D3* *D2* *D1*) "rank") 1))
*tester*
(handQuickSort (hand-cards *tester*) "suit")

(isHighCard (hand-cards *tester*))
(isPair (hand-cards *tester*))
(isTwoPair (hand-cards *tester*))
(isThreeKind (hand-cards *tester*))
(isFlush (hand-cards *tester*))
(isStraight (hand-cards *tester*))
(isFullHouse (hand-cards *tester*))
(isFourKind (hand-cards *tester*))
(isStraightFlush (hand-cards *tester*))
