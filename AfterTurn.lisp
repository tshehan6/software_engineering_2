(in-package "ACL2")
(include-book "io-utilities" :dir :teachpacks)
(include-book "structs")
(include-book "cards")
(include-book "utilities") ;included for testing purposes

;************************************************************************;
;                              After Turn                                ;
;************************************************************************;

;TODO: 	Make sure ace-high hands win.
;		Tie-breaking

;Helper method for quicksort algorithm
;
;@param h: 			the list of items to be sorted
;@param lesser:		list of items lower than the pivot item
;@param greater:	list of items greater than pivot item
;@param pivot:		item being compared against
;@param value:		the data type to sort by
;@return:			sorted hand or players
(defun quickSortHelper (h lesser greater pivot value)
  (if (not(consp h))
      (list lesser greater)
      (let ((front (car h)))
		;Sort hand by card value
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
			;Sort hand by card suit
            (if (equal value "suit") 
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
				;Sort players by handRank
                (if (equal value "players")
                    (if (<= (car(hand-handRank (player-cards front))) 
                            (car(hand-handRank (player-cards pivot))))
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
					
;Quick Sort algorithm used to sort hands and players.
;
;@param h: 		the list of items to be sorted
;@param value:	the data type to sort by
;@return:		sorted hand or players
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
				
;isHighCard checks to see if a hand's rank is highCard
;
;@param h:	the hand being checked
;@return:	the hand integer rank (1) along with the value of the card
(defun isHighCard (h)
  (list 1 (card-value (car h))))
  
;isPair checks to see if a hand's rank is twoPair
;
;@param h:	the hand being checked
;@return:	the hand integer rank (2) along with the value of the pair
(defun isPair (h)
  (if (not (consp (cdr h)))
      nil
      (if (equal (card-value (car h)) (card-value (cadr h)))
          (list 2 (card-value (car h)))
          (isPair (cdr h)))))
		  
;isTwoPair checks to see if a hand's rank is twoPair
;
;@param h:	the hand being checked
;@return:	the hand integer rank (3) 
;			along with the values of the two cards making up the twoPair
(defun isTwoPair (h)
  (if (not (consp (cddr h)))
      nil
      (let* ((firstPair (isPair (list (first h) (second h))))
             (secondPair (isPair (cddr h))))
        (if(and firstPair secondPair)
           (list 3 (card-value (car h)) (cadr secondPair))
           (isTwoPair (cdr h))))))
		   
;isThreeKind checks to see if a hand's rank is threeKind
;
;@param h:	the hand being checked
;@return:	the hand integer rank (4) 
;			along with the value of the threeKind
(defun isThreeKind (h)
  (if (not (consp (cddr h)))
      nil
      (let* ((firstCard (first h))
             (firstCardRank (card-value firstCard))
             (secondCard (second h))
             (thirdCard (third h)))
        (if (and (equal firstCardRank (card-value secondCard))
                 (equal firstCardRank (card-value thirdCard)))
            (list 4 firstCardRank)
            (isThreeKind (cdr h))))))
			
;isFlush checks to see if a hand's rank is flush
;
;@param h:	the hand being checked
;@return:	the hand integer rank (5)
;			along with the cards of the flush sorted by value
(defun isFlush (h)
  (if (not (consp (cddddr h)))
      nil
  (let* ((sortedBySuit (quickSort h "suit"))
         (firstCard (first sortedBySuit))
         (firstCardSuit (card-suit firstCard))
         (secondCard (second sortedBySuit))
         (thirdCard (third sortedBySuit))
         (fourthCard (fourth sortedBySuit))
         (fifthCard (fifth sortedBySuit))
         (sortedbyValue (quickSort (list firstCard
                                        secondCard
                                        thirdCard
                                        fourthCard
                                        fifthCard) "value")))
        (if (and (equal firstCardSuit (card-suit secondCard))
                 (equal firstCardSuit (card-suit thirdCard))
                 (equal firstCardSuit (card-suit fourthCard))
                 (equal firstCardSuit (card-suit fifthCard)))
            (list 5 sortedByValue)
            (isFlush (cdr sortedBySuit))))))
			
;isStraight checks to see if a hand's rank is straight
;
;@param h:	the hand being checked
;@return:	the hand integer rank (6) 
;			along with the value of the highest card in the straight
(defun isStraight (h)
  (if (not (consp (cddddr h)))
      nil
      (let* ((highCardValue (card-value (first h)))
             (secondCard (card-value (second h)))
             (thirdCard (card-value (third h)))
             (fourthCard (card-value (fourth h)))
             (lowCardValue (card-value (fifth h)))
             (setOfCards (add-to-set-eql lowCardValue 
                         (add-to-set-eql fourthCard 
                         (add-to-set-eql thirdCard 
                         (add-to-set-eql secondCard 
                                         (list highCardValue)))))))
        (if (and (equal 5 (length setOfCards))
                 (equal 4 (- highCardValue lowCardValue)))
            (list 6 highCardValue)
            (isStraight (cdr h))))))
			
;isFullHouse checks to see if a hand's rank is fullHouse
;
;@param h:	the hand being checked
;@return:	the hand integer rank (7) 
;			along with the value threeKind followed by the value Pair
(defun isFullHouse (h)
  (if (not (consp (cdddr h)))
      nil
      (let* ((firstThreeKind (isThreeKind (list (first h) 
                                                (second h) 
                                                (third h))))
             (firstPair (isPair (cdddr h)))
             (secondThreeKind (isThreeKind (cddr h)))
             (secondPair (isPair (list (first h) (second h)))))
        (if (and firstThreeKind firstPair)
            (list 7 (cadr firstThreeKind) (cadr firstPair))
            (if (and secondThreeKind secondPair)
                (list 7 (cadr secondThreeKind) (cadr secondPair))
                (isFullHouse (cdr h)))))))
				
;isFourKind checks to see if a hand's rank is fourKind
;
;@param h:	the hand being checked
;@return:	the hand integer rank (8) 
;			along with the value of the fourKind
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
            (list 8 firstCardRank)
            (isFourKind (cdr h))))))
			
;isStraightFlush checks to see if a hand's rank is straightFlush
;
;@param h:	the hand being checked
;@return:	the hand integer rank (9) 
;			along with the value of the highest card in the straight
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
            (list 9 (card-value (first fiveCardHand)))
            (isStraightFlush (cdr h))))))
			
;isRoundOver checks to see if the round of betting has concluded
;It does this by checking to see if the player to last raise matches the
;current player.
;
;@param gamestate:	the current gamestate
;@return:			boolean indicating if the round is over
(defun isRoundOver (gamestate)
  (if (equal (gamestate-last-raise gamestate)
             (gamestate-current-player-turn gamestate))
      t
      nil))
	  
;isHandOver checks to see if the hand has concluded
;It does this by checking to see if the round is over and if all of the
;common cards have been dealt.
;
;@param gamestate:	the current gamestate
;@return:			boolean indicating if the hand is over
(defun isHandOver (gamestate)
  (if (and (isRoundOver gamestate)
           (equal 5 (length (hand-cards (gamestate-common gamestate)))))
      t
      nil))
	  
;getHandRank gets the rank for a hand
;
;@param hand:	the hand to be ranked
;@return:		the rank of the jand provided by the correct helper f(n)
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
									
;addCommonCardsToPlayersAndSort adds the common cards to the players hands
;and sorts the players using QuickSort
;
;@param players: 		players from the gamestate
;@param commonCards:	common cards from the gamestate
(defun addCommonCardsToPlayersAndSort (players commonCards)
  (if (not (consp players))
      nil
      (let* ((thisPlayer (car players))
             (mergedHand (quickSort (append (hand-cards commonCards) 
                                            (hand-cards (player-cards thisPlayer))) 
                                    "value")))
      	(quickSort (cons (update-player thisPlayer :cards (make-hand :cards mergedHand
                                                   :handRank (getHandRank mergedHand)))
                         (addCommonCardsToPlayersAndSort (cdr players) commonCards)) 
                    "players"))))
				
;called after a round of beting
;sets the last-raise to an empty string and deals the next common card	
(defun readyGamestateForNextRound (gamestate)
   (if (gamestate-p gamestate)
       (let* ((numCommons (len (hand-cards (gamestate-common gamestate))))
              (newGamestate (cond ((equal numCommons 0) (dealFlop gamestate))
                                  ((equal numCommons 3) (dealTurn gamestate))
                                  ((equal numCommons 4) (dealRiver gamestate))
                                  (t gamestate))))
             (update-gamestate newGamestate :last-raise ""))
       Nil))

;afterTurn is the main function of the module
;It checks to see if a hand is over and if a round of betting is over and
;modifies the gamestate accordingly
;
;@param gamestate:	the gamestate to be checked
;@return:			the modified gamestate
(defun afterTurn (gamestate)
    (if (isHandOver gamestate)
        (let* ((game (update-gamestate gamestate 
                                  :players (addCommonCardsToPlayersAndSort (gamestate-players gamestate)
                                                                    (gamestate-common gamestate))))
          	(winner (car (gamestate-players game)))
          	(updatedWinner (update-player winner :chips (+ (gamestate-pot game) (player-chips winner)))))
        	(update-gamestate game
          				:players (append updatedWinner (cdr (gamestate-players game)))
                          	:last-raise ""
                             	:game-status-message (string-append (player-name updatedWinner)
                                                                  " won the game!" )))
        (if (isRoundOver gamestate);if the round of betting is over
            (readyGamestateForNextRound gamestate);set last-raise to "" & deal next card
            gamestate)));othewise a round of betting is still going on, let the gamestate pass through without being modified
			
;(defconst *tester* 
;  (let* ((cards (quickSort (list *D12* *S9* *C5* *H4* *D3* *D2* *D1*) "value"))
;         (handRank (getHandRank cards)))
;    (hand cards handRank)))
;(defconst *tester2* 
;  (let* ((cards (quickSort (list *D12* *C9* *D5* *H4* *D11* *C2* *D1*) "value"))
;         (handRank (getHandRank cards)))
;    (hand cards handRank)))
;(defconst *tester3* 
;  (let* ((cards (quickSort (list *H12* *D9* *D5* *H4* *D8* *D2* *D1*) "value"))
;         (handRank (getHandRank cards)))
;    (hand cards handRank)))
;(defconst *tester4* 
;  (let* ((cards (quickSort (list *D13* *H12* *D11* *H9* *C10* *D2* *H1*) "value"))
;         (handRank (getHandRank cards)))
;    (hand cards handRank)))
	
;*tester*
;*tester2*
;*tester3*
;*tester4*

;(quickSort (list *tester* *tester2* *tester3* *tester4*) "hands")
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