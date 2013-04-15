(in-package "ACL2")
(include-book "io-utilities" :dir :teachpacks)
(include-book "structs")
(include-book "cards")
(include-book "utilities") ;included for testing purposes

;************************************************************************;
;                              After Turn                                ;
;                       author: Thomas Lindley                           ;
;************************************************************************;

;TODO: 	Write function for ace-low straights (isAceLowStraight)
;		Fix bug that breaks program if there are multiple winners
;		Manage split pots 
;		Hook up determineWinner() to afterTurn()


;Helper method for quicksort algorithm
;
;@param h: 		the list of items to be sorted
;@param lesser:	list of items lower than the pivot item
;@param greater:	list of items greater than pivot item
;@param pivot:		item being compared against
;@param value:		the data type to sort by
;@return:			sorted hand or players
(defun quickSortHelper (h lesser greater pivot value)
  (if (not(consp h))
      (list lesser greater)
      (let ((front (car h)))
        (cond 
            ;Sort hand by card value
		  ((equal value "value")
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
                                 value)))
		  ;Sort hand by card suit
            ((equal value "suit") 
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
                                 value)))
	       ;Sort players by handRank
            ((equal value "players")
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
                                 value)))))))
					
;Quick Sort algorithm used to sort hands and players.
;
;@param h: the list of items to be sorted
;@param value:	the data type to sort by
;@return: sorted hand or players
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

;Sorts by value the hand broken up by suit
;
;@param hand: the hand to be sorted
;@return: sorted hand
(defun sortBrokenUpHand (hand)
   (if (not (consp hand))
       nil
       (append (quickSort (car hand) "value")
               (sortBrokenUpHand (cdr hand)))))

;Finds the cards in a hand for a suit
;
;@param n: the suit
;@param hand: the hand to search through
;@return: list of cards matching suit n
(defun findCardsInSuit (n hand)
   (if (not (consp hand))
       nil
       (if (equal n (card-suit (car hand)))
           (cons (car hand) (findCardsInSuit n (cdr hand)))
           (findCardsInSuit n (cdr hand)))))

;Breaks up the hand into suits, so the cards can be sorted by value 
;within their suit by their value
;
;@param hand: the hand to be broken up
;@return: the cards in the hand sorted by suit and value
(defun breakUpSuits (hand)
   (if (not (consp hand))
       nil
       (let* ((diamonds (findCardsInSuit 0 hand))
              (hearts (findCardsInSuit 1 hand))
              (clubs (findCardsInSuit 2 hand))
              (spades (findCardsInSuit 3 hand)))
             (sortBrokenUpHand(list diamonds hearts clubs spades)))))
	
;isHighCard checks to see if a hand's rank is highCard
;
;@param h: the hand being checked
;@return:	the hand integer rank (1) 
;		along with the value of the card
(defun isHighCard (h)
  (list 1 (card-value (car h))))
  
;isPair checks to see if a hand's rank is twoPair
;
;@param h: the hand being checked
;@return:	the hand integer rank (2) along with the value of the pair
(defun isPair (h)
  (if (not (consp (cdr h)))
      nil
      (if (equal (card-value (car h)) (card-value (cadr h)))
          (list 2 (card-value (car h)))
          (isPair (cdr h)))))
		  
;isTwoPair checks to see if a hand's rank is twoPair
;
;@param h: the hand being checked
;@return:	the hand integer rank (3) 
;		along with the values of the two cards making up the twoPair
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
;@param h: the hand being checked
;@return:	the hand integer rank (4) 
;		along with the value of the threeKind
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

(defun valuesOfCards (cards)
  (if (not (consp cards))
      nil
      (cons (card-value (car cards)) 
            (valuesOfCards (cdr cards)))))
;isFlush checks to see if a hand's rank is flush
;
;@param h: the hand being checked
;@return:	the hand integer rank (5)
;		along with the cards of the flush sorted by value
(defun isFlush (h)
  (if (not (consp (cddddr h)))
      nil
  (let* ((sortedBySuit (breakUpSuits(quickSort h "suit")))
         (firstCard (first sortedBySuit))
         (firstCardSuit (card-suit firstCard))
         (secondCard (second sortedBySuit))
         (thirdCard (third sortedBySuit))
         (fourthCard (fourth sortedBySuit))
         (fifthCard (fifth sortedBySuit)))
        (if (and (equal firstCardSuit (card-suit secondCard))
                 (equal firstCardSuit (card-suit thirdCard))
                 (equal firstCardSuit (card-suit fourthCard))
                 (equal firstCardSuit (card-suit fifthCard)))
            (append '(5) (valuesOfCards (list firstCard secondCard
                                         thirdCard fourthCard fifthCard)))
            (isFlush (cdr sortedBySuit))))))
			
;isStraight checks to see if a hand's rank is straight
;
;@param h: the hand being checked
;@return:	the hand integer rank (6) 
;		along with the value of the highest card in the straight
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
;@param h: the hand being checked
;@return: the hand integer rank (7) 
;		along with the value threeKind followed by the value Pair
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
;@param h: the hand being checked
;@return: the hand integer rank (8) 
;		along with the value of the fourKind
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
;@param h: the hand being checked
;@return:	the hand integer rank (9) 
;		along with the value of the highest card in the straight
(defun isStraightFlush (h)
  (if (not (consp (cddddr h)))
      nil
      (let* ((sortedBySuit (breakUpSuits(quickSort h "suit")))
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

;Gets the correct number of kicker cards for a hand
;
;@param ranking: ranking of hand w/o kickers
;@param hand: hand to choose kickers from
;@param n: number of kickers to get
;@return: returns a list of kickers
(defun getKickers (ranking hand n)
   (let* ((icards (cdr ranking))
  		(icardsClone icards))
         (if (equal n 0)
       	   nil
             (if (equal (length icards) (length (add-to-set-eql (card-value (car hand)) 
                                                               icardsClone)))
                 (getKickers ranking (cdr hand) n)
                 (cons (card-value (car hand))
                       (getKickers ranking (cdr hand) (- n 1)))))))

;Adds the kicker cards to a hand's ranking
;
;@param ranking: current hand rank
;@param hand: hand to rank
;@return: returns the new ranking with kickers added
(defun addKickers (ranking hand)
   (let* ((integerRank (car ranking)))
         (cond ((equal integerRank 8)
                (append ranking (getKickers ranking hand 1)))
               ((equal integerRank 4)
                (append ranking (getKickers ranking hand 2)))
               ((equal integerRank 3)
                (append ranking (getKickers ranking hand 1)))
               ((equal integerRank 2)
                (append ranking (getKickers ranking hand 3)))
               ((equal integerRank 1)
                (append ranking (getKickers ranking hand 4))))))	

;getHandRank gets the rank for a hand
;
;@param hand:	the hand to be ranked
;@return:		the rank of the jand provided by the correct helper f(n)
(defun getHandRank (hand)
  (let* ((straightFlush (isStraightFlush hand))
         (fourKind (addKickers (isFourKind hand) hand))
         (fullHouse (isFullHouse hand))
         (straight (isStraight hand))
         (flush (isFlush hand))
         (threeKind (addKickers (isThreeKind hand) hand))
         (twoPair (addKickers (isTwoPair hand) hand))
         (pair (addKickers (isPair hand) hand))
         (highCard (addKickers (isHighCard hand) hand)))
        
    ;Assign the highest value to the hand possible    
    (cond (straightFlush straightFlush) 
          (fourKind fourKind) ; 1
          (fullHouse fullHouse)
          (straight straight) 
          (flush flush) 
          (threeKind threeKind) ; 2
          (twoPair twoPair) ; 1
          (pair pair) ; 3
          (highCard highCard)))) ; 4

;Finds the max value in at index n in a set of handRanks
;
;@param players: players to search through
;@param n: intdex to search at
;@param maxValue: highest found so far
;@return: returns max value at n from player handRanks
(defun maxAtNth (players n maxValue)
   (if (not (consp players))
       maxValue
       (let* ((thisValue (nth n (hand-handRank (player-cards (car players))))))
       	(if (> 	thisValue
            		maxValue)
              (maxAtNth (cdr players) n thisValue)
              (maxAtNth (cdr players) n maxValue)))))

;Breaks ties between a set of players with the same integer handRank (1-9)
;
;loop through all of the players
;compare the value at n to the known highest value at n (maxAtN) in the lists
;add any players higher than this to the return list
;	function that receives the return list checks the length of 
;	return list and last checked n vs highest n
;
;this function makes me feel all dirty inside :(
;CURRENTLY BREAKS FOR MULTIPLE WINNERS
;
;@param players: players to break
;@param return: return list to build
;@param n: current index being viewed
;@param rankLength: obvious
;@param loopsLeft: obvious
;@param maxAtN: max value at index n in the player handRanks
;@return: list of winners
(defun breakTie (players return n rankLength loopsLeft maxAtN)
   	(if (or 	(and (equal 1 (length players)) (equal 0 (length return)))
         		(equal n rankLength))
         players
         (if (equal loopsLeft 0)
             (breakTie return '() (+ 1 n) rankLength (length return) (maxAtNth return (+ 1 n) 0))
             (if (equal maxAtN (nth n (hand-handRank (player-cards (car players)))))
                 (breakTie (cdr players) (cons (car players) return) n rankLength (- loopsLeft 1) maxAtN)
                 (breakTie (cdr players) return n rankLength (- loopsLeft 1) maxAtN)))))

;Finds a list of players with the same integer handRank
;
;@param player: players to search through
;@param rank: rank to compare to
;@return: returns list of players with handRank integer rank
(defun playersWithSameHand (players rank)
   (if (not (consp players))
       nil
       (if (equal rank
            	   (car (hand-handRank ( player-cards (car players)))))
             (cons (car players) (playersWithSameHand (cdr players) rank))
             nil)))

;Determines the winner(s) from a list of players
;
;@param players: players to pick winner from
;@return: returns the player(s)
(defun determineWinner (players)
   (let* ((sortedPlayers (quickSort players "players"))
          (bestIntRank (car (hand-handRank (player-cards (car sortedPlayers)))))
          (potentialWinners (playersWithSameHand sortedPlayers bestIntRank))
          (winners (breakTie potentialWinners '() 0 (length (hand-handRank (player-cards (car sortedPlayers))))
                            (length potentialWinners) (maxAtNth potentialWinners 0 0))))
         winners))


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

;addCommonCardsToPlayersAndSort adds the common cards to the players' hands
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
      	(quickSort (cons 
                   	    (update-player thisPlayer :cards (make-hand :cards mergedHand 
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
                                  			:players (addCommonCardsToPlayersAndSort 
                                            			(gamestate-players gamestate)
                                            			(gamestate-common gamestate))))
          	(winner (car (gamestate-players game)))
          	(updatedWinner (update-player winner 
                                         	:chips (+ (gamestate-pot game) (player-chips winner)))))
        	(update-gamestate game
          				:players (append updatedWinner (cdr (gamestate-players game)))
                          	:last-raise ""
                             	:game-status-message (string-append (player-name updatedWinner)
                                                                  " won the game!" )))
        (if (isRoundOver gamestate);if the round of betting is over
            (readyGamestateForNextRound gamestate);set last-raise to "" & deal next card
            gamestate)));othewise a round of betting is still going on, let the gamestate pass through without being modified

;****************************Test Constants******************************;
(defconst *tester* 
  (let* ((cards (quickSort(list *C1* *S1* *D10* *H11* *D11* *D2* *D8*) "value"))
         (handRank (getHandRank cards)))
    (hand cards handRank)))

(defconst *tester2* 
  (let* ((cards (quickSort(list *C13* *S13* *D13* *H10* *D10* *C1* *H3*) "value"))
         (handRank (getHandRank cards)))
    (hand cards handRank)))

(defconst *tester3* 
  (let* ((cards (quickSort(list *C13* *S13* *D12* *H10* *D10* *C1* *H3*) "value"))
         (handRank (getHandRank cards)))
    (hand cards handRank)))

(defconst *tester4* 
  (let* ((cards (quickSort(list *C2* *S2* *D3* *H5* *D5* *C9* *H4*) "value"))
         (handRank (getHandRank cards)))
    (hand cards handRank)))

(defconst *player1*
   (player "1" 0 0 1 *tester*))

(defconst *player2*
   (player "2" 0 0 1 *tester2*))

(defconst *player3*
   (player "3" 0 0 1 *tester3*))

(defconst *player4*
   (player "4" 0 0 1 *tester4*))

(defconst *testPlayers* 
   (list *player1* *player2* *player3* *player4*))

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