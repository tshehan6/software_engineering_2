(in-package "ACL2")
(include-book "doublecheck" :dir :teachpacks)
(include-book "testing" :dir :teachpacks)
(include-book "AfterTurn")

;****************************Test Constants******************************;
(defconst *StraightFlush* 
  (let* ((cards (quickSort(list *D10* *D9* *D8* *D7* *D6* *H1* *S2*) "value"))
         (handRank (getHandRank cards)))
    (hand cards handRank)))

(defconst *FourKind* 
  (let* ((cards (quickSort(list *D7* *H7* *C7* *S7* *D13* *H10* *C2*) "value"))
         (handRank (getHandRank cards)))
    (hand cards handRank)))

(defconst *FullHouse* 
  (let* ((cards (quickSort(list *D7* *H7* *C7* *D6* *S6* *H10* *C2*) "value"))
         (handRank (getHandRank cards)))
    (hand cards handRank)))

(defconst *Flush* 
  (let* ((cards (quickSort(list *D12* *D9* *D5* *D3* *D1* *H7* *S2*) "value"))
         (handRank (getHandRank cards)))
    (hand cards handRank)))
	
(defconst *Straight* 
  (let* ((cards (quickSort(list *D12* *D11* *C9* *D10* *C1* *H8* *S2*) "value"))
         (handRank (getHandRank cards)))
    (hand cards handRank)))
	
(defconst *ThreeKind* 
  (let* ((cards (quickSort(list *D7* *H7* *C7* *D13* *D8* *C6* *S4*) "value"))
         (handRank (getHandRank cards)))
    (hand cards handRank)))

(defconst *TwoPair* 
  (let* ((cards (quickSort(list *D12* *C12* *D10* *H10* *D1* *H7* *S2*) "value"))
         (handRank (getHandRank cards)))
    (hand cards handRank)))

(defconst *Pair* 
  (let* ((cards (quickSort(list *D12* *C12* *H7* *D5* *D4* *C3* *S2*) "value"))
         (handRank (getHandRank cards)))
    (hand cards handRank)))


(defconst *player1*
   (player "1" 0 0 1 *Pair*))

(defconst *player2*
   (player "2" 0 0 1 *TwoPair*))

(defconst *player3*
   (player "3" 0 0 1 *ThreeKind*))

(defconst *player4*
   (player "4" 0 0 1 *Flush*))
   
(defconst *player5*
   (player "5" 0 0 1 *Straight*))

(defconst *player6*
   (player "6" 0 0 1 *FullHouse*))

(defconst *player7*
   (player "7" 0 0 1 *FourKind*))

(defconst *player8*
   (player "8" 0 0 1 *StraightFlush*))

(defconst *testPlayers* 
   (list *player3* *player2* *player1* *player7*
		*player8* *player6* *player4* *player5*	))

;Tests quicksort player sorting
(check-expect (first (quickSort *testPlayers* "players"))
              *player8*)

(check-expect (second (quickSort *testPlayers* "players"))
              *player7*)

(check-expect (third (quickSort *testPlayers* "players"))
              *player6*)

(check-expect (fourth (quickSort *testPlayers* "players"))
              *player5*)

(check-expect (fifth (quickSort *testPlayers* "players"))
              *player4*)

(check-expect (sixth (quickSort *testPlayers* "players"))
              *player3*)

(check-expect (seventh (quickSort *testPlayers* "players"))
              *player2*)

(check-expect (eighth (quickSort *testPlayers* "players"))
              *player1*)


;Hand checking functions
;Also indirectly tests quicksort on hands

;Straight Flush
(check-expect (isStraightFlush (hand-cards *StraightFlush*))
				(list 9 10))
(check-expect (isStraightFlush (hand-cards *fourkind*))
				nil)
(check-expect (isStraightFlush (hand-cards *fullhouse*))
				nil)
(check-expect (isStraightFlush (hand-cards *flush*))
				nil)
(check-expect (isStraightFlush (hand-cards *straight*))
				nil)
(check-expect (isStraightFlush (hand-cards *threekind*))
				nil)
(check-expect (isStraightFlush (hand-cards *twopair*))
				nil)
(check-expect (isStraightFlush (hand-cards *pair*))
				nil)
(check-expect (getHandRank (hand-cards *StraightFlush*))
				(list 9 10))

;Four of a Kind
(check-expect (isFourKind (hand-cards *straightflush*))
				nil)
(check-expect (isFourKind (hand-cards *fourkind*))
				(list 8 7))
(check-expect (isFourKind (hand-cards *fullhouse*))
				nil)
(check-expect (isFourKind (hand-cards *flush*))
				nil)
(check-expect (isFourKind (hand-cards *straight*))
				nil)
(check-expect (isFourKind (hand-cards *threekind*))
				nil)
(check-expect (isFourKind (hand-cards *twopair*))
				nil)
(check-expect (isFourKind (hand-cards *pair*))
				nil)
(check-expect (getHandRank (hand-cards *fourkind*))
				(list 8 7 13))

;Full House				
(check-expect (isFullHouse (hand-cards *straightflush*))
				nil)
(check-expect (isFullHouse (hand-cards *fourkind*))
				nil)
(check-expect (isFullHouse (hand-cards *fullhouse*))
				(list 7 7 6))
(check-expect (isFullHouse (hand-cards *flush*))
				nil)
(check-expect (isFullHouse (hand-cards *straight*))
				nil)
(check-expect (isFullHouse (hand-cards *threekind*))
				nil)
(check-expect (isFullHouse (hand-cards *twopair*))
				nil)
(check-expect (isFullHouse (hand-cards *pair*))
				nil)
(check-expect (getHandRank (hand-cards *FullHouse*))
				(list 7 7 6))
				
;Flush
(check-expect (isFlush (hand-cards *straightflush*))
				(list 5 10 9 8 7 6))
(check-expect (isFlush (hand-cards *fourkind*))
				nil)
(check-expect (isFlush (hand-cards *fullhouse*))
				nil)
(check-expect (isFlush (hand-cards *flush*))
				(list 5 14 12 9 5 3))
(check-expect (isFlush (hand-cards *straight*))
				nil)
(check-expect (isFlush (hand-cards *threekind*))
				nil)
(check-expect (isFlush (hand-cards *twopair*))
				nil)
(check-expect (isFlush (hand-cards *pair*))
				nil)
(check-expect (getHandRank (hand-cards *flush*))
				(list 5 14 12 9 5 3))

;Straight
(check-expect (isStraight (hand-cards *straightflush*))
				(list 6 10))
(check-expect (isStraight (hand-cards *fourkind*))
				nil)
(check-expect (isStraight (hand-cards *fullhouse*))
				nil)
(check-expect (isStraight (hand-cards *flush*))
				nil)
(check-expect (isStraight (hand-cards *straight*))
				(list 6 12))
(check-expect (isStraight (hand-cards *threekind*))
				nil)
(check-expect (isStraight (hand-cards *twopair*))
				nil)
(check-expect (isStraight (hand-cards *pair*))
				nil)
(check-expect (getHandRank (hand-cards *straight*))
				(list 6 12))
				
;Three of a Kind
(check-expect (isThreeKind (hand-cards *straightflush*))
				nil)
(check-expect (isThreeKind (hand-cards *fourkind*))
				(list 4 7))
(check-expect (isThreeKind (hand-cards *fullhouse*))
				(list 4 7))
(check-expect (isThreeKind (hand-cards *flush*))
				nil)
(check-expect (isThreeKind (hand-cards *straight*))
				nil)
(check-expect (isThreeKind (hand-cards *threekind*))
				(list 4 7))
(check-expect (isThreeKind (hand-cards *twopair*))
				nil)
(check-expect (isThreeKind (hand-cards *pair*))
				nil)
(check-expect (getHandRank (hand-cards *ThreeKind*))
				(list 4 7 13 8))

;Two Pair
(check-expect (isTwoPair (hand-cards *straightflush*))
				nil)
(check-expect (isTwoPair (hand-cards *fourkind*))
				(list 3 7 7))
(check-expect (isTwoPair (hand-cards *fullhouse*))
				(list 3 7 6))
(check-expect (isTwoPair (hand-cards *flush*))
				nil)
(check-expect (isTwoPair (hand-cards *straight*))
				nil)
(check-expect (isTwoPair (hand-cards *threekind*))
				nil)
(check-expect (isTwoPair (hand-cards *twopair*))
				(list 3 12 10))
(check-expect (isTwoPair (hand-cards *pair*))
				nil)
(check-expect (getHandRank (hand-cards *TwoPair*))
				(list 3 12 10 14))
				
;Two Pair
(check-expect (isPair (hand-cards *straightflush*))
				nil)
(check-expect (isPair (hand-cards *fourkind*))
				(list 2 7))
(check-expect (isPair (hand-cards *fullhouse*))
				(list 2 7))
(check-expect (isPair (hand-cards *flush*))
				nil)
(check-expect (isPair (hand-cards *straight*))
				nil)
(check-expect (isPair (hand-cards *threekind*))
				(list 2 7))
(check-expect (isPair (hand-cards *twopair*))
				(list 2 12))
(check-expect (isPair (hand-cards *pair*))
				(list 2 12))
(check-expect (getHandRank (hand-cards *Pair*))
				(list 2 12 7 5 4))