(in-package "ACL2")
(include-book "cards")
(include-book "structs")


;generates a random number based on the provided seed using the linear congruential method
;literal values for a, c ,m are used to ensure that the period of the generation is equal to m
;this is sufficient, since m is greater than the number of cards in a deck
;values were found online here: http://ecee.colorado.edu/~mathys/ecen5022/slides/prng90.pdf
(defun rand-helper (seed)
  (if (integerp seed)
      (let* ((m 100)
             (a 41)
             (c 7))
        (mod (+ (* a seed) c) m)); x = (a*seed + c) mod m
      Nil))

;generates a list of n random integers based on the seed argument passed to the function
;this is intended to be used by the shuffling method by setting n=52 (number of cards in deck)
;and reading the random seed from the gamestate
(defun generate-randoms (n seed)
  (if (and (integerp n)
           (> n 0)
           (integerp seed))
      (let* ((val (rand-helper seed)))
        (cons val (generate-randoms (- n 1) val)))
      Nil))
  
;swaps the position of cards c1 and c2 returns a new deck
;used by the deck shuffling algorithm
;TODO... test the following:
	;1) no cards are lost in result
	;2) size of deck is preserved
	;3) no cards are duplicated in the result 
(defun swap-cards (c1 c2 deck)
   (if (and (card-p c1)
            (card-p c2)
            (deck-p deck))
       (let* ((pos1 (position c1 (deck-cards deck)))
              (pos2 (position c2 (deck-cards deck))))
             (make-deck :cards;create new deck
                        (update-nth pos1 c2 ;swap card2
                                    (update-nth pos2 c1 (deck-cards deck)))));swap card1 in the cards list
             Nil))

;constant used to initialize new deck before sorting
;this must contain all possible cards exactly once each... the accurracy of the shuffling depends on it
(defconst *newdeck* 
   (make-deck :cards (list
                      *D1* *D2* *D3* *D4* *D5* *D6* *D7*
                      *D8* *D9* *D10* *D11* *D12* *D13*
                      *H1* *H2* *H3* *H4* *H5* *H6* *H7*
                      *H8* *H9* *H10* *H11* *H12* *H13*
                      *C1* *C2* *C3* *C4* *C5* *C6* *C7*
                      *C8* *C9* *C10* *C11* *C12* *C13*
                      *S1* *S2* *S3* *S4* *S5* *S6* *S7*
                      *S8* *S9* *S10* *S11* *S12* *S13*)))

;Data used for testing:
(defconst *test-player*
   (make-player :name "player_1"
                :chips 50
                :call-amount 0
                :ready t
                :cards (make-hand :cards (list *c1* *d5*))))
(defconst *shuffle-test-gamestate*
   (make-gamestate :seed 23423532
                   :deck (make-deck :cards (list *c1*))
                   :players (list *test-player*)
                   :common (make-hand :cards (list *c3* *c1*))
                   :last-raise *test-player*
                   :pot 0))

;shuffles a deck using the Fisher-Yates shuffle and returns the shuffled deck
;args: deck can be any deck structure, randoms should be a list of reandom ints (created using the generate-randoms function) 
;algorithm can be referenced here: http://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle
(defun fisher-yates-shuffle (deck randoms)
   (if (and (deck-p deck)
            (integer-listp randoms))
       (if (> (len randoms) 1)
           (let* ((i (- (len randoms) 1));i is the upper bound of the shuffle 
                  (j (mod (car randoms) i)));j is position of the card that will be swapped with i
                 ;swap the cards at spots j & i, then continue recursion
                 (fisher-yates-shuffle (swap-cards (nth i (deck-cards deck)) (nth j (deck-cards deck)) deck)
                                       (cdr randoms)))
           deck);return the deck when all cards have been shuffled
       Nil));return Nil if invalid args were provided
