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
(defconst *player1*
   (make-player :name "player_1"
                :chips 50
                :call-amount 0
                :ready t
                :cards (make-hand :cards Nil)))
(defconst *player2*
   (make-player :name "player_2"
                :chips 50
                :call-amount 0
                :ready t
                :cards (make-hand :cards Nil)))
(defconst *player3*
   (make-player :name "player_3"
                :chips 50
                :call-amount 0
                :ready t
                :cards (make-hand :cards Nil)))
(defconst *test-gamestate*
   (make-gamestate :seed 23423
                   :deck *newdeck*
                   :players (list *player1* *player2* *player3*)
                   :common (make-hand :cards Nil);(list *c3* *c1*))
                   :last-raise *player1*
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
                 (fisher-yates-shuffle 
                  (swap-cards (nth i (deck-cards deck)) 
                              (nth j (deck-cards deck)) deck)
                                       (cdr randoms)))
           deck);return the deck when all cards have been shuffled
       Nil));return Nil if invalid args were provided

;shuffles the deck of a gamestate struct and returns the gamestate
;gamestate must be a valid gamestate, but it does not matter what the condition
	;of its deck is (a new deck is created and then sorted)
;it is important that a new seed is added between shuffles, otherwise the decks will be identical
;this function uses the helper functions found in shuffle-helpers.lisp
;TODO: test to make sure the other gamestate variables are not modified
(defun shuffleDeck (gamestate)
   (if (gamestate-p gamestate)
       (let* ((seed (gamestate-seed gamestate))
  			(randoms (generate-randoms 
               	(len (deck-cards *newdeck*)) seed)));generate list of random ints based on seed
            (update-gamestate gamestate 
                              :deck (fisher-yates-shuffle *newdeck* randoms)))
       Nil))

;recursive helper to deal a list of cards to players in order
(defun dealCardsToPlayers (cards players)
   (if (and (listp cards)
            (listp players)
            (not (endp players)));base case, all players have been updated
       (let* ((numPlayers (len players))
              (card1 (nth 0 cards))
              (card2 (nth numPlayers cards))
              (newCards (remove card2 (remove card1 cards))))
             (cons (update-player (car players)
                                  :cards (make-hand :cards (list card1 card2)))
                   (dealCardsToPlayers newCards (cdr players))))
       Nil))

;deals two cards from the deck to each of the players
;deck should already be shuffled before calling this method
(defun dealPlayerHands (gamestate)
   (if (gamestate-p gamestate)
       (let* ((numPlayers (len (gamestate-players gamestate)))
              (cardsToDeal (take (* numPlayers 2) 
                                 (deck-cards (gamestate-deck gamestate))))
              (newPlayers (dealCardsToPlayers cardsToDeal 
                                              (gamestate-players gamestate)))
              (newDeck (nthcdr (* numPlayers 2) (deck-cards 
                                                 (gamestate-deck gamestate)))))
             (update-gamestate gamestate
                               :players newPlayers
                               :deck (make-deck :cards newDeck)))
       Nil))

;helper used by dealTurn and dealFlop to deal a single card from the deck to the common pile
(defun dealCardToCommonPile (gamestate)
   (if (gamestate-p gamestate)
       (let* ((topCard (take 1 (deck-cards 
                                (gamestate-deck gamestate))))
              (newDeck (nthcdr 1 (deck-cards 
                                  (gamestate-deck gamestate))))
              (currentCommonHand (hand-cards 
                                  (gamestate-common gamestate))))
             (update-gamestate gamestate
                               :common (make-hand 
                                        :cards (cons topCard currentCommonHand))
                               :deck (make-deck :cards newDeck)))
       Nil))

;removes the top three cards from the deck and deals them to the common pile 
(defun dealFlop (gamestate)
   (if (gamestate-p gamestate)
       (let* ((cardsToDeal (take 3 (deck-cards 
                                    (gamestate-deck gamestate))))
              (newDeck (nthcdr 3 (deck-cards 
                                  (gamestate-deck gamestate)))))
           (update-gamestate 
            	gamestate
             	:common (make-hand :cards cardsToDeal)
             	:deck (make-deck :cards newDeck)))
    Nil))

;deals the turn (4th common card) out to the common pile
(defun dealTurn (gamestate)
   (if (and (gamestate-p gamestate)
            (= 3 (len (hand-cards 
                       (gamestate-common gamestate)))))
       (dealCardToCommonPile gamestate)
       Nil))

;deals the river (5th & last common card) out to the common pile
(defun dealRiver (gamestate)
   (if (and (gamestate-p gamestate)
            (= 4 (len (hand-cards 
                       (gamestate-common gamestate)))))
       (dealCardToCommonPile gamestate)
       Nil))