(in-package "ACL2")
(include-book "cards")
(include-book "structs")
(include-book "util_helpers")

;initial gamestate used when running the program for the first time
;intiializes all values to Nil
(defconst *new-gamestate*
   (make-gamestate :players Nil
                   :common (make-hand :cards Nil
                                      :handRank Nil)
                   :last-raise ""
                   :seed -1
                   :pot -1
                   :deck (make-deck :cards Nil)
                   :current-player-turn ""
                   :game-status-message ""
                   :is-hand-over Nil
                   :error-message ""))
                   

;creates a deck, shuffles the deck, and then deals two cards to each player
;this should be used once all players are ready to play a new hand
(defun dealHands (gamestate)
   (if (gamestate-p gamestate)
       (dealPlayerHands (shuffleDeck gamestate))
       Nil))

;deals the flop (top three cards) to the common pile 
(defun dealFlop (gamestate)
   (if (gamestate-p gamestate)
       (let* ((cardsToDeal (take 3 (deck-cards 
                                    (gamestate-deck gamestate))))
              (newDeck (nthcdr 3 (deck-cards 
                                  (gamestate-deck gamestate)))))
           (update-gamestate 
            	gamestate
             	:common (make-hand :cards cardsToDeal
                                  :handRank Nil)
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

;finds and returns a player struct if there is a player with a name matching playerName
(defun getPlayer (playerName gamestate)
   (if (and (stringp playerName)
            (gamestate-p gamestate))
       (getPlayerHelper playerName (gamestate-players gamestate))
       Nil))