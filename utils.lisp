(in-package "ACL2")
(include-book "shuffle-helpers")

;shuffles the deck of a gamestate struct and returns the gamestate
;gamestate must be a valid gamestate, but it does not matter what the condition
	;of its deck is (a new deck is created and then sorted)
;it is important that a new seed is added between shuffles, otherwise the decks will be identical
;this function uses the helper functions found in shuffle-helpers.lisp
;TODO: test to make sure the other gamestate variables are not modified
(defun shuffle-deck (gamestate)
   (if (gamestate-p gamestate)
       (let* ((seed (gamestate-seed gamestate))
  			(randoms (generate-randoms (len (deck-cards *newdeck*)) seed)));generate list of random ints based on seed
            (update-gamestate gamestate :deck (fisher-yates-shuffle *newdeck* randoms)))
       Nil))