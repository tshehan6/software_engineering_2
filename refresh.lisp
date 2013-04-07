(in-package "ACL2")
(include-book "structs")
(include-book "utilities")
(include-book "refresh_helpers")

;read in gamestate, create a response struct for the player specified in request-player
	;and the write the gamestate out to the file
(defun refreshRequest (request)
   (if (request-p request)
       ;TODO: need to read in the gamestate from file using let*
            (updateResponseStructForPlayer (request-player request) (dealHands (shuffleDeck *test-gamestate*)))
       ;TODO: need to write the result above out to file
       Nil))
         