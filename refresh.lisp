(in-package "ACL2")
(include-book "structs")
(include-book "io-utilities" :dir :teachpacks)
(include-book "utilities")
(include-book "refresh_helpers")

(set-state-ok t)

(defun toFile (state content file) 
		(string-list->file	
			file 
			(list content)
			state
		)
)

;read in gamestate, create a response struct for the player specified in request-player
	;and the write the gamestate out to the file
(defun refreshRequest (request)
   (if (request-p request)
       ;TODO: need to read in the gamestate from file using let*
            (updateResponseStructForPlayer (request-player request) (dealHands (shuffleDeck *test-gamestate*)))
       ;(toFile state (list "one" "two") "testfile.txt")
       Nil))
         