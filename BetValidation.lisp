(in-package "ACL2")
(include-book "structs")
(include-book "io-utilities" :dir :teachpacks)


; Checks boundary cases for the bet such as negative values
(defun isBetLegal (amount)
   (if 
    		(>= amount 0)
       		t
         		nil))

; Checks if player has enough chips for what they're offering
(defun isBetSufficient (curPlayer amount)
	(if 
    		(>= (player-chips curPlayer) amount)
     		t
     		nil))


; Main function called by game
; Determines if the bet given by the player is valid
; 	by checking if they have enough chips and it
;	is a legal move to do in the current situation.
(defun isBetValid (req)
   (let* ((amount (request-bet req)))
   (if (and
        (isBetLegal amount)
        (isBetSufficient (request-player req) amount))
       		t
         		nil)))
