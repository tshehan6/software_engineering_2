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

; Given a list of players and a string, iterates through the list
; looking for the player whose name matches the given string.
(defun getPlayer (players reqname)
   (if (consp players)
       (if (equal (player-name (car players)) reqname)
           (car players)
           (getPlayer (cdr players) reqname))
       nil))

; Main function called by game
; Determines if the bet given by the player is valid
; 	by checking if they have enough chips and it
;	is a legal move to do in the current situation.
(defun isBetValid (game req)
   (let* ((amount (request-bet req)))
   (if (and
        (isBetLegal amount)
        (isBetSufficient (getPlayer (gamestate-players game) 
                                    (request-player req)) 
                         amount))
       		t
         		nil)))
