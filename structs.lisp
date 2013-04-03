(in-package "ACL2")
(include-book "world" :dir :teachpacks)

(defstructure card
	         (suit (:assert (integerp suit)))
		    (value	(:assert (integerp value))))

(defstructure hand
	         (cards (:assert (listp cards))))

(defstructure deck
   			(cards (:assert (listp cards))))

(defstructure player
	        (name	(:assert (stringp name)))
		  (chips (:assert (integerp chips)))
		    (call-amount	(:assert (integerp call-amount)))
		      (ready (:assert (booleanp ready)))
		        (cards	(:assert (hand-p cards))))

(defstructure gamestate
	         (players	(:assert (listp players)))
		    (common	(:assert (hand-p common)))
		       (last-raise	(:assert (player-p last-raise)))
		          (seed	(:assert (integerp seed)))
			     (pot	(:assert (integerp pot)))
			(deck (:assert (deck-p deck))))

(defstructure request
	         (type (:assert (stringp type)))
		    (player (:assert (stringp player)))
		       (bet (:assert (integerp bet))))