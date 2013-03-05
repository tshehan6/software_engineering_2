(in-package "ACL2")
(include-book "world" :dir :teachpacks)

(defstructure card
   (suit  		(:assert (integerp suit)))
   (value		(:assert (integerp value))))

(defstructure hand
   (cards 		(:assert (listp cards)))) 

(defstructure player
  (name			(:assert (stringp name)))
  (chips 		(:assert (integerp chips)))
  (call-amount	(:assert (integerp call-amount)))
  (ready       (:assert (booleanp ready)))
  (cards			(:assert (hand-p cards))))

(defstructure gamestate
   (players		(:assert (listp players)))
   (common		(:assert (hand-p common)))
   (last-raise	(:assert (player-p last-raise)))
   (seed			(:assert (integerp seed)))
   (pot			(:assert (integerp pot))))

(defstructure request
   (type       (:assert (stringp  type)))
   (player     (:assert (player-p player)))
   (bet        (:assert (integerp bet)))
   (ready      (:assert (booleanp ready))))
