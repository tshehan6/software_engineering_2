(in-package "ACL2")
(include-book "world" :dir :teachpacks)

; added by Tom - feel free to reformat and remove this comment, just let me know if anything important changes
(defstructure request
   (type       (:assert (stringp  type)))
   (player     (:assert (stringp  player)))
   (bet        (:assert (integerp bet)))
   (ready      (:assert (booleanp ready))))

(defstructure card
   (suit  		(:assert (integerp suit)))
   (value		(:assert (integerp value))))

(defstructure hand
   (cards 		(:assert (listp cards)))) 

(defstructure player
  (name			(:assert (stringp name)))
  (chips 		(:assert (integerp chips)))
  (call-amount		(:assert (integerp call-amount)))
  (ready 		(:assert (booleanp ready)))
  (cards			(:assert (hand-p cards))))

(defstructure gamestate
   (players		(:assert (listp players)))
   (common		(:assert (hand-p common)))
   (last-raise		(:assert (player-p last-raise)))
   (seed			(:assert (integerp seed)))
   (pot			(:assert (integerp pot))))
