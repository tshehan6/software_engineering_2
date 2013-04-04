(in-package "ACL2")
(include-book "world" :dir :teachpacks)

(defstructure card
  (suit (:assert (integerp suit)))
  (value (:assert (integerp value))))

(defstructure hand
  (cards (:assert (listp cards)))
  (handRank (:assert (listp handRank))))

(defstructure deck
  (cards (:assert (listp cards))))

(defstructure player
  (name (:assert (stringp name)))
  (chips (:assert (integerp chips)))
  (ready (:assert (booleanp ready)))
  (cards (:assert (hand-p cards))))

(defstructure gamestate
  (players (:assert (listp players)))
  (common (:assert (hand-p common)))
  (last-raise (:assert (player-p last-raise)))
  (seed (:assert (integerp seed)))
  (pot (:assert (integerp pot)))
  (deck (:assert (deck-p deck))))

(defstructure request
  (type (:assert (stringp type)))
  (player (:assert (stringp player)))
  (bet (:assert (integerp bet))))

(defstructure response-other-player
   	(name  (:assert (stringp name)))
   	(money (:assert (integerp money)))
   	(cards (:assert (hand-p cards))))

(defstructure response
   	(player-cards (:assert (hand-p player-cards)))
   	(player-money (:assert (integerp player-money)))
   	(player-name  (:assert (stringp player-name)))
   	(other-players (:assert (listp other-players)))
   	(community-cards (:assert (hand-p community-cards)))
   	(pot (:assert (integerp pot))))
