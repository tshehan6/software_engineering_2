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
  (call-amount (:assert (integerp call-amount)))
  (ready (:assert (booleanp ready)))
  (cards (:assert (hand-p cards))))

(defstructure gamestate
  (players (:assert (listp players)))
  (common (:assert (hand-p common)))
  (last-raise (:assert (stringp last-raise)))
  (seed (:assert (integerp seed)))
  (pot (:assert (integerp pot)))
  (deck (:assert (deck-p deck)))
  (current-player-turn (:assert (stringp current-player-turn)))
  (game-status-message (:assert (stringp game-status-message)))
  (is-hand-over (:assert (booleanp is-hand-over)))
  (error-message (:assert (stringp error-message))))

(defstructure request
  (type (:assert (stringp type)))
  (player (:assert (stringp player)))
  (bet (:assert (integerp bet))))

(defstructure response-other-player
   	(name  (:assert (stringp name)))
   	(money (:assert (integerp money)))
   	(cards (:assert (listp cards))))

(defstructure response
   	(player-cards (:assert (listp player-cards)))
   	(player-money (:assert (integerp player-money)))
   	(player-name  (:assert (stringp player-name)))
   	(other-players (:assert (listp other-players)))
   	(community-cards (:assert (listp community-cards)))
   	(pot (:assert (integerp pot)))
   	(current-player-turn (:assert (stringp current-player-turn)))
   	(game-status-message (:assert (stringp game-status-message)))
   	(is-hand-over (:assert (booleanp is-hand-over)))
   	(error-message (:assert (stringp error-message))))