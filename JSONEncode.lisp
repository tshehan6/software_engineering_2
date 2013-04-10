; Tom Shehan
; Team Stroustrup
; Encode / Decode Module

; includes and setup
(include-book "list-utilities" :dir :teachpacks)
(include-book "io-utilities" :dir :teachpacks)
(include-book "structs")

; get a token from a larger string with a given terminator
(defun getStringToken (charList terminator)
	; go until the end of the string or the terminating character is reached
	(if (or (not (consp charList)) (char-equal terminator (car charList)))

	  	; base case
     		'()

		;recursive case
		(cons (car charList) (getStringToken (cdr charList) terminator))
     	)
)

; convert a JSON string into a list of tokens
(defun tokenize (charList)
	
	; go until there are no more characters in the JSON character list
	(if (not (consp charList))

		;base case
		'()
		
		;recursive case
		(if (char-equal (car charList) #\')
		  	
		  	; string token
			(let* (( token (getStringToken (cdr charList) #\') ))
				(cons token (tokenize (nthcdr (+ 2 (length token)) charList)))
			)

			(if (or (char-equal (car charList) #\{) (char-equal (car charList) #\}) (char-equal (car charList) #\[) (char-equal (car charList) #\]) )

				; control token
				(cons (list (car charList)) (tokenize (cdr charList)))

				; unrecognized , unnecessary, or whitespace token
				(tokenize (cdr charList))
			)
		)
	)
)


; pair elements into tuples to create dictionary elements
(defun makeNameContentPairs (items)
	(if (consp items)
		(cons (list (car items) (cadr items)) (makeNameContentPairs (cddr items)))
		'()
	)
)

; generate a parse tree from the token list
(defun parser (tokens terminator) 

	(if (string-equal (chrs->str(car tokens)) terminator)

		; base case
		(list '() 1)

		; recursive case
		(if (string-equal (chrs->str(car tokens)) "[")

			; new child list
			(let*	(
					(newLevel (parser (cdr tokens) "]" )) 
					(newLevelContent (car newLevel)) 
					(newLevelLength (+ 1 (cadr newLevel)))
				)
				(let*	(
						(theRest (parser (nthcdr newLevelLength tokens) terminator)) 
						(restContent (car theRest)) 
						(restConsumed (cadr theRest))
					)
					(list (cons newLevelContent restContent) (+ restConsumed newLevelLength))
				)
			)
                    
			(if (string-equal (chrs->str(car tokens)) "{")

				; new child dictionary
				(let*	(
						(newLevel (parser (cdr tokens) "}" )) 
						(newLevelContent (car newLevel)) 
						(newLevelLength (+ 1 (cadr newLevel)))
					)
					(let*	(
							(theRest (parser (nthcdr newLevelLength tokens) terminator)) 
							(restContent (car theRest)) 
							(restConsumed (cadr theRest))
						)
						(list (cons (makeNameContentPairs newLevelContent) restContent) (+ restConsumed newLevelLength))
					)
				)
				
				; just another regular string token
				(let*	(
						(theRest (parser (cdr tokens) terminator)) 
						(restContent (car theRest)) 
						(restConsumed (cadr theRest))
					)
					(list (cons (chrs->str(car tokens)) restContent) (+ 1 restConsumed))
				)
			)
		) 
	)
)

; get the JSON in the correct form for tokenizing, tokenize it, parse it, and extract the result from its wrapper
(defun JSON->tree (JSON)
	(car(car(parser(tokenize (str->chrs JSON)) "" )))
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Request Converters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; ACL2 list to structure
(defun tree->request (tree)
  
	(if (consp tree)
            
		(let* ((theRest (tree->request (cdr tree))))
                  
			(if (string-equal (first (first tree)) "type")

				(update-request theRest :type (second (first tree)))

				(if (string-equal (first (first tree)) "player")

					(update-request theRest :player (second (first tree)))

					(if (string-equal (first (first tree)) "bet")

						(update-request theRest :bet (str->rat(second (first tree))))

						theRest


					)
					
				)

			)

		)

		(request "" "" 0)

	)

)
;JSON string to structure
(defun JSON->request (JSON)
	(tree->request(JSON->tree JSON))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Card Converters 
;;;;;;;;;;:;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; structure to JSON string
(defun card->JSON (card)
	(let* ((suite (card-suit card)) (value (card-value card)) )
		(concatenate 'string "['" (rat->str suite 0 ) "','" (rat->str value 0) "']")
	)
)
(defun toLetter (n)
  (if (= n 0)
      "d"
      (if (= n 1)
          "h"
          (if (= n 0)
              "c"
              "s"
          )
      )
  )
)
(defun cardimg->JSON (card)
	(let* ((suite (card-suit card)) (value (card-value card)) )
		(concatenate 'string "{'card' : 'cards/" (toLetter suite) (rat->str value 0) ".png' }")
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hand Converters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; helper - list of ACL2 lists to list of card structures
(defun cardListList->cardStructList (cards)

  (if (consp (cdr cards))
  
      (cons (card (str->rat (first (car cards))) (str->rat (second (car cards)))) (cardListList->cardStructList (cdr cards)))
      
      (list (card (str->rat (first (car cards))) (str->rat (second (car cards)))) )
      
  )
  
)

; helper for hand->JSON to keep interface consistent
(defun helper_hand->JSON (cards)
	(if (consp cards)
		(concatenate 'string (card->JSON (car cards)) (if (consp (cdr cards))", " "") (helper_hand->JSON (cdr cards)) )	
		""
	)
)

;structure to JSON string
(defun hand->JSON (hand)
	(let* ((cardlist (hand-cards hand)))
		(concatenate 'string "{'cards' : " "[" (helper_hand->JSON cardlist) "], 'handRank' : ['" (rat->str (first (hand-handRank hand)) 0) "', '" (rat->str(second (hand-handRank hand)) 0) "']}") 
	)  
)

; ACL2 list to structure
(defun tree->hand (tree)
  
	(if (consp tree)
            
		(let* ((theRest (tree->hand (cdr tree))))
                  
			(if (string-equal (first (first tree)) "cards")

				(update-hand theRest :cards (cardListList->cardStructList (second (first tree))))

				(if (string-equal (first (first tree)) "handRank")

					(update-hand theRest :handRank (list (str->rat(first(second (first tree)))) (str->rat(second(second (first tree)))) ))
					
					theRest
					
				)

			)

		)

		(hand '() '())

	)

)
; JSON string to structure
(defun JSON->hand (JSON)
  
	(tree->hand(JSON->tree JSON))
  
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Deck Converters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ACL2 list to structure
(defun tree->deck (tree)
  
	(if (consp tree)
            
		(let* ((theRest (tree->deck (cdr tree))))
                  
			(if (string-equal (first (first tree)) "cards")

				(update-deck theRest :cards (cardListList->cardStructList (second (first tree))))

				theRest
				
			)

		)

		(deck '())

	)

)
;structure to JSON string
(defun deck->JSON (deck)
	(let* ((cardlist (deck-cards deck)))
		(concatenate 'string "{'cards' : " "[" (helper_hand->JSON cardlist) "] }") 
	)  
)
; JSON string to structure
(defun JSON->deck (JSON)
  
	(tree->deck(JSON->tree JSON))
  
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Player Converters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; ACL2 list to structure
(defun tree->player (tree)
  
	(if (consp tree)
            
		(let* ((theRest (tree->player (cdr tree))))
                  
			(if (string-equal (first (first tree)) "cards")

				(update-player theRest :cards (hand (cardlistlist->cardstructlist(second(first (second (first tree))))) (list (str->rat(first(second(second (second (first tree)))))) (str->rat(second(second(second (second (first tree))))))) ))
 
				(if (string-equal (first (first tree)) "name")

					(update-player theRest :name (second (first tree)))
					
					(if (string-equal (first (first tree)) "ready")

                                            (update-player theRest :ready (if (string-equal(second (first tree)) "yes") T nil ))
					
                                            (if (string-equal (first (first tree)) "chips")

                                                (update-player theRest :chips (str->rat (second (first tree)) ))
					
                                                (if (string-equal (first (first tree)) "call-amount")

                                                    (update-player theRest :call-amount (str->rat (second (first tree)) ))
					
                                                    theRest
					
                                                )
					
                                            )
					
                                   )
					
				)

			)

		)

		(player "" 0 0 nil (hand '() '()))

	)

)
;JSON string to structure
(defun JSON->player (JSON)
	(tree->player(JSON->tree JSON))
)

; structure to JSON string
(defun player->JSON (player)
	(let* ((cards (hand->JSON (player-cards player))))
		(concatenate 'string "{'name' : '" (player-name player) "', 'cards' : " cards " , 'ready' : '" ( if (player-ready player) "yes" "no") "', 'chips' : '" (rat->str(player-chips player) 0) "', 'call-amount' : '" (rat->str(player-call-amount player) 0) "' }") 
	)
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Gamestate Converters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; helper - list of ACL2 lists to list of player structures
(defun playerListList->playerStructList (players)

  (if (consp (cdr players))

      (cons (tree->player (car players)) (playerListList->playerStructList (cdr players)))
      
      (list  (tree->player (car players)))
      
  )
  
)

; ACL2 list to structure
(defun tree->gamestate (tree)

	(if (consp tree)
            
		(let* ((theRest (tree->gamestate (cdr tree))))
                  
			(if (string-equal (first (first tree)) "players")

				(update-gamestate theRest :players (playerListList->playerStructList(second (first tree))) )

                          	(if (string-equal (first (first tree)) "pot")

                                    (update-gamestate theRest :pot (str->rat(second (first tree))) )

                                    (if (string-equal (first (first tree)) "common")

                                        (update-gamestate theRest :common (tree->hand(second (first tree))) )

                                        (if (string-equal (first (first tree)) "last-raise")

                                            (update-gamestate theRest :last-raise (second (first tree)) )

                                            (if (string-equal (first (first tree)) "seed")

                                                (update-gamestate theRest :seed (str->rat(second (first tree))) )

                                                (if (string-equal (first (first tree)) "current-player-turn")

                                                    (update-gamestate theRest :current-player-turn (second (first tree)) )

                                                    (if (string-equal (first (first tree)) "game-status-message")

                                                        (update-gamestate theRest :game-status-message (second (first tree)) )

                                                        (if (string-equal (first (first tree)) "is-hand-over")

                                                            (update-gamestate theRest :is-hand-over (if (string-equal "yes" (second (first tree))) T nil) )

                                                            (if (string-equal (first (first tree)) "error-message")

                                                                (update-gamestate theRest :error-message (second (first tree)) )

                                                                (if (string-equal (first (first tree)) "deck")

                                                                    (update-gamestate theRest :deck (tree->deck(second (first tree)) ))

                                                                    theRest	
					
                                                                )	
					
                                                            )	
					
                                                        )	
					
                                                    )	
					
                                                )	
					
                                            )		
					
                                        )	
	
                                    )	
				
                           )
					
			)

		)

		(gamestate '() '() "" 0 0 '() "" "" nil "")

	)

)
;JSON string to structure
(defun JSON->gamestate (JSON)
	(tree->gamestate(JSON->tree JSON))
)

; list of players to json list
(defun playerlist->JSON (players)
  (if (consp players )
      (concatenate 'string (player->JSON (car players)) (if (consp (cdr players)) "," "") (playerlist->JSON (cdr players)))
      ""
  )
  
)

;structure to JSON string
(defun gamestate->JSON (gamestate)
	(let* ((players (concatenate 'string "["(playerlist->JSON(gamestate-players gamestate ))"]")))
          (concatenate 'string "{'players' : " players ", 'common' :" (hand->JSON (gamestate-common gamestate)) ", 'last-raise' : '"(gamestate-last-raise gamestate)"', 'seed' : '" (rat->str(gamestate-seed gamestate) 0) "', 'pot':'"(rat->str(gamestate-pot gamestate)0)"', 'current-player-turn' : '" (gamestate-current-player-turn gamestate) "', 'game-status-message' : '" (gamestate-game-status-message gamestate) "' , 'is-hand-over' : '" (if (gamestate-is-hand-over gamestate) "yes" "no") "', 'error-message':'" (gamestate-error-message gamestate) "' , 'deck':" (deck->JSON(gamestate-deck gamestate)) " }") 
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Response Converters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; helper for hand->JSON to keep interface consistent
(defun helper_cardimgs->JSON (cards)
	(if (consp cards)
		(concatenate 'string (cardimg->JSON (car cards)) (if (consp (cdr cards))", " "") (helper_cardimgs->JSON (cdr cards)) )	
		""
	)
)

(defun others->JSON (others)

  (if (consp others )
      
      (concatenate 'string "{ 'name' : '" (response-other-player-name (car others)) "', 'money' : '" (rat->str(response-other-player-money (car others)) 0) "',  'cards' : [" (helper_cardimgs->JSON(response-other-player-cards (car others)))"]}" (if (consp (cdr others)) "," "") (others->JSON (cdr others)))
      ""
      
  )
  
)


;structure to JSON string
(defun response->JSON (response)
	(let* ((others (concatenate 'string "["(others->JSON(response-other-players response ))"]")))
		(concatenate 'string "{'player_cards' : " "[" (helper_cardimgs->JSON (response-player-cards response)) "], 'player_money' : '" (rat->str(response-player-money response)0) "', 'player_name' : '"(response-player-name response)"', 'pot' : '" (rat->str(response-pot response) 0) "' , 'other_players' : "others", 'community_cards' : [" (helper_cardimgs->JSON(response-community-cards response)) "] }") 
	)  
)


;test
;(deck->JSON(JSON->deck "{'cards' : [['1','1'],['2','3'],['4','5']]  }" ))
;(hand->JSON(JSON->hand "{'cards' : [['1','1'],['2','3'],['4','5']] , 'handRank' : ['10','20'] }" ))
;(player->JSON (JSON->player(player->JSON (JSON->player "{'name': 'tom', 'cards' : {'cards' : [['1','1'],['2','3'],['4','5']], 'handRank' : ['3','4']} , 'ready' : 'yes', 'chips' : '10' , 'call-amount' : '123' }"))))
;(JSON->gamestate(gamestate->JSON(JSON->gamestate "{'players' : [{'name': 'alice', 'cards' : {'cards' : [['1','1'],['2','3'],['4','5']], 'handRank' : ['3','4']} , 'ready' : 'yes', 'chips' : '10' , 'call-amount' : '123' },{'name': 'bob', 'cards' : {'cards' : [['1','1'],['2','3'],['4','5']], 'handRank' : ['3','4']} , 'ready' : 'yes', 'chips' : '10' , 'call-amount' : '123' }] , 'common' : {'cards' : [['1','1'],['2','3'],['4','5']] , 'handRank' : ['0','0'] } , 'last-raise' : 'eve' , 'seed' : '1235813' , 'pot' : '999' , 'current-player-turn' : 'alice' , 'game-status-message' : 'bob wins' , 'is-hand-over' : 'yes' , 'error-message' :'eve hacked it' , 'deck' : {'cards' : [['5','5'],['6','6'],['7','8']]  } }")))
;(response->JSON (response (list(card 5 5) (card 4 4)) 10 "tom" (list (response-other-player "test" 123 (list (card 1 2) (card 3 4))) (response-other-player "testing" 6543 (list (card 1 2) (card 3 4)))) (list (card 1 1) (card 2 2)) 40 "" "" nil ""))