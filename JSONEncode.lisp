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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; structure to JSON string
(defun card->JSON (card)
	(let* ((suite (card-suit card)) (value (card-value card)) )
		(concatenate 'string "['" (rat->str suite 0 ) "','" (rat->str value 0) "']")
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


;test
(hand->JSON(JSON->hand "{'cards' : [['1','1'],['2','3'],['4','5']] , 'handRank' : ['10','20'] }" ))
(player->JSON (JSON->player(player->JSON (JSON->player "{'name': 'tom', 'cards' : {'cards' : [['1','1'],['2','3'],['4','5']], 'handRank' : ['3','4']} , 'ready' : 'yes', 'chips' : '10' , 'call-amount' : '123' }"))))