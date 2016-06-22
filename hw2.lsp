; CS 161 Winter 2016: HW2 Solution Skeleton
; 904-063-131

; [!] Include OUR HW1 solution here!
; [!] IMPORTANT: Remove the below line when you submit your code!
;     We'll include the hw-1-solution using *our* path, which will likely
;     differ from yours
;(load "../hw1/hw-1-solution")
;(load "hw1.lsp")
;(load "hw-1-solution.lsp")

; -----------------------------------------------------------------------------
; Helper functions
; -----------------------------------------------------------------------------

; FUNCTION: NEWATM
; PURPOSE:  Produces a fresh, unused, unbound, unique symbol with the given
;           symName prefix and (ostensibly) random numerical suffix
; INPUT:    symName: symbol prefix for the new symbol
; OUTPUT:   new unbound symbol with symName prefix and numeric suffix
(defun NEWATM (symName)
    ; Generate the new symbol using gensym
    (let* ((new-sym (gensym (string symName))))
        (intern (string new-sym))
    )
)



; -----------------------------------------------------------------------------
; Utility Functions
; -----------------------------------------------------------------------------

; Tell whether a triplet has the given phrase
(defun IS-PHRASE-IN-TRIPLET (phrase triplet)
    (equal phrase (first triplet))
)

; Get a triplet with the given phrase
(defun GET-TRIPLET-FROM-LM (phrase lex-mem)
    (loop for triplet in lex-mem do (if (IS-PHRASE-IN-TRIPLET phrase triplet) (return triplet)))
)

; Add triplet to the front of *LM
(defun ADD-TRIPLET-TO-LM (triplet)
    (setq *LM (cons triplet *LM))
)

; FUNCTION: ADD-TO-LM
; PURPOSE:  Adds the given (phrase frame demon) triplet to the global LEX-MEM,
;           making sure to not add duplicate phrases
; INPUT:    phrase: a list of English words
;           frame: a frame associated with that phrase
;           demons: a list of 0 or more demon instantiations
; OUTPUT:   phrase-frame-demon triplet constructed
(defun ADD-TO-LM (phrase frame demons)

    (let* ((new-triplet (list phrase frame demons)) 
           (triplet (GET-TRIPLET-FROM-LM phrase *LM)))

        ; remove matched triplet entry if any
        (if (not (null triplet)) (setq *LM (set-difference *LM (list triplet))))

        ; add new triplet to the front of *LM
        (ADD-TRIPLET-TO-LM new-triplet)

        ; IMPORTANT: return new triplet
        new-triplet
    ) 
)

; -----------------------------------------------------------------------------

; IMPORTANT: for LOOKUP-PHRASE and LOOKUP-PHRASE-HELPER, we assume
; that the sentece argument is not nil because by the definiton
; of a phrase, it is a list of one or more words, thus it does
; not make sense to look up a phrase in an empty sentence
(defun LOOKUP-PHRASE-HELPER (sub-sent rest-sent lex-mem)
    (let* ((triplet (GET-TRIPLET-FROM-LM sub-sent lex-mem)))
        (cond
            ; none of the phrases are found
            ((null sub-sent) (list (list (subseq rest-sent 0 1) nil nil) (rest rest-sent)))

            ; not found yet, try a shorter phrase
            ((null triplet) 
                (LOOKUP-PHRASE-HELPER (subseq sub-sent 0 (- (length sub-sent) 1)) 
                                      (append (last sub-sent) rest-sent) 
                                      lex-mem))

            ; found the longest match
            (t (list triplet rest-sent)) 
        )
    )
)

; FUNCTION: LOOKUP-PHRASE
; PURPOSE:  Looks for the longest phrase at the start of sent that matches a
;           corresponding phrase in the input lex-mem, and if found, returns:
;           ((phrase frame demons) rest-sent)
;           ...for corresponding (phrase frame demons) triplet found in lex-mem
;           ...and rest-sent being the rest of the sentence minus the found phrase
;
;           If NOT found, returns:
;           ((phrase nil nil) rest-sent)
; INPUT:    sent: a list of English words
;           lex-mem: a lexical memory with ((phrase frame demon)*) triplets
; OUTPUT:   (see above in purpose)
(defun LOOKUP-PHRASE (sent lex-mem)
    (LOOKUP-PHRASE-HELPER sent nil lex-mem)
)

; -----------------------------------------------------------------------------

; Expand any filler (gap) in a list of slotfillers
(defun NEWGAPS-HELPER (slotFillers)
    (cond
        ((null slotFillers) nil)

        (t (append (list (first slotFillers) (NEWGAPS (second slotFillers))) (NEWGAPS-HELPER (nthcdr 2 slotFillers))))
    )
)

; FUNCTION: NEWGAPS
; PURPOSE:  Replaces all gaps in the input frames with unique, unbound gap names
;           and returns a copy of the resulting frame
; INPUT:    frame: a frame
; OUTPUT:   frame with now unique gap-names
(defun NEWGAPS (frame)
    (cond
        ; Base case: return nil atom 
        ((null frame) nil)

        ; otherwise, expand the atom if it is bound
        (t 
            (cond
                ; symbol found, return it
                ((symbolp frame) 
                    (let* ((new-gap (NEWATM frame)))
                        ; IMPORTANT: we use SET instead of SETQ bcos it will bind
                        ; nil to what new-gap evaluates to (which is the new unique gap name)
                        (set new-gap nil)
                        new-gap
                    )
                )
                
                ; otherwise, newAtom must be a frame at this point
                (t (cons (first frame) (NEWGAPS-HELPER (rest frame))))
            )
        )
    )
)

; -----------------------------------------------------------------------------


; FUNCTION: IS-SUBCLASS
; PURPOSE:  Queries the global *TX for whether or not the given entity
;           ISA member of the given class
; INPUT:    entity, class: items in a class hierarchy
; OUTPUT:   boolean indicating hierarchical relationship
;(defun IS-SUBCLASS (entity class)
(defun IS-SUBCLASS (entity class &optional (path nil))
    (cond 
        ; IMPORTANT: entity that matches exactly the class is a subclass of itself
        ((equal entity class) t)

        ; Check class relationships recursively
        (t 
            (let* ((new-path (cons entity path)))
                (loop for v in *TX do (if (equal (second v) entity) 
                                        (if (equal (third v) class) 
                                            (return t)  
                                            (if (and (not (member (third v) new-path)) 
                                                     (IS-SUBCLASS (third v) class new-path)) 
                                                (return t)
                                            )
                                        )
                                      )
                )
            )
        )
    )    
)

; -----------------------------------------------------------------------------

; IMPORTANT: only consider a matching bound CONatom 
(defun FIND-MATCHING-CON (list class)
    (loop for CONatom in list do (if (and (boundp CONatom) (IS-SUBCLASS (first (eval CONatom)) class)) (return CONatom)))
)

; Self-explanatory
(defun REVERSE-LIST (list)
    (if (null list) nil (append (REVERSE-LIST (rest list)) (list (first list))))
)

; Regardless of the dir, WM* is traversed from the beginning to the end
; In case of dir = BEF, WM* is reversed so it can be traversed from 
; the beginning to the end 
(defun FIND-CON-HELPER (mycon dir class index)
    (cond
        ; Get the subset of *WM following mycon
        ; dir = AFT
        ((equal dir 'AFT) (FIND-MATCHING-CON (subseq *WM (+ index 1) (length *WM)) class))

        ; Get the subset of *WM preceding mycon
        ; dir = BEF; reverse the list
        (t (FIND-MATCHING-CON (REVERSE-LIST (subseq *WM 0 index)) class))
    )
)

; FUNCTION: FIND-CON
; PURPOSE:  Returns the CONatom found by searching the *WM for a CONatom with a
;           pred of the given class starting at mycon in direction dir within
;           the global *WM
; INPUT:    mycon: where to start searching in *WM
;           dir: either 'BEF or 'AFT indicating search direction
;           class: predicate superclass for which to search
; OUTPUT:   found CONatom, or nil if not found
(defun FIND-CON (mycon dir class)
    (cond
        ((null *WM) nil)

        ; IMPORTANT: remember to return from the loop
        (t (loop for v from 0 to (- (length *WM) 1) by 1 do 
                            (if (equal (nth v *WM) mycon) 
                                (return (FIND-CON-HELPER mycon dir class v)))))
    )
)

; -----------------------------------------------------------------------------

; Return T if CONatom is embedded in slotfillers
(defun IS-CON-EMBEDDED-IN-SF (CONatom sf)
    (cond
        ; No more slotfillers in which to find CONatom
        ((null sf) nil)

        ; Search current filler and the rest of slotfiller pairs
        (t (or (IS-CON-EMBEDDED-IN-CON CONatom (second sf)) (IS-CON-EMBEDDED-IN-SF CONatom (nthcdr 2 sf))))
    )
)

; Return T if CONatom is embedded in another CONatom
; IMPORTANT: check if CONatom is embedded in a bound gap within a frame that is bound to some CONatom
(defun IS-CON-EMBEDDED-IN-CON (CONatom CONatom2)
    (cond
        ((null CONatom2) nil)

        ; CONatom is a frame, search it
        ((not (symbolp CONatom2)) (IS-CON-EMBEDDED-IN-SF CONatom (rest CONatom2)))

        ; CONatom found
        ((equal CONatom CONatom2) t)

        ; CONatom is bound, search the gap the filler is bound to
        ((boundp CONatom2) (IS-CON-EMBEDDED-IN-CON CONatom (eval CONatom2)))

        ; CONatom is unbound
        (t nil)
    )
)

; Return T if CONatom is embedded in any of the CONatoms in the *WM
(defun IS-CON-EMBEDDED (CONatom)
    (loop for atom in *WM do (if (and (not (equal CONatom atom)) (IS-CON-EMBEDDED-IN-CON CONatom atom)) (return t)))
)

; FUNCTION: MAIN-ACT
; PURPOSE:  Returns the first CONatom in *WM whose frame is an ACT, and is NOT
;           embedded in another *WM frame
; INPUT:    N/A
; OUTPUT:   CONatom
(defun MAIN-ACT ()
    (loop for atom in *WM do (if (and (boundp atom) 
                                      (IS-SUBCLASS (first (eval atom)) 'ACT) 
                                      (not (IS-CON-EMBEDDED atom))) 
                                 (return atom)))
)

; -----------------------------------------------------------------------------


; FUNCTION: GEN-DEMS
; PURPOSE:  Inserts the given CONATM at the front of every partial-demon-
;           instance in the given demons and then adds those completed
;           demon instantiations to the global *DM
; INPUT:    demons: a list of partial-demon-instantiations of the format:
;           ((demon-name arg2 arg3 ...)*)
;           conatm: a CONatom indicating which frame the demons work for
; OUTPUT:   current state of *DM after insertion
(defun GEN-DEMS (demons conatm)
    (loop for demon in (REVERSE-LIST demons) do (setq *DM (cons (cons (first demon) (cons conatm (rest demon))) *DM)))
    *DM
)



; -----------------------------------------------------------------------------
; Here There Be Demons
; -----------------------------------------------------------------------------

; FUNCTION: DEM-SRCH
; PURPOSE:  Searches the *WM for a CONatom whose frame is a member of the type
;           of the given class. If found, then sets the top-level gap in the
;           myslot slot in mycon to the found CONatom. Returns the found
;           CONatom if found, NIL otherwise. 
; INPUT:    mycon: the conatm this demon works for in the WK-MEM
;           myslot: the slot-name of the gap in myslot to bind when found
;           dir: either 'BEF or 'AFT indicating search direction
;           class: the predicate superclass for which to search
; OUTPUT:   found CONatom, or nil if nothing found
(defun DEM-SRCH (mycon myslot dir class)
    (let* ((found-CONatom (FIND-CON mycon dir class)))
        (if (not (null found-CONatom)) (set (GET-SF myslot (eval mycon)) found-CONatom))
    )
)

; -----------------------------------------------------------------------------


; FUNCTION: DEM-AMEND
; PURPOSE:  Searches the *WM for a CONatom whose frame is a member of the type
;           of the given class. If found, then inserts the top-level slot given
;           by myslot in the found CONatom to the given myfiller. 
;           Returns the found CONatom if found, NIL otherwise. 
; INPUT:    mycon: the conatm this demon works for in the WK-MEM
;           myslot: the slot-name to insert
;           myfiller: the filler of myslot to insert
;           dir: either 'BEF or 'AFT indicating search direction
;           class: the predicate superclass for which to search
; OUTPUT:   found CONatom, or nil if nothing found
(defun DEM-AMEND (mycon myslot myfiller dir class)
    (let* ((found-CONatom (FIND-CON mycon dir class)))
        (if (not (null found-CONatom)) (set found-CONatom (AMEND-SF myslot myfiller (eval found-CONatom))))
        found-CONatom
    )
)

; -----------------------------------------------------------------------------


; FUNCTION: DEM-REF
; PURPOSE:  Searches the *WM for a CONatom whose frame is a member of the type
;           of the given class. If found, then inserts the top-level slot given
;           by myslot in the found CONatom to the given mycon. 
;           Returns the found CONatom if found, NIL otherwise. 
; INPUT:    mycon: the conatm this demon works for in the WK-MEM
;           myslot: the slot-name to insert
;           dir: either 'BEF or 'AFT indicating search direction
;           class: the predicate superclass for which to search
; OUTPUT:   found CONatom, or nil if nothing found
(defun DEM-REF (mycon myslot dir class)
    (DEM-AMEND mycon myslot mycon dir class)
)



; -----------------------------------------------------------------------------
; Workhorse Functions
; -----------------------------------------------------------------------------

; FUNCTION: DEM-EXEC
; PURPOSE:  Repeatedly calls each active demon instantiation within the global
;           *DM until all active demons return nil. Whenever a demon
;           returns something non-nil, we remove it from the global *DM
;           and then call each remaining demon again
; INPUT:    N/A
; OUTPUT:   Status of *DM after executing all active demons (a list of all
;           remaining, active demon instantiations)
(defun DEM-EXEC ()
    (let* ((demon-to-remove (loop for demon in *DM do (if (apply (first demon) (rest demon)) (return demon)))))
        (if (null demon-to-remove) *DM (progn (setq *DM (set-difference *DM (list demon-to-remove))) (DEM-EXEC)))
    )
)

; -----------------------------------------------------------------------------


; FUNCTION: PARSE-SENT
; PURPOSE:  Performs a conceptual anaylsis of the input SENT using the known
;           phrases and interpretations within the global *LM.
; INPUT:    sent: list of English words comprising a sentence
; OUTPUT:   frame consisting of: (EXPAND (MAIN-ACT))
(defun PARSE-SENT (sent)
    (cond
        ; No more words in the sentence
        ((null sent) (EXPAND (MAIN-ACT)))

        ; Otherwise, do the following
        (t
            (let* ((lp-out (LOOKUP-PHRASE sent *LM)) 
                   (triplet (first lp-out)) 
                   (rest-sent (second lp-out)) 
                   (new-con (NEWATM 'CON)))
                
                ; Set new CONatom to NEWGAPS version of the frame
                (set new-con (NEWGAPS (second triplet)))

                ; Add newly created CONatom to the back of *WM
                (setq *WM (append *WM (list new-con)))

                ; Insert new CONatom to demons from triplet
                (GEN-DEMS (third triplet) new-con)

                ; Execute all demon instances
                (DEM-EXEC)

                ; Recursively call itself on the rest of sent from lp-out
                (PARSE-SENT rest-sent)
            )
        )
    )
)

; -----------------------------------------------------------------------------

