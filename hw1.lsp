; CS 161 Winter 2016: HW1
; 904-063-131

; -----------------------------------------------------------------------------
; Helper functions
; -----------------------------------------------------------------------------

; [!] TODO: Place any helpers that you'd like here!

; find a slot in a list of slotfillers
(defun GET-SF-HELPER (slot sfs)
  (cond
    ; no matching slot found
    ((null sfs) nil)

    ; matching slot found, return the filler
    ((equal slot (first sfs)) (second sfs))

    ; no matching slot found so far, check the next one
    (t (GET-SF-HELPER slot (nthcdr 2 sfs)))
  )
)

; check if symbol is unbound
(defun UNBOUND-SYMBOL-P (atom)
  (and (symbolp atom) (not (boundp atom)))
)

; given a concept, return a frame or an unbound symbol
(defun GET-FRAME-OR-USYM (concept)
  (cond
    ; IMPORTANT: nil is a bound symbol (to itself), so don't expand it recursively
    ((null concept) nil)

    ; evaluate bound symbol recursively until we get the actual frame or unbound symbol
    ((and (symbolp concept) (boundp concept)) (GET-FRAME-OR-USYM (eval concept)))

    ; otherwise, return the frame or unbound symbol
    (t concept)
  )
)

; return frame if (expanded) concept is a frame, otherwise return nil
(defun GET-FRAME (concept)
  (let* ((result (GET-FRAME-OR-USYM concept)))
    (cond
      ; unbound symbol found, return nil
      ((UNBOUND-SYMBOL-P concept) nil)

      ; otherwise, return the frame
      (t result)
    )
  )
)

; expand any filler in a list of slotfillers
(defun EXPAND-HELPER (slotFillers)
  (cond
    ((null slotFillers) nil)

    (t (append (list (first slotFillers) (EXPAND (second slotFillers))) (EXPAND-HELPER (nthcdr 2 slotFillers))))
  )
)

; find matching slot filler in a list of slotfillers
(defun AMEND-SF-HELPER (slot filler slotFillers)
  (cond
    ; add the given slotfiller to the frame if it does not exist
    ((null slotFillers) (list slot filler))

    ; replace the filler of the given slot with the given filler
    ((equal slot (first slotFillers)) (append (list slot filler) (nthcdr 2 slotFillers)))

    ; no matching slot found yet, check the next slotfiller
    (t (append (list (first slotFillers) (second slotFillers)) (AMEND-SF-HELPER slot filler (nthcdr 2 slotFillers))))
  )
)

; NOTE
; sfs = slotfillers
; sf = slotfiller

; if sf is in sfs, return sfs - sf (remove sf from sfs)
; otherwise return sfs
(defun COMPARE-SF-TO-MANY (sf sfs)
  (cond
    ((null sfs) nil)

    ; if we find matching sf, then return the unmatched sfs
    ((and (equal (first sf) (first sfs)) (EQUAL-SF-EXPANDED (second sf) (second sfs))) (nthcdr 2 sfs))

    ; current sf is not equal to the first sf in sfs, check the rest of sfs
    (t (append (list (first sfs) (second sfs)) (COMPARE-SF-TO-MANY sf (nthcdr 2 sfs))))
  )
)

; compare two sfs
(defun COMPARE-SF (sf1 sf2)
  (cond
    ; Base case: when both sf1 and sf2 are nil, we know they must be equal
    ((and (null sf1) (null sf2)) t)
    ((and (null sf1) (not (null sf2))) nil)
    ((and (null sf2) (not (null sf1))) nil)

    (t 
      (let* ((newSF2 (COMPARE-SF-TO-MANY (list (first sf1) (second sf1)) sf2)))
        (cond
          ; if the length of the traversed list of slotfillers does not change
          ; then there is no matching slotfiller in the list
          ; so we return nil
          ((= (length newSF2) (length sf2)) nil)

          ; otherwise, we continue to compare the rest of slotfillers in both sf1 and sf2
          (t (COMPARE-SF (nthcdr 2 sf1) newSF2))
        )
      )
    )
  )
)

; if predicates are equal, compare the slotfillers (sfs) 
(defun EQUAL-SF-EXPANDED (frame1 frame2)
  (cond
      ; if both frames are actually unbound symbols, compare them
      ((and (symbolp frame1) (symbolp frame2)) (equal frame1 frame2))

      ; else if only one of them is actually an unbound symbol, they cannot be equal
      ((and (symbolp frame1) (not (symbolp frame2))) nil)
      ((and (symbolp frame2) (not (symbolp frame1))) nil)

      ; otherwise, compare both frames
      ((equal (first frame1) (first frame2)) (COMPARE-SF (rest frame1) (rest frame2)))
      (t nil)
    )
)

; -----------------------------------------------------------------------------
; Main Functions
; -----------------------------------------------------------------------------

; FUNCTION: GET-SF
; PURPOSE:  Returns the filler of the given slot-name
; INPUTS:   slot: an atom designating the slot name
;           frame: a frame
; OUTPUT:   A FILLER (FRAME or GAP), according to slot in frame, or NIL if not
;           present
(defun GET-SF (slot frame)
    (cond
    	((null frame) nil)

      ((evenp (length frame)) nil)

    	((symbolp slot) (GET-SF-HELPER slot (rest frame)))

    	(t nil)
    )
)

; -----------------------------------------------------------------------------

; FUNCTION: GET-NESTED-SF
; PURPOSE:  Returns the filler at the end of the given slot-path, or nil if
;           the path does not exist in the given frame
; INPUTS:   slots: a path of slots
;           concept: a frame
; OUTPUT:   The requested filler, which will be a frame or gap
(defun GET-NESTED-SF (slots concept)
    (cond
    	; Base case: concept is nil, so return nil
    	((null concept) nil)

    	; slots is an empty list (nil), so return concept
    	((null slots) concept)

    	; otherwise, recursively call itself with the remaining slots
    	(t (GET-NESTED-SF (rest slots) (GET-SF (first slots) (GET-FRAME concept))))
    )
)

; -----------------------------------------------------------------------------

; FUNCTION: EXPAND
; PURPOSE:  Returns the frame represented by the given atom (if it is bound)
;           with all gaps replaced by their values
; INPUTS:   atom: a symbol (possibly) representing a frame
; OUTPUT:   That frame with all bound gaps replaced by their values
(defun EXPAND (atom)
    (cond
      ; Base case: return nil atom 
    	((null atom) nil)

      ; otherwise, expand the atom if it is bound
    	(t 
	    	(let* ((newAtom (GET-FRAME-OR-USYM atom)))
		    	(cond
            ; unbound symbol found, return it
			    	((UNBOUND-SYMBOL-P newAtom) newAtom)
			    	
			    	; otherwise, newAtom must be a frame at this point
			    	(t (cons (first newAtom) (EXPAND-HELPER (rest newAtom))))
			    )
		    )
	    )
    )	    
)

; -----------------------------------------------------------------------------

; FUNCTION: AMEND-SF
; PURPOSE:  Returns a copy of the input concept with the given slot-filler
;           pair added. If the slot already exists in the frame, its filler
;           will be replaced by the given input filler
; INPUTS:   slot: an atom representing the slot name
;           filler: a filler to place in the corresponding slot
;           frame: the frame being operated on
; OUTPUT:   Frame with added / replaced slot-filler pair
(defun AMEND-SF (slot filler frame)
    (cond
    	((null frame) nil)

    	(t 
    		(let* ((newFrame (GET-FRAME-OR-USYM frame)))
    			(cond
            ; return the unbound symbol
    				((UNBOUND-SYMBOL-P newFrame) newFrame)

            ; otherwise, check the slotfillers
    				(t (cons (first frame) (AMEND-SF-HELPER slot filler (rest frame))))
    			)
    		)
    	)
    )
)

; -----------------------------------------------------------------------------

; FUNCTION: EQUAL-SF
; PURPOSE:  Boolean predicate which compares two frames and returns tr
; INPUTS:   frame1: FRAME (first frame to compare)
;           frame2: FRAME (second frame to compare)
; OUTPUT:   T if frames have same slot-filler structure (order may vary),
;           NIL otherwise
(defun EQUAL-SF (frame1 frame2)
    ; expand both frames before making comparison to simplify it
    (EQUAL-SF-EXPANDED (EXPAND frame1) (EXPAND frame2))
)

; -----------------------------------------------------------------------------
