; CS 161 Winter 2016: HW3

; [!] Include OUR HW2 solution here!
; [!] IMPORTANT: Remove the below line when you submit your code!
;     We'll include the hw-2-solution using *our* path, which will likely
;     differ from yours (Note: HW2 also includes HW1)
;(load "../hw2/hw-2-solution.lsp")

; -----------------------------------------------------------------------------
; Helper functions
; -----------------------------------------------------------------------------

; Check if the argument is a variable
(defun IS-VAR (arg)
    (if (and (not (symbolp arg)) (eq (length arg) 2) (eq (first arg) 'V)) t)
)

; Self-explanatory
(defun GET-BINDING-VAL (b)
  (second b)
)

; -----------------------------------------------------------------------------
; Utility Functions
; -----------------------------------------------------------------------------

; Self-explanatory
(defun IS-FRAME (x)
  (if (and (not (symbolp x)) (symbolp (first x))) t)
)

; Self-explanatory
(defun IS-LIST-OF-FRAMES (x)
  (if (and (not (symbolp x)) (not (symbolp (first x)))) t)
)

; Get the binding value for a given variable
(defun GET-VAR-BINDING-VAL (var bds)
    (cond
      ; Base case: bds does not contain any variables
      ((< (length bds) 2) nil)

      ; Otherwise, there is at least one variable in bds
      (t (loop for b in (rest bds) do (if (eq (second (first b)) (second var)) (return (GET-BINDING-VAL b)))))
    )
)

; Similar to the one in the textbook
(defun UNIFY-VAR (var other bds)
  (let* ((var-val (GET-VAR-BINDING-VAL var bds)) 
         (other-val (GET-VAR-BINDING-VAL other bds)))
    (cond
      ; Binding value for var exists, unify it to other
      ((not (null var-val)) (UNIFY-FR var-val other bds))

      ; Binding value for other exists, unify var to it
      ((not (null other-val)) (UNIFY-FR var other-val bds))

      ; Otherwise, bind var to other and return the new bds
      (t (append bds (list (list var other))))
    )
  )
)

; Unify list of slot frames to a frame
(defun UNIFY-LSF-FR (lsf1 fr2 bds)
  (cond
    ((null lsf1) bds)

    (t 
      (let* ((filler (GET-SF (first lsf1) fr2)))
        (cond
          ((null filler) nil)

          (t (UNIFY-LSF-FR (nthcdr 2 lsf1) fr2 (UNIFY-FR (second lsf1) filler bds)))
        )
      )
    )
  )
)

; Unify the first frame to the second frame
(defun UNIFY-2-FR (fr1 fr2 bds)
  (cond
    ; Length of frame 1 must be <= length of frame 2
    ((> (length fr1) (length fr2)) nil)

    ; Unify the slots of frame 1 to frame 2
    (t (if (eq (first fr1) (first fr2)) (UNIFY-LSF-FR (rest fr1) fr2 bds)))
  )
)

; Unify the frame to a list of frames
(defun UNIFY-FR-LFR (fr lfr bds)
  (loop for frame in lfr do (if (not (null (UNIFY-2-FR fr frame bds))) 
                                (return (list (set-difference lfr (list frame)) (UNIFY-2-FR fr frame bds)))))
)

; Unify the first list of frames to the second list of frames
(defun UNIFY-2-LFR (lfr1 lfr2 bds)
  (cond
    ((null lfr1) bds)

    (t 
        (let* ((lfr2-bds (UNIFY-FR-LFR (first lfr1) lfr2 bds)) (new-lfr2 (first lfr2-bds)) (new-bds (second lfr2-bds)))
          (cond
            ((null lfr2-bds) nil)

            (t (UNIFY-2-LFR (rest lfr1) new-lfr2 new-bds))
          )
        )
    )
  )
)

; FUNCTION: UNIFY-FR
; PURPOSE:  Unifies the given variables, frames, or lists of frames by the
;           criteria listed in the spec
; INPUT:    LFR1: a variable, frame, or list of frames
;           LFR2: a variable, frame, or list of frames
;           BDS: [Optional; default: '(T)] A binding list being built during
;                execution
; OUTPUT:   Binding list
(defun UNIFY-FR (lfr1 lfr2 &optional (bds '(T)))
  (cond
    ((null bds) nil)

    ((equal lfr1 lfr2) bds)

    ((and (IS-VAR lfr1) (not (IS-LIST-OF-FRAMES lfr2))) (UNIFY-VAR lfr1 lfr2 bds))

    ((and (IS-VAR lfr2) (not (IS-LIST-OF-FRAMES lfr1))) (UNIFY-VAR lfr2 lfr1 bds))

    ((and (IS-FRAME lfr1) (IS-FRAME lfr2)) (UNIFY-2-FR lfr1 lfr2 bds))

    ((and (IS-LIST-OF-FRAMES lfr1) (IS-LIST-OF-FRAMES lfr2)) (UNIFY-2-LFR lfr1 lfr2 bds))

    (t nil)
  )
)

; -----------------------------------------------------------------------------

(defun SUBST-FR-HELPER (lsf bds)
  (cond
    ((null lsf) nil)

    (t (append (list (first lsf) (SUBST-FR (second lsf) bds)) (SUBST-FR-HELPER (nthcdr 2 lsf) bds)))
  )
)

; FUNCTION: SUBST-FR
; PURPOSE:  Substitutes the bindings in the given BDS into the corresponding
;           variables in the given frame
; INPUT:    FRM: a frame with variables
;           BDS: a binding list
; OUTPUT:   FRM with replacements made
(defun SUBST-FR (frm bds)
  (cond 
    ((or (null frm) (null bds)) frm)

    ((IS-VAR frm) (GET-VAR-BINDING-VAL frm bds))

    (t (cons (first frm) (SUBST-FR-HELPER (rest frm) bds)))
  )
)

; -----------------------------------------------------------------------------


; FUNCTION: MP-INFER
; PURPOSE:  Attempts to unify the given rule's premises on the given facts,
;           and if successful, returns the conclusion of the rule with SUBST-FR
;           called on it using the successful binding list
; INPUT:    RULE: an if-then rule
;           O-FRAMES: a list of facts / concepts
; OUTPUT:   conclusion if successfully unified; nil otherwise
(defun MP-INFER (rule o-frames)
  (let* ((bds (UNIFY-FR (rest (first rule)) o-frames)))
    (if (not (null bds)) (SUBST-FR (second (second rule)) bds))
  )
)

; -----------------------------------------------------------------------------

; Hack to handle NIL concept; return T in this case
; We can assume that nil is a member of the epmem implicitly
(defun IS-IN-EPMEM (concept epmem)
  (cond
    ((null concept) t)
    (t (member concept epmem :test #'EQUAL-SF))
  )
)

; FUNCTION: FRW-CHAIN
; PURPOSE:  Performs simplified forward chaining given the list of rules and
;           facts, returning any new conclusions that are derived
; INPUT:    RULES: a list of if-then rules
;           EPMEM: a list of facts storing an episodic memory
;           NEW-EPMEM: [Optional: default: nil] the list of newly discovered
;                      facts grown through code execution and returned at the
;                      end
; OUTPUT:   NEW-EPMEM
(defun FRW-CHAIN (rules epmem &optional (new-epmem nil))
  (let* ((new (loop for rule in rules append (if (not (IS-IN-EPMEM (MP-INFER rule epmem) epmem)) (list (MP-INFER rule epmem)) nil))))
    (cond
      ((null new) new-epmem)

      ; add new to epmem
      (t (FRW-CHAIN rules (append new epmem) (append new-epmem new)))
    )
  )
)

; -----------------------------------------------------------------------------


; -----------------------------------------------------------------------------
; Problem 4 Rule Definitions
; -----------------------------------------------------------------------------

(setq RULE-1 '((PREMISES
                 (TEACH AGENT (V x)
                        SITU (V z))
               )
               (CONCLU
                 (STATE AGENT (V x) 
                        TYPE (EMOTIONAL)
                        VALUE (HAPPY) 
                        SITU (V z))
               ))
      
      ; RULE-2 was done for you! Yay!
      RULE-2 '((PREMISES
                 (INFORM AGENT (V x)
                         RECIP (V y)
                         OBJECT (V z)
                         SITU (V sa))
                 
                 (AFTER ANTE (V sa)
                        CONSEQ (V sb))
               )
               (CONCLU
                 (KNOWS AGENT (V y)
                        OBJECT (V z)
                        SITU (V sb))
               ))
      
      RULE-3 '((PREMISES
                 (KNOWS AGENT (V x)
                        OBJECT (STATE AGENT (V x)
                                      OBJECT (CANCER))
                        SITU (V sa))
               )
               (CONCLU
                 (STATE TYPE (EMOTIONAL)
                        AGENT (V x)
                        VALUE (SAD) 
                        SITU (V sa))
               ))
      
      RULE-4 '((PREMISES
                 (MARRIED AGENT (V x)
                          OBJECT (V y)
                          SITU (V sa))

                 (STATE TYPE (PHYSICAL)
                        AGENT (V y) 
                        VALUE (PREGNANT)
                        SITU (V sb))
               )
               (CONCLU
                 (SEX-ACT AGENT (V x) 
                          OBJECT (V y)
                          SITU (V sa))
               ))
      
      RULE-5 '((PREMISES
                 (TEACH AGENT (V x)
                        OBJECT (CHEM)
                        SITU (V sa))

                 (STATE TYPE (EMOTIONAL)
                        AGENT (V x)
                        VALUE (SAD) 
                        SITU (V sb))

                 (AFTER ANTE (V sa)
                        CONSEQ (V sb))
               )
               (CONCLU
                 (MAKES AGENT (V x) 
                        OBJECT (COCAINE)
                        SITU (V sb))
               ))
      
      RULE-6 '((PREMISES
                 (INGEST AGENT (V x)
                         OBJECT (COCAINE) 
                         SITU (V sa))

                 (STATE AGENT (V x)
                        OBJECT (LESIONS AREA (NOSE)) 
                        SITU (V sb))

                 (AFTER ANTE (V sa)
                        CONSEQ (V sb))
               )
               (CONCLU
                 (CAUSE ANTE (INGEST AGENT (V x)
                                     OBJECT (COCAINE)
                                     SITU (V sa))
                        CONSEQ (STATE AGENT(V x) 
                                      OBJECT (LESIONS AREA (NOSE))
                                      SITU (V sb)))
               ))
      
      RULE-7 '((PREMISES
                 (MAKES AGENT (V x) 
                        OBJECT (COCAINE)
                        SITU (V sa))

                 (INGEST AGENT (V y)
                         OBJECT (COCAINE) 
                         SITU (V sb))

                 (AFTER ANTE (V sa)
                        CONSEQ (V sb))
               )
               (CONCLU
                 (ACQUIRED AGENT (V y) 
                           OBJECT (COCAINE)
                           FROM (V x)
                           SITU (V sb))
               ))
)

; -----------------------------------------------------------------------------