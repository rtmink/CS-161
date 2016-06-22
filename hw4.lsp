; CS 161 Winter 2016: HW4 Solution

; [!] Include OUR HW3 solution here!
; [!] IMPORTANT: Remove the below line when you submit your code!
;     We'll include the hw-3-solution using *our* path, which will likely
;     differ from yours (Note: HW3 also includes HW2, which includes HW1)
;(load "../hw3/hw-3-solution.lsp")

; -----------------------------------------------------------------------------
; Helper functions
; -----------------------------------------------------------------------------

; Derive a slot name
(defun DERIVE-SL-NAME (sl-name e-pats q-con &optional sl-names)
	(let* ((filler (GET-NESTED-SF (append sl-names (list sl-name)) *CUR-C-ANS)))
		(cond
			; There is no filler
			((null filler) nil)

			(t
				(let* ((slots (if (> (length filler) 1) (append sl-names (list sl-name)) sl-names)))
					; IMPORTANT: don't forget to set "c-ans-set" to non-nil
					; so we don't update *CUR-C-ANS
					(FR-TO-ENG e-pats filler q-con t slots)
				)
			)
		)
	)
)

; Derive a decision tree
(defun DERIVE-D-TREE (d-tree e-pats q-con sl-names)
	(let* ((patterns (EVAL-D-TREE (eval d-tree))))
		(GLUE-PATS patterns e-pats q-con sl-names)
	)
)

; Derive a pattern according to its type
(defun DERIVE-PAT (pattern e-pats q-con sl-names)
	(let* ((keyword (first pattern)))
		(cond
			((equal keyword 'PHRASE) (rest pattern))

			((equal keyword 'SL-NAME) (DERIVE-SL-NAME (second pattern) e-pats q-con sl-names))

			((equal keyword 'D-TREE) (DERIVE-D-TREE (second pattern) e-pats q-con sl-names))

			(t nil)
		)
	)
)

; Glue patterns which make up the structure of the english sentence
(defun GLUE-PATS (patterns e-pats q-con sl-names)
	(cond
		((null patterns) nil)

		(t (append (DERIVE-PAT (first patterns) e-pats q-con sl-names) (GLUE-PATS (rest patterns) e-pats q-con sl-names)))
	)
)

; -----------------------------------------------------------------------------
; Utility Functions
; -----------------------------------------------------------------------------

; FUNCTION: FR-TO-ENG
; PURPOSE:  Translates a conceptual Q&A system answer into its English, human-
;           readable sentence equivalent.
; INPUT:    E-PATS: English patterns that take frame predicates and provide
;                   templates for how to translate the given frame
;           C-ANS: A conceptual frame answer to a question posed to our Q&A
;                  system. Assume this has been derived by a hypothetical
;                  inference system not shown.
;           Q-CON: A conceptual frame question created from a user-posed query.
; OUTPUT:   English sentence translation of C-ANS
(defun FR-TO-ENG (e-pats c-ans q-con &optional (c-ans-set nil) sl-names)
	(setq *CUR-Q-CON q-con)
	(if (null c-ans-set) (setq *CUR-C-ANS c-ans))
	(setq *CUR-FILLER c-ans)

	; Find the predicate of c-ans in e-pats
	(let* ((patterns (loop for lp in e-pats do (if (equal (first c-ans) (first lp)) (return (rest lp))))))
		(cond
			((null patterns) (list (first c-ans)))

			(t (GLUE-PATS patterns e-pats q-con sl-names))
		)
	)

	
)

; -----------------------------------------------------------------------------

; Self-explanatory
(defun EVAL-D-TREE-HELPER (path)
	(loop for v in (rest path) do (if (not (eval v)) (return t)))
)

; FUNCTION: EVAL-D-TREE
; PURPOSE:  Takes the given decision tree, and uses the frames in the global
;           variables to return a replacement pattern, if any.
; INPUT:    D-TREE: a decision tree (NOT a symbol name for a decision tree,
;                   but the tree itself)
; OUTPUT:   Replacement pattern if a decision tree path is satisfied, else NIL.
(defun EVAL-D-TREE (d-tree)
	(loop for v in d-tree do (if (not (EVAL-D-TREE-HELPER v)) (return (eval (first (last v))))))
)

