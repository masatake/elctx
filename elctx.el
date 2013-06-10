;;; elctx.el --- show context of source code reading in margin area

;; Copyright (C) 2011, 2012, 20313 Masatake YAMATO

;; This software is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with This software.  If not, see <http://www.gnu.org/licenses/>.

;;
;; condition
;;
(defun c-expand-conditional-context (context)
  (if context
      (let* ((d 4)
	     (indent (* d -1)))
	(mapcar 
	 (lambda (c)
	   (cons 
	    (format "%s"
		   (let ((elt (car c)))
		     (cond
		      ((string-match "#[ \t]*if" elt)
		       (setq indent (+ indent d))
		       (concat (make-string indent ?\ ) elt))
		      ((string-match "#[ \t]*elif" elt)
		       (concat (make-string indent ?\ ) elt))
		      ((string-match "#[ \t]*else" elt)
		       (concat (make-string indent ?\ ) elt))
		      (t
		       (setq indent (+ indent d))
		       (concat (make-string indent ?\ ) elt)))))
	    (cdr c)))
	 context
	 ))
    nil))

(defun c-climb-conditinal-context ()
  (c-up-conditional-with-else 1))
(defun c-adjust-conditinal-context ()
  (line-beginning-position))
(defun c-extract-conditinal-context (pos)
  (buffer-substring pos (line-end-position)))
(defun c-build-conditional-context ()
  (c-eldoc-build-context #'c-climb-conditinal-context
			 #'c-adjust-conditinal-context
			 #'c-extract-conditinal-context
			 'condition))

;;
;; control
;;
(defun c-build-control-subcontext (context)
  (let ((elt (car context))
	(pos (cadr (memq :point context))))
    (if (string-match "\\(.*\\)\\<else\\>" elt)
	(save-excursion
	  (goto-char (+ pos (length (match-string 1 elt))))
	  (backward-sexp 1)
	  (let* ((p (c-adjust-control-context))
		 (str (c-extract-control-context p))
		 (l (line-number-at-pos)))
	    (c-build-control-subcontext (list str :point p :line l))))
      context)))
	    
(defun c-expand-control-context (context)
  (if context
      (mapcar 
       (lambda (c)
	 (cons (let ((elt (car c)))
		 (cond
		  ((string-match "\\<else\\>" elt)
		   (let ((subcontext (c-build-control-subcontext c)))
		     (concat (car subcontext) "\n" elt)
		     ))
		  (t
		   elt)))
	       (cdr c)))
       context)
    nil))

(defun c-climb-control-context ()
  (backward-up-list 1))
(defun c-adjust-control-context ()
  (backward-sexp 1)
  (line-beginning-position))
(defun c-extract-control-context (pos)
  (car (split-string 
	(buffer-substring pos (line-end-position))
	"\n")))

(defun c-build-control-context ()
  (let ((paren-base (c-eldoc-build-context #'c-climb-control-context
					   #'c-adjust-control-context
					   #'c-extract-control-context
					   'control
					   ))
	(font-lock-base (let ((b (save-excursion
				   (let ((p  (point))
					 (b  (progn (beginning-of-defun) (point)))
					 (e  (progn (end-of-defun) (point))))
				     (if (and (< b p)
					      (< p e))
					 b
				       nil)))))
			  (save-excursion
			    (end-of-line)
			    (let ((context ()))
			      (while (not (eq b (point)))
				(goto-char (previous-single-property-change (point)
									    'face
									    nil
									    b))
				(when (and (eq 'font-lock-keyword-face 
					       (get-text-property (point) 'face))
					   (not (memq (symbol-at-point)
						      '(sizeof
							const
							struct
							union)))
					   )
				  (let ((str (buffer-substring
					      (line-beginning-position)
					      (line-end-position)))
					(symbol (symbol-at-point))
					(p (point))
					(l (line-number-at-pos)))
				    (setq context (cons 
						   (list str :point p :line l :symbol symbol)
						   context)))))
			      context)))))
    (let ((context (sort (append paren-base font-lock-base)
			 (lambda (a b)
			   (< (cadr (memq :line a))
			      (cadr (memq :line b)))))))
      (delete-duplicates context :test (lambda (a b)
					 (eq (cadr (memq :line a))
					    (cadr (memq :line b))))))))

;;
;; Variables
;;
(defun c-expand-variable-context (context)
  context)
(defun c-build-variable-context ()
  (let ((b (save-excursion
	     (let ((p  (point))
		   (b  (progn (beginning-of-defun) (point)))
		   (e  (progn (end-of-defun) (point))))
	       (if (and (< b p)
			(< p e))
		   b
		 nil)))))
    (save-excursion
      (let ((context ()))
	(while (not (eq b (point)))
	  (goto-char (previous-single-property-change (point)
						    'face
						    nil
						    b))
	  (when (eq 'font-lock-variable-name-face 
		    (get-text-property (point) 'face))
	    (let ((str (buffer-substring
			(line-beginning-position)
			(line-end-position)))
		  (symbol (symbol-at-point))
		  (p (point))
		  (l (line-number-at-pos)))
	      (setq context (cons 
			       (list str :point p :line l :symbol symbol :engine 'variable)
			       context)))))
	context))))

;;
;; Keyword
;;
(defun c-build-face-context-check ()
  (let ((f (get-text-property (point) 'face))
	(s (or (symbol-at-point)
	       (save-excursion (goto-char (1+ (point)))
			       (symbol-at-point)))))
    (cond 
     ((and (eq f 'font-lock-preprocessor-face)
	   (not (memq s '(include))))
      t)
     ((and (memq f '(font-lock-keyword-face
		     font-lock-constant-face
		     font-lock-function-name-face
		     font-lock-variable-name-face))
	   (not (memq s '(sizeof
			  const
			  struct
			  union
			  asm
			  NULL
			  ))))
      s)
     (t
      nil))))

(defun c-build-backward-face-context ()
  (let ((b (save-excursion
	     (let ((p  (point))
		   (b  (progn (beginning-of-defun) (point)))
		   (e  (progn (end-of-defun) (point))))
	       (if (and (< b p)
			(< p e))
		   b
		 nil)))))
    (save-excursion
      (end-of-line)
      (let ((context ()))
	(while (not (eq b (point)))
	  (goto-char (previous-single-property-change (point)
						      'face
						      nil
						      b))
	  (when (c-build-face-context-check)
	    (let* ((symbol (symbol-at-point))
		   (str (replace-regexp-in-string  
			 "\n\\|[^ \t]+\t\\| +" " "
			 (buffer-substring
			  (save-excursion (line-beginning-position))
			  (if (memq symbol '(if while do switch for))
			      (let ((sp (save-excursion (forward-sexp 2) (point)))
				    (lp (line-end-position)))
				(if (> sp lp) sp lp))
			    (line-end-position)))))
		   (p (point))
		   (l (line-number-at-pos)))
	      (setq context (cons 
			     (list str :point p :line l :symbol symbol)
			     context)))))
	context))))

(defun c-build-forward-face-context ()
  (let ((e (save-excursion
	     (let ((p  (point))
		   (b  (progn (beginning-of-defun) (point)))
		   (e  (progn (end-of-defun) (point))))
	       (if (and (< b p)
			(< p e))
		   e
		 nil)))))
    (save-excursion
      (end-of-line)
      (let ((context ()))
	(while (not (eq e (point)))
	  (goto-char (next-single-property-change (point)
						  'face
						  nil
						  e))
	  (when (c-build-face-context-check)
	      (let ((str (buffer-substring
			  (line-beginning-position)
			  (line-end-position)))
		    (symbol (symbol-at-point))
		    (p (point))
		    (l (line-number-at-pos)))
		(setq context (cons 
			       (list str :point p :line l :symbol symbol)
			       context)))))
	context))))
;;
;; Common
;;
(defun c-eldoc-build-context (climb adjust extract engine)
  (save-excursion
    (let ((context ()))
      (condition-case nil
	  (while t
	    (funcall climb)
	    ;; push
	    (setq context 
		  (cons (let ((p (funcall adjust)))
			  (list (funcall extract p)
				:point p
				:line (line-number-at-pos)
				:engine engine
				))
			context)))
	(error nil))
      context)))
  
(defconst c-context-eldoc-margin-length 72)
(defun c-context-eldoc-render (l)
  (split-string 
   (replace-regexp-in-string 
    "\t" "      " 
    (mapconcat 'car (sort 
		     (delete-duplicates l
					:test (lambda (a b)
						(eq (cadr (memq :line a))
						    (cadr (memq :line b)))))
		     (lambda (a b)
		       (< (cadr (memq :line a))
			  (cadr (memq :line b)))))
	       "\n")
    ) "\n"))

(defface cc-eldoc-current-line
  '((t (:strike-through "black" :weight bold)))
  "Face used to highlight the annotated region."
  ;:group 'stitch
  )

(defun c-context-eldoc-function ()
  (eldoc-clear-right-margin-all)
  (if (one-window-p)
      (let* ((s (symbol-at-point))
	     (l (line-number-at-pos))
	     (wsl (- l (line-number-at-pos (window-start))))
	     (wel (- (- (line-number-at-pos (window-end)) 2) l))
	     (back (nreverse (c-context-eldoc-render (append 
						      ;; occur with slice
						      (c-expand-conditional-context (c-build-conditional-context))
						      (c-expand-control-context (c-build-control-context))
						      (c-build-backward-face-context)
						      ))))
	     (backl (length back))
	     (front (c-context-eldoc-render (append 
					     (c-build-forward-face-context)
					     )))
	     (frontl (length front)))
	;(set-window-fringes (selected-window) (car (window-fringes)) 10 nil)
	(set-window-margins (selected-window) (car (window-margins)) c-context-eldoc-margin-length)
	(save-excursion 
	  (cond 
	   ((and (< backl wsl) (< frontl wel))
	    t)
	   ((and (< wsl backl) (< wel frontl))
	    t)
	   ((< wsl backl)
	    (goto-line (+ (- backl wsl) l)))
	   ((< wel frontl)
	    (goto-line (- l (- frontl wel)))))
	  (eldoc-print-right-margin-back back)
	  (eldoc-print-right-margin (propertize (make-string c-context-eldoc-margin-length ?\ )
						'face ;'cc-eldoc-current-line
						'mode-line-inactive
						'help-echo "mode-line"
						'mouse-face 'highlight
						)
				    (line-end-position)
				    )
	  (eldoc-print-right-margin-front front)))
    (set-window-fringes (selected-window) 8 8 nil)
    (set-window-margins (selected-window) nil))
  (let* ((s (symbol-at-point))
	 (vc (c-build-variable-context))
	 (m (find s vc :test (lambda (S VC)
			       (eq s (cadr (memq :symbol VC)))))))
    (when m
	(car m))))

(add-hook 'c-mode-hook
 	  (lambda ()
 	    (set (make-local-variable 'eldoc-documentation-function)
		 #'c-context-eldoc-function)
 	    (turn-on-eldoc-mode)))


(defvar eldoc-right-margin-overlays nil)
(make-variable-buffer-local 'eldoc-right-margin-overlays)

(defun eldoc-print-right-margin (str &optional p)
  (let ((ov (make-overlay (or p (point)) (or p (point)))))
    (overlay-put ov 'before-string
		 (propertize " " 
			     'display `((margin right-margin) ,str)
			     ))
    (push ov eldoc-right-margin-overlays)))

(defun eldoc-print-right-margin-back (back)
  (save-excursion
    (while (eq (forward-line -1) 0)
      (eldoc-print-right-margin (car back)
				(line-end-position)
				)
      (setq back (cdr back)))))

(defun eldoc-print-right-margin-front (front)
  (save-excursion
    (while (eq (forward-line 1) 0)
      (eldoc-print-right-margin (car front)
				(line-end-position)
				)
      (setq front (cdr front)))))

(defun eldoc-clear-right-margin-all ()
  (interactive)
  (mapc #'delete-overlay eldoc-right-margin-overlays)
  (setq eldoc-right-margin-overlays nil))

;;(set-window-fringes (selected-window) (car (window-fringes)) 2 nil)


;;(define-key global-map (kbd "<right-margin> <mouse-1>") (lambda (&rest rest)
;;							       (interactive "e")
;;							       (message "%s" rest)))
(provide 'elctx)
