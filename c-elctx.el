;;; c-elctx.el --- c-mode adapter for elctx

;; Copyright (C) 2013 Masatake YAMATO

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

(require 'elctx)
(require 'cl)

(defun c-elctx-wash-string (string)
  (let* ((string (replace-regexp-in-string "\n" " " string))
	 (prefix+rest (when (string-match "\\(^[ \t]+\\)\\(.*\\)" string)
			(cons (match-string 1 string)
			      (match-string 2 string)))))
    (if prefix+rest
	(concat (car prefix+rest) (replace-regexp-in-string "[\t ]+" " "
							    (cdr prefix+rest)))
      string)))

(defun c-elctx-backward-face-context (checker engine)
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
	  (when (funcall checker)
	    (let* ((symbol (symbol-at-point))
		   (str (c-elctx-wash-string
			 (buffer-substring
			  (save-excursion (line-beginning-position))
			  (if (memq symbol '(if while switch for))
			      (let ((sp (save-excursion (forward-sexp 2) (point)))
				    (lp (line-end-position)))
				(if (> sp lp) sp lp))
			    (line-end-position)))))
		   (p (point))
		   (l (line-number-at-pos)))
	      (setq context (cons 
			     (list str :point p :line l :symbol symbol :engine engine)
			     context)))))
	context))))

(defun c-elctx-forward-face-context (checker engine)
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
	  (when (funcall checker)
	      (let ((str (buffer-substring
			  (line-beginning-position)
			  (line-end-position)))
		    (symbol (symbol-at-point))
		    (p (point))
		    (l (line-number-at-pos)))
		(setq context (cons 
			       (list str :point p :line l :symbol symbol :engine engine)
			       context)))))
	context))))

;;
;; CONDITION
;;
(defun c-elctx-conditional-context-expand (context)
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
(defun c-elctx-conditional-context-build ()
  (c-eldoc-build-context #'c-climb-conditinal-context
			 #'c-adjust-conditinal-context
			 #'c-extract-conditinal-context
			 'condition))
(defun c-elctx-conditional-context-check ()
  (let ((f (get-text-property (point) 'face))
	(s (or (symbol-at-point)
	       (save-excursion (goto-char (1+ (point)))
			       (symbol-at-point)))))
    (cond 
     ((and (eq f 'font-lock-preprocessor-face)
	   (not (memq s '(include define))))
      t)
     (t
      nil))))

(define-elctx-provider c-elctx-conditional-context-provider "" 
  p "preprocessor #ifdef/#else/#endif"
  (append
   (c-elctx-conditional-context-expand (c-elctx-conditional-context-build))
   (c-elctx-backward-face-context #'c-elctx-conditional-context-check 'condition)
   (c-elctx-forward-face-context #'c-elctx-conditional-context-check 'condition)
   ))

;;
;; VARIABLES
;;
(defun c-expand-variable-context (context)
  context)
(defun c-elctx-variable-context-check ()
  (let ((f (get-text-property (point) 'face)))
    (memq f '(font-lock-function-name-face
	      font-lock-variable-name-face ))))

(defun c-build-variable-context ()
  (c-elctx-backward-face-context #'c-elctx-variable-context-check
				 'variable))

(define-elctx-provider c-elctx-variable-context-provider "" 
  l "local variables"
  (c-build-variable-context))


;;
;; CONTROL
;;
(defun c-elctx-control-context-check ()
  (let ((f (get-text-property (point) 'face))
	(s (or (symbol-at-point)
	       (save-excursion (goto-char (1+ (point)))
			       (symbol-at-point)))))
    (cond 
     ((and (memq f '(font-lock-keyword-face
		     font-lock-constant-face
		     font-lock-function-name-face
		     ))
	   (not (memq s '(sizeof
			  const
			  struct
			  register
			  union
			  asm
			  NULL
			  ))))
      s)
     (t
      nil))))

(define-elctx-provider c-elctx-control-context-provider "" 
  c "control FUNC/if/while/for/do/switch/case/goto"
  (append 
   (c-elctx-backward-face-context #'c-elctx-control-context-check
				 'control)
   (c-elctx-forward-face-context #'c-elctx-control-context-check
				 'control)))


;;
;; SLICE
;;
(defun c-elctx-slice-context-build ()
  (let ((sym (symbol-at-point)))
    (when sym
      (unless (memq sym '(while do if for goto case switch default
				continue break))
	(save-excursion
	  (let ((e (save-excursion (end-of-defun) (point)))
		(r (concat "\\<" (symbol-name sym) "\\>"))
		(result (list)))
	    (beginning-of-defun)
	    (while (re-search-forward r e t)
	      (setq result (cons (list 
				  (buffer-substring
				   (line-beginning-position)
				   (line-end-position))
				  :point (point)
				  :line (line-number-at-pos)
				  :symbol sym
				  :engine 'slice) result))
	      (end-of-line)
	      )
	    result
	    ))))))
(define-elctx-provider c-elctx-slice-context-provider "" 
  s "pseudo variable slice"
  (c-elctx-slice-context-build))

;;
;; LINUX
;;
(define-elctx-provider c-elctx-LINUX-context-provider "" 
  L "something interesting in LINUX kernel code"
  (c-elctx-LINUX-context-build))
(defvar c-elctx-LINUX-context-regexp
  (concat "^\\(?:__setup\\|module_init\\|EXPORT_SYMBOL\\)\\>\\|\\<\\(?:"
	  (mapconcat #'identity
		     '("notifier_block"
		       "kthread_run"
		       "kthread_create")
		     "\\|")
	  "\\)\\>"))

(defun c-elctx-LINUX-context-build ()
  (save-excursion
    (let ((b (point-min))
	  (e (point-max))
	  (r c-elctx-LINUX-context-regexp)
	  (result (list)))
      (goto-char b)
      (while (re-search-forward r e t)
	(setq result (cons (list 
			    (buffer-substring
			     (line-beginning-position)
			     (line-end-position))
			    :point (point)
			    :line (line-number-at-pos)
			    :symbol (symbol-at-point)
			    :uniq t
			    :engine 'LINUX) result))
	(end-of-line)
	)
      result
      )))

;;
;; Registers
;;
(define-elctx-provider c-elctx-register-context-provider "" 
  r "the positions of registers"
  (c-elctx-register-context-build))
(defun c-elctx-register-context-build ()
  (save-excursion
    (let ((buf (current-buffer))
	  (e (save-excursion (end-of-defun) (point)))
	  (b  (save-excursion (beginning-of-defun) (point)))
	  (result ()))
      (mapc (lambda (r)
	      (when (and (equal (marker-buffer (cdr r)) buf)
			 (< (marker-position (cdr r)) e)
			 (< b (marker-position (cdr r))))
		(setq result
		      (cons
		       (list (format "#<%c>" (car r))
			     :point (marker-position (cdr r))
			     :line (line-number-at-pos (cdr r))
			     :engine 'register)
		       result))))
       register-alist)
      result)))

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

(defconst c-elctx-tab-lenngth 4)
(defun c-elctx-render-decorate (l0)
  (let ((str (car l0))
	(line (cadr (memq :line l0))))
    (if line
	(propertize (copy-sequence str)
		    'help-echo (format "line: %d" line)
		    'keymap (let ((map (make-sparse-keymap)))
			      (define-key map [right-margin mouse-1] `(lambda ()
								       (interactive)
								       (goto-line ,line)))
			      map))
      str)))
(defun c-elctx-render (l pos)
   (let* ((cur (line-number-at-pos))
		(l0 (delete-if
		     (lambda (elt)
		       (cond
			((eq pos 'back)
			 (<= cur (cadr (memq :line elt))))
			((eq pos 'front)
			 (> cur (cadr (memq :line elt))))))
		     (sort 
		      (delete-duplicates l
					 :test (lambda (a b)
						 (and (eq (cadr (memq :line a))
							  (cadr (memq :line b)))
						      (not (cadr (memq :uniq a)))
						      (not (cadr (memq :uniq b)))
						      )
						 ))
		      (lambda (a b)
			(< (cadr (memq :line a))
			   (cadr (memq :line b))))))))
	   (split-string 
	    (replace-regexp-in-string 
	     "\t" (make-string c-elctx-tab-lenngth ?\ )
	     (mapconcat #'c-elctx-render-decorate l0
			"\n")
	     ) "\n")))

(defvar c-elctx-providers '(p c l s L r))

(defvar c-elctx-cache nil)
(defun c-elctx-back-function ()
  (setq c-elctx-cache (append 
		       (c-elctx-conditional-context-provider)
		       (c-elctx-variable-context-provider)
		       (c-elctx-control-context-provider)
		       (c-elctx-slice-context-provider)
		       (c-elctx-LINUX-context-provider)
		       (c-elctx-register-context-build)
		       ))
  (c-elctx-render (copy-tree c-elctx-cache) 'back))

(defun c-elctx-front-function ()
  (prog1
      (c-elctx-render c-elctx-cache 'front)
    (setq c-elctx-cache nil)))


(defun c-elctx-current-function ()
  (let* ((s (symbol-at-point))
	 (vc (c-build-variable-context))
	 (m (find s vc :test (lambda (S VC)
			       (eq s (cadr (memq :symbol VC)))))))))


(defun turn-on-c-elctx ()
  (set (make-local-variable 'elctx-back-function)
       #'c-elctx-back-function)
  (set (make-local-variable 'elctx-front-function)
       #'c-elctx-front-function)
  (set (make-local-variable 'elctx-current-function)
       #'c-elctx-current-function)
  (set (make-local-variable 'elctx-providers) c-elctx-providers)

  (turn-on-elctx))

(add-hook 'c-mode-hook #'turn-on-c-elctx)

(provide 'c-elctx)
