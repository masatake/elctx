;;; elctx.el --- show context of source code reading in margin area -*- lexical-binding: t -*-

;; Copyright (C) 2011, 2012, 2013 Masatake YAMATO

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


(defmacro define-elctx-provider (name doc indicator idoc &rest body)
  `(progn
    (defun ,name ()
       ,doc
       (when (memq ',indicator elctx-active-providers)
       ,@body))
    (put ',indicator 'elctx-provider-doc ,idoc)))


(defvar elctx-providers nil)
(defvar elctx-active-providers nil)
(defvar elctx-margin-length 72)
(defun elctx-make-cursor ()
  (let ((indicators (mapconcat #'identity
			       (mapcar #'(lambda (c)
					   (propertize
					    (symbol-name c)
					    'face 
					    (if (memq c elctx-active-providers)
						'mode-line
					      'mode-line-inactive
					      )
					    'help-echo (get c 'elctx-provider-doc)))
				       elctx-providers)
			       ""))
	(common-indicators
	 (concat (propertize "+" 'face 'widget
			     'help-echo "Enlarge area")
		 (propertize "-" 'face 'widget
			     'help-echo "Shrink area"))))

    (concat 
     indicators
     (propertize (make-string (- elctx-margin-length
				 (length indicators)
				 (length common-indicators)
				 ) ?\ )
		 'face
		 'mode-line-inactive
		 'help-echo "mode-line"
		 'mouse-face 'highlight
		 )
     common-indicators
     )))
(defun elctx-eldoc-function ()
  (elctx-clear-all)
  (if (one-window-p)
      (let ((back (reverse (funcall elctx-back-function)))
	    (front (funcall elctx-front-function)))
	(set-window-margins (selected-window) (car (window-margins)) elctx-margin-length)
	(let* ((s (symbol-at-point))
	       (l (line-number-at-pos))
	       (wsl (- l (line-number-at-pos (window-start))))
	       (wel (- (- (line-number-at-pos (window-end)) 2) l))
	       (backl (length back))
	       (frontl (length front)))
	  (save-excursion 
	    (cond 
	     ((and (< backl wsl) (< frontl wel))
	      t)
	     ((and (< wsl backl) (< wel frontl))
	      t)
	     ((< wsl backl)
	      (goto-char (point-min))
	      (forward-line (1- (+ (- backl wsl) l))))
	     ((< wel frontl)
	      (goto-char (point-min))
	      (forward-line (- l (- frontl wel)))
	      ))
	    (elctx-print-back back)
	    (elctx-print-1 (elctx-make-cursor)
			   (line-end-position)
			   )
	    (elctx-print-front front))))
	(set-window-margins (selected-window) nil))
    (car (funcall elctx-current-function)))

(defvar elctx-overlays nil)
(make-variable-buffer-local 'elctx-overlays)

(defun elctx-print-1 (str &optional p)
  (let ((ov (make-overlay (or p (point)) (or p (point)))))
    (overlay-put ov 'before-string
		 (propertize " " 
			     'display `((margin right-margin) ,str)
			     ))
    (push ov elctx-overlays)))

(defun elctx-print-back (back)
  (save-excursion
    (while (eq (forward-line -1) 0)
      (elctx-print-1 (car back) (line-end-position))
      (setq back (cdr back)))))

(defun elctx-print-front (front)
  (save-excursion
    (while (eq (forward-line 1) 0)
      (elctx-print-1 (car front) (line-end-position))
      (setq front (cdr front)))))

(defun elctx-clear-all ()
  (interactive)
  (mapc #'delete-overlay elctx-overlays)
  (setq elctx-overlays nil))

(defun elctx-enlarge-area (p)
  (interactive "p")
  (setq elctx-margin-length (1+ elctx-margin-length))
  (elctx-eldoc-function))

(defun elctx-shrink-area (p)
  (interactive "p")
  (setq elctx-margin-length (1- elctx-margin-length))
  (if (< elctx-margin-length 32)
      (setq elctx-margin-length 32))
  (elctx-eldoc-function)
  )

(defun elctx-activate-all (disable)
  (interactive "P")
  (if disable
      (setq elctx-active-providers nil)
    (setq elctx-active-providers (copy-list elctx-providers)))
  (elctx-eldoc-function))

;;(define-key global-map (kbd "<right-margin> <mouse-1>") (lambda (&rest rest)
;;							       (interactive "e")
;;							       (message "%s" rest)))
(defun elctx-install-provider-keys (sym)
  (let ((n (symbol-name sym)))
    (local-set-key  n
		   (lambda (exclusive) 
		     (interactive "P")
		     (if exclusive
			 (setq elctx-active-providers (list sym))
		       (if (memq sym elctx-active-providers)
			   (setq elctx-active-providers
				 (delete sym elctx-active-providers))
			 (setq elctx-active-providers
			       (cons sym elctx-active-providers))))
		     (elctx-eldoc-function)))))
(defun turn-on-elctx ()
  (setq elctx-providers (mapcar #'intern-soft (sort (mapcar #'symbol-name elctx-providers)
						    #'string<)))
  (setq elctx-active-providers (copy-list elctx-providers))
  (when buffer-read-only
    (local-set-key "+" 'elctx-enlarge-area)
    (local-set-key "-" 'elctx-shrink-area)
    (local-set-key "*" 'elctx-activate-all)
    (mapc #'elctx-install-provider-keys elctx-providers)
    )
  (set (make-local-variable 'eldoc-documentation-function)
       #'elctx-eldoc-function)
  (turn-on-eldoc-mode))

(provide 'elctx)
