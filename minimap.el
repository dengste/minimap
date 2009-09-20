;;; minimap.el --- Minimap sidebar for Emacs

;; Copyright (C) 2009  David Engster

;; Author: David Engster <dengste@eml.cc>
;; Keywords:
;; Version: 0.3

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file is an implementation of a minimap sidebar, i.e., a
;; smaller display of the current buffer on the left side. It
;; highlights the currently shown region and updates its position
;; automatically. You can navigate in the minibar by dragging the
;; active region with the mouse, which will scroll the corresponding
;; edit buffer.

;; Usage:
;;  * Put minimap.el in your load path.
;;  * (require 'minimap)
;;  * Use 'M-x minimap-create' in a buffer you're currently editing.
;;  * Use 'M-x minimap-kill' to kill the minimap.
;;  * Use 'M-x customize-group RET minimap RET' to adapt minimap to your needs.

;;; KNOWN BUGS:

;; * Currently cannot deal with images.
;; * Display/movement can be a bit erratic at times.

;;; TODO:

;; * Fix known bugs.
;; * Make sidebar permanently visible. This requires something like a
;;   'window group' feature in Emacs, which is currently being worked on.
;; * Moving the active region with the keyboard / mouse-wheel ?


;;; Customizable variables:

(defgroup minimap nil
  "A minimap sidebar for Emacs."
  :group 'convenience)

(defface minimap-font-face
;;  '((default :family "Bitstream Vera Sans Mono" :height 40))
  '((default :family "Bitstream Vera Sans" :height 40))
  "Face used for text in minimap buffer, notably the font familiy and height.
This height should be really small. You probably want to use a
TrueType Font for this."
  :group 'minimap)

(defface minimap-active-region-background
  '((((background dark)) (:background "#4517305D0000"))
    (t (:background "#C847D8FEFFFF")))
  "Face for the active region in the minimap.
By default, this is only a different background color."
  :group 'minimap)

(defcustom minimap-width-fraction 0.2
  "Fraction of width which should be used for minimap sidebar."
  :type 'number
  :group 'minimap)

(defcustom minimap-buffer-name-prefix "*MINIMAP* "
  "Prefix for buffer names of minimap sidebar."
  :type 'string
  :group 'minimap)

(defcustom minimap-update-delay 0.5
  "Delay in seconds after which sidebar gets updated."
  :type 'number
  :group 'minimap)

(defcustom minimap-always-recenter t
  "Whether minimap sidebar should be recentered after every point movement."
  :type 'boolean
  :group 'minimap)

;;; Internal variables

(defvar minimap-start nil)
(defvar minimap-end nil)
(defvar minimap-active-overlay nil)
(defvar minimap-bufname nil)
(defvar minimap-timer-object nil)
(defvar minimap-active-minimaps 0)

(make-variable-buffer-local 'minimap-start)
(make-variable-buffer-local 'minimap-end)
(make-variable-buffer-local 'minimap-active-overlay)
(make-variable-buffer-local 'minimap-bufname)


;;; Minimap creation / killing

(defun minimap-create ()
  "Create a minimap sidebar for the current window."
  (interactive)
  ;; If minimap is visible, do nothing.
  (unless (and minimap-bufname
	       (get-buffer minimap-bufname)
	       (get-buffer-window (get-buffer minimap-bufname)))
    (let ((bufname (concat minimap-buffer-name-prefix
			   (buffer-name (current-buffer))))
	  (new-win (split-window-horizontally
		    (round (* (window-width)
			      minimap-width-fraction)))))
      ;; If minimap exists but isn't visible, reuse it.
      (if (and minimap-bufname
	       (get-buffer minimap-bufname))
	  (switch-to-buffer minimap-bufname t)
	;; Otherwise create new minimap
	(minimap-new-minimap bufname)
	;; If this is the first minimap, create the idle timer.
	(when (zerop minimap-active-minimaps)
	  (setq minimap-timer-object
		(run-with-idle-timer minimap-update-delay t 'minimap-update)))
	(setq minimap-active-minimaps
	      (1+ minimap-active-minimaps))))
    (other-window 1)))

(defun minimap-new-minimap (bufname)
  "Create new minimap BUFNAME for current buffer and window."
  (let ((indbuf (make-indirect-buffer (current-buffer) bufname t))
	ov)
    (setq minimap-bufname bufname)
    (switch-to-buffer indbuf)
    (setq ov (make-overlay (point-min) (point-max) nil t t))
    (overlay-put ov 'face 'minimap-font-face)
    (setq minimap-start (window-start)
	  minimap-end (window-end)
	  minimap-active-overlay (make-overlay minimap-start minimap-end)
	  line-spacing 0)
    (overlay-put minimap-active-overlay 'face
		 'minimap-active-region-background)
    (minimap-mode 1)
    (setq buffer-read-only t)))

(defun minimap-kill ()
  "Kill minimap for current buffer.
Cancel the idle timer if no more minimaps are active."
  (interactive)
  (if (null minimap-bufname)
      (message "No minimap associated with %s." (buffer-name (current-buffer)))
    (let ((curname (buffer-name (current-buffer)))
	  (buf (get-buffer minimap-bufname))
	  (win (get-buffer-window minimap-bufname)))
      (setq minimap-bufname nil)
      (if (null buf)
	  (message "No minimap associated with %s." curname)
	(when win
	  (delete-window win))
	(kill-buffer buf)
	(when (zerop
	       (setq minimap-active-minimaps
		     (1- minimap-active-minimaps)))
	  (cancel-timer minimap-timer-object)
	  (setq minimap-timer-object nil))
	(message "Minimap for %s killed." curname)))))

;;; Minimap update

(defun minimap-update ()
  "Update minimap sidebar.
This is meant to be called from the idle-timer or the post command hook."
  (when minimap-bufname
    (let ((win (get-buffer-window minimap-bufname))
	  start end pt ov)
      (when win
	(setq start (window-start)
	      end (window-end)
	      pt (point)
	      ov)
	(with-selected-window win
	  (unless (and (= minimap-start start)
		       (= minimap-end end))
	    (move-overlay minimap-active-overlay start end)
	    (setq minimap-start start
		  minimap-end end))
	  (goto-char pt)
	  (when minimap-always-recenter
	    (recenter (round (/ (window-height) 2)))))))))

;;; Overlay movement

(defun minimap-move-overlay-mouse (start-event)
  "Move overlay by tracking mouse movement."
  (interactive "e")
  (mouse-set-point start-event)
  (when (get-buffer-window (buffer-base-buffer (current-buffer)))
    (let* ((echo-keystrokes 0)
	   (end-posn (event-end start-event))
	   (start-point (posn-point end-posn))
	   (make-cursor-line-fully-visible nil)
           pt ev)
      (move-overlay minimap-active-overlay start-point minimap-end)
      (track-mouse
	(minimap-set-overlay start-point)
	(while (and
		(consp (setq ev (read-event)))
		(eq (car ev) 'mouse-movement))
	  (setq pt (posn-point (event-start ev)))
	  (when (numberp pt)
	    (minimap-set-overlay pt))))
      (select-window (get-buffer-window (buffer-base-buffer)))
      (minimap-update))))
  
(defun minimap-set-overlay (pt)
  "Set overlay position, with PT being the middle."
  (goto-char pt)
  (let* ((ovstartline (line-number-at-pos minimap-start))
	 (ovendline (line-number-at-pos minimap-end))
	 (ovheight (round (/ (- ovendline ovstartline) 2)))
	 (line (line-number-at-pos))
	 (winstart (window-start))
	 (winend (window-end))
	 newstart newend)
    (setq pt (point-at-bol))
    (setq newstart (minimap-line-to-pos (- line ovheight)))
    (while (< newstart winstart)
      (scroll-down 5)
      (redisplay t)
      (setq winstart (window-start)))
    (with-selected-window (get-buffer-window (buffer-base-buffer))
      (set-window-start nil newstart)
      (setq newend (window-end)))
    (while (> newend winend)
      (scroll-up 5)
      (redisplay t)
      (setq winend (window-end)))
    (move-overlay minimap-active-overlay newstart newend)))

(defun minimap-line-to-pos (line)
  "Returns point position of line number LINE."
  (save-excursion
    (goto-char 1)
    (if (eq selective-display t)
	(re-search-forward "[\n\C-m]" nil 'end (1- line))
      (forward-line (1- line)))
    (point)))

;;; Minimap minor mode

(defvar minimap-mode-map (make-sparse-keymap)
  "Keymap used by `minimap-mode'.")

(define-key minimap-mode-map [down-mouse-1] 'minimap-move-overlay-mouse)

(define-minor-mode minimap-mode
  "Minor mode for minimap sidebar."
  nil "minimap" minimap-mode-map)

(provide 'minimap)

;;; minimap.el ends here
