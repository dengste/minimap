;;; minimap.el --- Minimap sidebar for Emacs

;; Copyright (C) 2009  David Engster

;; Author: David Engster <dengste@eml.cc>
;; Keywords:
;; Version: 0.4

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
  '((default :family "DejaVu Sans Mono" :height 30))
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

(defcustom minimap-always-recenter nil
  "Whether minimap sidebar should be recentered after every point movement."
  :type 'boolean
  :group 'minimap)

(defcustom minimap-hide-scroll-bar t
  "Whether the minimap should hide the vertical scrollbar."
  :type 'boolean
  :group 'minimap)

(defcustom minimap-hide-fringes t
  "Whether the minimap should hide the fringes."
  :type 'boolean
  :group 'minimap)

(defcustom minimap-dedicated-window nil
  "Whether the minimap should create a dedicated window."
  :type 'boolean
  :group 'minimap)

(defcustom minimap-sync-overlay-properties '(face invisible)
  "Specifies which overlay properties should be synced.
Unlike text properties, overlays are not applied automatically to
the minimap and must be explicitly synced.  This variable
specifies which overlay properties should be synced by
`minimap-sync-overlays'.  Most importantly, this variable should
include 'invisible', so that hidden text does not appear in the
minimap buffer."
  :type '(repeat symbol)
  :group 'minimap)

;;; Internal variables

(defvar minimap-start nil)
(defvar minimap-end nil)
(defvar minimap-active-overlay nil)
(defvar minimap-bufname nil)
(defvar minimap-timer-object nil)
(defvar minimap-active-minimaps 0)
(defvar minimap-base-overlay nil)

(make-variable-buffer-local 'minimap-start)
(make-variable-buffer-local 'minimap-end)
(make-variable-buffer-local 'minimap-active-overlay)
(make-variable-buffer-local 'minimap-bufname)
(make-variable-buffer-local 'minimap-base-overlay)

;;; Minimap creation / killing

;;;###autoload
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
    (other-window 1)
    (minimap-sync-overlays)))

(defun minimap-new-minimap (bufname)
  "Create new minimap BUFNAME for current buffer and window."
  (let ((indbuf (make-indirect-buffer (current-buffer) bufname t)))
    (setq minimap-bufname bufname)
    (set-buffer indbuf)
    (when minimap-hide-scroll-bar
      (setq vertical-scroll-bar nil))
    (switch-to-buffer indbuf)
    (setq minimap-base-overlay (make-overlay (point-min) (point-max) nil t t))
    (overlay-put minimap-base-overlay 'face 'minimap-font-face)
    (setq minimap-start (window-start)
	  minimap-end (window-end)
	  minimap-active-overlay (make-overlay minimap-start minimap-end)
	  line-spacing 0)
    (overlay-put minimap-active-overlay 'face
		 'minimap-active-region-background)
    (minimap-mode 1)
    (when (and (boundp 'linum-mode)
	       linum-mode)
      (linum-mode 0))
    (when minimap-hide-fringes
      (set-window-fringes nil 0 0))
    (when minimap-dedicated-window
      (set-window-dedicated-p nil t))
    (setq buffer-read-only t)))

;;;###autoload
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

(defun minimap-update (&optional force)
  "Update minimap sidebar if necessary.
This is meant to be called from the idle-timer or the post command hook.
When FORCE, enforce update of the active region."
  (when minimap-bufname
    (let ((win (get-buffer-window minimap-bufname))
	  start end pt ov)
      (when win
	(setq start (window-start)
	      end (window-end)
	      pt (point)
	      ov)
	(with-selected-window win
	  (unless (and (not force)
		       (= minimap-start start)
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
	   (cursor-type nil)
	   (pcselmode pc-selection-mode)
           pt ev)
      (when pcselmode
	(pc-selection-mode -1))
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
      (minimap-update)
      (when pcselmode
	(pc-selection-mode 1)))))

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

;;; Sync minimap with modes which create/delete overlays.

(defun minimap-sync-overlays ()
  "Synchronize overlays between base and minimap buffer."
  (interactive)
  (when minimap-bufname
    (let ((baseov (overlays-in (point-min) (point-max)))
	  ov props p)
      (with-current-buffer minimap-bufname
	(remove-overlays)
	(while baseov
	  (when (setq props (minimap-get-sync-properties (car baseov)))
	    (setq ov (make-overlay (overlay-start (car baseov))
				   (overlay-end (car baseov))))
	    (while (setq p (car props))
	      (overlay-put ov (car p) (cadr p))
	      (setq props (cdr props))))
	  (setq baseov (cdr baseov)))
	;; Re-apply font overlay
	(move-overlay minimap-base-overlay (point-min) (point-max))))
    (minimap-update t)))

(defun minimap-get-sync-properties (ov)
  "Get properties from overlay OV which should be synced.
You can specify those properties with
`minimap-sync-overlay-properties'."
  (delq nil
	(mapcar
	 (lambda (p)
	   (let ((val (overlay-get ov p)))
	     (if val
		 (list p val)
	       nil)))
	 minimap-sync-overlay-properties)))

;; outline-(minor-)mode
(add-hook 'outline-view-change-hook 'minimap-sync-overlays)

;; hideshow
(add-hook 'hs-hide-hook 'minimap-sync-overlays)
(add-hook 'hs-show-hook 'minimap-sync-overlays)

(provide 'minimap)

;;; minimap.el ends here
