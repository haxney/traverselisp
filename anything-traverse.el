;;; anything-traverse.el --- Use traverselisp within anything.

;; Copyright (C) 2008, 2009 Thierry Volpiatto, all rights reserved

;; Author: thierry volpiatto
;; Maintainer: thierry volpiatto

;; Created: lun jan 12 11:23:02 2009 (+0100)
;; Last-Updated: sam fév 21 15:21:58 2009 (+0100)
;;           By: thierry
;;     Update #: 40

;; X-URL: http://freehg.org/u/thiedlecques/traverselisp/
;; Keywords: data, regexp

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;  ==========
;;  This is the source and functions to use traverselisp.el
;;  with anything. http://www.emacswiki.org/cgi-bin/wiki/Anything.
;;
;;  You will be able to incremental search any regexp in current buffer
;;  or in all files of current dired buffer.
;;
;;  NOTE: You don't need this file to use traverselisp.el if you don't use
;;  Anything.

;;; Install:
;;  =======
;; You have first to install anything and traverselisp.
;; Then put this file somewhere in your load-path and:
;; (require 'anything-traverse)

;;; Usage:
;;  =====
;; If you add `anything-c-source-traverse-occur' to `anything-sources'
;; you will be able to use traverse from the main anything, but the
;; variable `anything-traverse-check-only' will not be available:
;; When searching in a dired buffer the search will be performed on ALL files.
;; If instead you use `anything-traverse' , a prefix arg will be available:
;; C-u M-x anything-traverse will give you a prompt for the .ext file to use only.
;; You can give as many .ext file you want at this prompt separated with a space.
;; Exemple: SearchOnly(.ext):.el .py .sh 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:

(require 'traverselisp)

(defvar anything-c-traverse-overlay-face nil)
(defvar anything-traverse-occur-overlay nil)
(defun anything-traverse-occur-color-current-line ()
  "Highlight and underline current position"
  (if (not anything-traverse-occur-overlay)
      (setq anything-traverse-occur-overlay
            (make-overlay
             (line-beginning-position) (1+ (line-end-position))))
      (move-overlay anything-traverse-occur-overlay
                    (line-beginning-position) (1+ (line-end-position))))
  (overlay-put anything-traverse-occur-overlay
               'face anything-c-traverse-overlay-face))

(defun anything-c-traverse-buffer-action (elm)
  (let (pos-elm)
    (when (string-match "[0-9]+" elm 0)
      (setq pos-elm (string-to-number (match-string 0 elm))))
    (with-current-buffer anything-traverse-current-buffer
      (goto-line pos-elm))))

(defun anything-c-traverse-dir-action (elm)
  (let* ((elm-split (split-string elm " "))
         (fname (nth 0 elm-split))
         (line-number (first (split-string (nth 1 elm-split)
                                           ":"))))
    (find-file fname)
    (goto-line (string-to-number line-number))))

(defun anything-c-traverse-default-action (elm)
  (if anything-c-traverse-diredp-flag
      (anything-c-traverse-dir-action elm)
      (anything-c-traverse-buffer-action elm)))

(add-hook 'anything-cleanup-hook #'(lambda ()
                                     (when anything-traverse-occur-overlay
                                       (delete-overlay anything-traverse-occur-overlay)
                                       (setq anything-traverse-occur-overlay nil))
                                     (setq anything-traverse-check-only nil)))
                                     
(add-hook 'anything-after-persistent-action-hook #'(lambda ()
                                                     (when anything-traverse-occur-overlay
                                                       (delete-overlay anything-traverse-occur-overlay)
                                                       (anything-traverse-occur-color-current-line))))


(defvar anything-c-traverse-func 'traverse-buffer-process-ext)
(defvar anything-c-traverse-length-line 80
  "Length of the line displayed in anything buffer.")
(defvar anything-c-traverse-diredp-flag nil)
(defvar anything-traverse-check-only nil)
(defvar anything-c-source-traverse-occur
  '((name . "Traverse Occur")
    (init . (lambda ()
              (setq anything-traverse-current-buffer
                    (current-buffer))
              (let ((dired-buffer-name (find (rassoc anything-traverse-current-buffer
                                                             dired-buffers)
                                                     dired-buffers)))
                (if dired-buffer-name
                    (setq anything-c-traverse-diredp-flag t)
                    (setq anything-c-traverse-diredp-flag nil)))))
    (candidates . (lambda ()
                    (let ((anything-traverse-buffer (get-buffer-create "*Anything traverse*"))
                          (dired-buffer-name (find (rassoc anything-traverse-current-buffer
                                                           dired-buffers)
                                                   dired-buffers)))
                      (with-current-buffer anything-traverse-buffer
                        (erase-buffer)
                        (goto-char (point-min))
                        (if anything-c-traverse-diredp-flag
                            (dolist (f (traverse-list-directory (car dired-buffer-name) t))
                              (if anything-traverse-check-only
                                  (when (member (file-name-extension f t)
                                                anything-traverse-check-only)
                                    (traverse-file-process-ext
                                     anything-pattern
                                     f))
                                  (unless (or (file-directory-p f)
                                              (member (file-name-extension f t)
                                                      traverse-ignore-files)
                                              (member (file-name-nondirectory f)
                                                      traverse-ignore-files)
                                              (file-compressed-p f)
                                              (file-symlink-p f)
                                              (not (file-regular-p f)))
                                    (traverse-file-process-ext
                                     anything-pattern
                                     f))))
                            (traverse-buffer-process-ext
                             anything-pattern
                             anything-traverse-current-buffer
                             :lline anything-c-traverse-length-line))
                        (split-string (buffer-string) "\n")))))
    (action . (("Go to Line" . (lambda (elm)
                                 (anything-c-traverse-default-action elm)))))
    (persistent-action . (lambda (elm)
                           (anything-c-traverse-default-action elm)
                           (anything-traverse-occur-color-current-line)))
    (requires-pattern . 3)
    (get-line . buffer-substring)
    (volatile)
    (delayed)))

;; (anything 'anything-c-source-traverse-occur)

(defun* anything-traverse-next-or-prec-file (&optional (n 1))
  "When search is performed in dired buffer on all files
this allow to switch from one file to the other.
If we are in another source just go to next/prec line."
  (interactive)
  (with-anything-window
      (if anything-c-traverse-diredp-flag
          (progn
            (let* ((current-line-list (split-string
                                       (buffer-substring
                                        (point-at-bol)
                                        (point-at-eol))))  
                   (current-fname (nth 0 current-line-list))
                   ;; Don't use file names like "somename+.el"
                   (current-fname-less (replace-regexp-in-string "\+"
                                                                 ""
                                                                 (file-name-sans-extension
                                                                  current-fname)))
                   (fn-b-o-f (if (eq n 1) 'eobp 'bobp))) ; func back or forward
              (catch 'break
                (while (not (funcall fn-b-o-f))
                  (forward-line n)
                  (beginning-of-line)
                  (when (not (or (re-search-forward current-fname
                                                    (point-at-eol) t)
                                 (when (string-match "\+" current-fname)
                                   (re-search-forward current-fname-less
                                                      (point-at-eol) t))))
                    (anything-mark-current-line)
                    (throw 'break nil))))
              (if (eq n 1)
                  (when (eobp)
                    (re-search-backward ".")
                    (beginning-of-line)
                    (anything-mark-current-line))
                  (when (bobp)
                    (forward-line)
                    (beginning-of-line)
                    (anything-mark-current-line)))))
          (if (eq n 1)
              (anything-next-line)
              (anything-previous-line)))))

(defun anything-traverse ()
  "Launch anything with traverse separately"
  (interactive)
  (when current-prefix-arg
    (setq anything-traverse-check-only
          (split-string (read-string "SearchOnly(.ext): "))))
  (anything 'anything-c-source-traverse-occur))

(define-key anything-map (kbd "M-n") #'anything-traverse-next-or-prec-file)
(define-key anything-map (kbd "M-p") #'(lambda ()
                                         (interactive)
                                         (anything-traverse-next-or-prec-file -1)))

(defface anything-overlay-face '((t (:background "MediumAquamarine" :underline t)))
  "Face for source header in the anything buffer." :group 'anything)

(setq anything-c-traverse-overlay-face 'anything-overlay-face)

(provide 'anything-traverse)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; anything-traverse.el ends here
