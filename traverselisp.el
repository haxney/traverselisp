;;; traverselisp.el 
;; 
;; Filename: traverselisp.el
;; Description: A clone of rgrep wrote all in lisp.
;; Also: walk through directories and perform diverses actions on files.
;; Author: Thierry Volpiatto 
;; Maintainer: Thierry Volpiatto 
;; Created: ven aoû  8 16:23:26 2008 (+0200)
;; Version:
(defconst traverse-version "1.2")
;; Copyright (C) 2008, Thierry Volpiatto, all rights reserved
;; Last-Updated: mer aoû 27 21:55:16 2008 (+0200)
;;           By: thierry
;;     Update #: 70
;; URL: http://freehg.org/u/thiedlecques/traverselisp/
;; Keywords: 

;; Compatibility:

;; Work on all platforms that support emacs.
;; Don't need any backend.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary:

;; Install:
;; =======
;; (require 'traverselisp)
;; (setq max-lisp-eval-depth 40000) ; should work with lower values now.

;; Usage:
;; =====
;; M-x traverse-deep-rfind
;; Use customize to configure or setq differents variables.

;; Contact:
;; =======
;; thierry dot volpiatto hat gmail dot com
;; You can get the developpement version of the file here with hg:
;; hg clone http://freehg.org/u/thiedlecques/traverselisp/

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change log:
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:

(require 'derived)
(eval-when-compile (require 'cl))

(defvar traversedir-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?q] 'traverse-quit)
    (define-key map [?S] 'traverse-search-and-replace)
    (define-key map [?!] 'traverse-search-and-replace-all)
    (define-key map [?X] 'traverse-search-and-replace-break)
    map)
  "Keymap used for traversedir commands.")

(define-derived-mode traversedir-mode text-mode "traversedir"
  "Major mode to search regexp in files recursively.

Special commands:
\\{traversedir-mode-map}")


(defun traverse-quit ()
  "Quit and kill traverse buffer"
  (interactive)
  (quit-window t))

(defgroup traversedir nil
  "Mode to search recursively regex like grep-find"
  :prefix "traversedir-"
  :group 'text)

(defcustom traverse-ignore-files
  '(".elc" ".pyc"
    ".orig" ".bz2"
    ".gz" ".zip"
    ".vdi" ".doc"
    ".jpg" ".avi"
    ".jpeg" ".png"
    ".xpm" ".jar"
    ".pbm" ".gif"
    ".xls" ".ppt"
    ".mdb" ".adp"
    "TAGS" ".tiff"
    ".pdf" ".dvi"
    ".xbm" ".gpg")
  "Files we want to ignore (extensions)"
  :group 'traversedir
  :type '(repeat string))

(defcustom traverse-ignore-dirs
  '(".hg" ".svn"
    "RCS" ".bzr"
    ".git" ".VirtualBox"
    ".arch-ids" "CVS"
    "{arch}")
  "Directory we don't want to search in"
  :group 'traversedir
  :type '(repeat string))

(defcustom traverse-length-line
  45
  "Length of the line displayed"
  :group 'traversedir
  :type 'integer)

(defcustom traverse-file-function
  'traverse-file-process
  "Default function to use to process files"
  :group 'traversedir
  :type 'symbol)

(defcustom traverse-show-regexp-delay
  2
  "Delay in seconds where regexp found is highligted"
  :group 'traversedir
  :type 'integer)

(defgroup traverse-faces nil
  "Faces for TRAVERSEDIR."
  :group 'traversedir)

(defface traverse-match-face '((t (:foreground "red")))
  "TRAVERSEDIR face."
  :group 'traverse-faces)
(defface traverse-regex-face '((t (:foreground "yellow")))
  "TRAVERSEDIR face."
  :group 'traverse-faces)
(defface traverse-path-face '((t (:foreground "green")))
  "TRAVERSEDIR face."
  :group 'traverse-faces)


(defun traverse-lisp-version ()
  (interactive)
  (message "traverse-lisp-version-%s" traverse-version))

;;; Main backend functions


(defsubst traverse-list-directory (dirname &optional abs)
  "Use directory-files without these \".\" \"..\".
If abs is non-nil use absolute path."
  (cddr (directory-files dirname abs)))
     

(defun traverse-walk-directory (dirname file-fn &optional exclude-files exclude-dirs)
    "Walk through dirname and use file-fn function
on each file found.
`dirname' ==> we start in this directory
`file-fn' ==> function to apply to FILES
`excludes-files' ==> list of .ext to ignore  
`exclude-dirs' ==> list of directory to ignore
"
    (labels
        ((walk (name)
           (cond ((and (file-directory-p name) ;; DIR PROCESSING
                       (not (file-symlink-p name))) ;; don't follow symlinks
                  (if exclude-dirs
                      (dolist (x (traverse-list-directory name t))
                        (when x ;; be sure x is a string and not nil
                          (if (and (not (equal (file-name-nondirectory x)
                                               "."))
                                   (not (equal (file-name-nondirectory x)
                                               "..")))
                              (unless (member (file-name-nondirectory x) exclude-dirs)
                                (walk x))))) ;; Return to TOP and take the good cond
                      (dolist (x (traverse-list-directory name t))
                        (when x
                          (if (and (not (equal (file-name-nondirectory x)
                                               "."))
                                   (not (equal (file-name-nondirectory x)
                                               "..")))
                              (walk x)))))) ;; Return to TOP and take the good cond
                 ((and (file-regular-p name) ;; FILE PROCESSING
                       (not (file-symlink-p name))) ;; don't follow symlinks
                  (if exclude-files
                      (unless (or (member (file-name-extension name t) exclude-files)
                                  (member (file-name-nondirectory name) exclude-files))
                        (funcall file-fn name))
                      (funcall file-fn name))))))
      (walk (expand-file-name dirname))))


(defvar traverse-count-occurences 0)

(defvar traverse-table (make-hash-table))

(defsubst hash-readlines (file table)
  "Load all the line of a file in an hash-table
with the number of line as key.
\\(emulate object.readlines() of python)"
  (let* ((my-string (with-temp-buffer
                       (insert-file-contents file)
                       (buffer-string)))
          (my-read-list (split-string my-string "\n"))
          (count 0))
     (dolist (i my-read-list)
       (puthash count i table)
       (incf count))))


(defsubst traverse-find-all-regex-in-hash (regex table)
  "Return a list of all lines that match regex
founded in the hash-table created by `hash-readlines'
Each element of the list is a list of the form '(key value)"
  (let ((match-list nil))
    (maphash #'(lambda (x y)
                 (when (string-match regex y)
                    (push (list x
                                (replace-regexp-in-string "\n"
                                                          ""
                                                          y))
                          match-list)))
             table)
    (setq match-list (reverse match-list))
    match-list))

;;;###autoload
(defun traverse-button-func (button)
  "The function called by buttons in traverse buffer"
  (let* ((list-line (split-string (thing-at-point 'line)))
         (nline (nth 1 list-line))
         (regex)
         (fname (button-label (button-at (point)))))
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^Found ")
        (end-of-line)
        (beginning-of-sexp)
        (setq regex (thing-at-point 'sexp))))
    (save-excursion
      (setq fname (replace-regexp-in-string "\\[" "" fname))
      (setq fname (replace-regexp-in-string "\\]" "" fname))
      (find-file-other-window fname) 
      (goto-line (string-to-number nline))
      (setq case-fold-search t)
      (beginning-of-line)
      (when (re-search-forward regex nil nil)
        (goto-char (- (point) (length regex)))
        (highlight-regexp regex)
        (sit-for traverse-show-regexp-delay)
        (unhighlight-regexp regex)))))

(defun traverse-search-and-replace (str)
  "Replace regex with `str', replacement is
performed only on current line"
  (interactive "sNewstring: ")
  (let ((pos (point))
        (regex))
    (goto-char (point-min))
    (when (re-search-forward "^Found ")
      (end-of-line)
      (beginning-of-sexp)
      (setq regex (thing-at-point 'sexp)))
    (goto-char pos)
    (if (button-at (point))
        (progn
          (save-window-excursion
            (let ((fname (button-label (button-at (point)))))
              (setq fname (replace-regexp-in-string "\\[" "" fname))
              (setq fname (replace-regexp-in-string "\\]" "" fname))
              (push-button)
              (with-current-buffer (file-name-nondirectory fname) 
                (let ((beg (point)))
                  (goto-char (+ beg (length regex)))
                  (delete-region beg (point))
                  (insert str))
                (save-buffer)
                (kill-buffer (current-buffer)))))
          (beginning-of-line)
          (delete-region (point) (line-end-position))
          (delete-blank-lines)
          (forward-line 1)
          (message "%s Replaced by %s"
                   (propertize regex
                               'face 'traverse-regex-face)
                   (propertize str
                               'face 'traverse-match-face)))
        (message "We are not on a button!"))))


(defun traverse-search-and-replace-all (str)
  "Launch search and replace on all occurences
you can stop it with X"
  (interactive "sNewstring: ")
  (let ((mem-srd traverse-show-regexp-delay))
    (unwind-protect
         (progn
           (setq traverse-show-regexp-delay 0)
           (when (not (button-at (point)))
             (goto-char (point-min))
             (forward-button 1))
           (catch 'break-sar
             (while (button-at (point))
               (traverse-search-and-replace str)
               (if traverse-sar-break
                   (throw 'break-sar
                     (progn
                       (message "Traverse-search-and-replace stopped!")
                       (setq traverse-sar-break nil)))))))
      (setq traverse-show-regexp-delay mem-srd))))


(defvar traverse-sar-break nil
  "When non nil stop traverse search and replace loop")
(defun traverse-search-and-replace-break ()
  "Break traverse-search-and-replace loop"
  (interactive)
  (setq traverse-sar-break t))

(defun traverse-file-process (regex fname &optional full-path)
  "Default function to process files  and insert matched lines
in *traverse-lisp* buffer"
  (clrhash traverse-table)
  (hash-readlines fname traverse-table)
  (let ((matched-lines (traverse-find-all-regex-in-hash regex traverse-table)))
    (when matched-lines 
      (dolist (i matched-lines) ;; each element is of the form '(key value)
        (let ((line-to-print (replace-regexp-in-string "\\(^ *\\)" "" (second i))))
          (and (insert-button (format "[%s]" (if full-path
                                                 fname
                                                 (file-relative-name fname
                                                                     default-directory)))
                              'action 'traverse-button-func
                              'face "hi-green")
               (insert (concat " "
                               (int-to-string (+ (first i) 1))
                               ":"
                               (if (> (length line-to-print)
                                      traverse-length-line)
                                   (substring line-to-print
                                              0
                                              traverse-length-line)
                                   line-to-print)
                               "\n")))))
        (setq traverse-count-occurences (+ traverse-count-occurences
                                           (length matched-lines))))))

;;;###autoload
(defun traverse-deep-rfind (tree regexp &optional only)
  "Main function that call walk, if only is omitted it
will be set as nil and search will be proceeded on all files
except on files that are in `traverse-ignore-files'"
  (interactive "DTree: \nsRegexp: \nsCheckOnly: ")
  (set-buffer (get-buffer-create "*traverse-lisp*"))
  (erase-buffer)
  (hi-lock-mode 1)
  (goto-char (point-min))
  (traversedir-mode)
  (insert " *Traverse-lisp-output*\n\n\n")
  (highlight-regexp " \\*Traverse-lisp-output\\*$" "hi-pink")
  (display-buffer "*traverse-lisp*")
  (insert  "Wait Lisp searching...\n\n")
  (sit-for 1)
  (let ((init-time (cadr (current-time))))
    (unwind-protect
         (traverse-walk-directory tree
                            #'(lambda (y)
                                (if (equal only "")
                                    (setq only nil))
                                (if current-prefix-arg
                                    (if only
                                        (when (equal (file-name-extension y t)
                                                     only)
                                          (funcall traverse-file-function regexp y t))
                                        (funcall traverse-file-function regexp y t))
                                    (if only
                                        (when (equal (file-name-extension y t)
                                                     only)
                                          (funcall traverse-file-function regexp y))
                                        (funcall traverse-file-function regexp y)))
                                (message "%s [Matches] for %s in [%s]"
                                         (if (>= traverse-count-occurences 1)
                                             (propertize (int-to-string traverse-count-occurences)
                                                         'face 'traverse-match-face)
                                             0)
                                         (propertize regexp
                                                     'face 'traverse-regex-face)
                                         (propertize y
                                                     'face 'traverse-path-face)))
                            traverse-ignore-files
                            traverse-ignore-dirs)
      (setq traverse-count-occurences (if (< traverse-count-occurences 0)
                                          0
                                          traverse-count-occurences))
      (if (eq traverse-count-occurences 0)
          (progn
            (goto-char (point-min))
            (when (re-search-forward "^Wait")
              (beginning-of-line)
              (kill-line)
              (insert "Oh!No! Nothing found!")))
          (goto-char (point-min))
          (when (re-search-forward "^Wait")
            (beginning-of-line)
            (kill-line)
            (insert (format "Search performed in %s seconds\n\n"
                            (- (cadr (current-time)) init-time)))
            (insert (format "Found %s occurences for %s:\n"
                            traverse-count-occurences
                            regexp))))
      (message "%s Occurences found for %s in %s seconds"
               (propertize (int-to-string traverse-count-occurences)
                           'face 'traverse-match-face)
               (propertize regexp
                           'face 'traverse-regex-face)
               (- (cadr (current-time)) init-time))
    (highlight-regexp regexp) 
    (setq traverse-count-occurences 0))))
    
(defun traverse-cp-or-mv-extfiles-in-dir (tree ext dir &optional func)
  "Recurse in `tree' and copy/move all files with `ext' in `dir'.
Default is copying, called with prefix-arg (C-u) Move files with `ext' in `Dir'
`func' is a symbol when called non-interactively
Note: `dir' will be use as target and NO search inside it will be performed.
If you want to move/copy files that are nested in subdir(s) of `dir'
It will fail silently.==> So use another dir target"
  (interactive "DTree: \nsExt(with dot): \nGTargetDirectory: ")
  (let ((fn (cond (func
                   func)
                  (current-prefix-arg
                   'rename-file)
                  (t 'copy-file)))
        (igndir (file-name-nondirectory dir)))
    (unless (file-directory-p dir)
      (make-directory dir t))
    (traverse-walk-directory tree
                       #'(lambda (x)
                           (if (or (eq fn 'rename-file)
                                   (eq fn 'copy-file))
                               (when (equal (file-name-extension x t) ext)
                                 (funcall fn (expand-file-name x) dir 1))
                               (when (equal (file-name-extension x t) ext)
                                 (funcall fn (expand-file-name x)))))
                       nil
                       `(,igndir))))

(provide 'traverselisp)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; traverselisp.el ends here


