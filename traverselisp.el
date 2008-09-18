;;; traverselisp.el -- Search and replace through directorys
;;
;; Filename: traverselisp.el
;; Description: A clone of rgrep wrote all in lisp.
;; Also: walk through directories and perform diverses actions on files.
;; Author: Thierry Volpiatto 
;; Maintainer: Thierry Volpiatto 
;; Created: ven aoû  8 16:23:26 2008 (+0200)
;; Version:
(defconst traverse-version "1.13")
;; Copyright (C) 2008, Thierry Volpiatto, all rights reserved
;; Last-Updated: jeu sep 18 15:59:16 2008 (+0200)
;;           By: thierry
;;     Update #: 290
;; URL: http://freehg.org/u/thiedlecques/traverselisp/
;; Keywords: 

;; Compatibility:

;; Developped and tested on:
;; GNU Emacs 23.0.60.1 (i686-pc-linux-gnu, GTK+ Version 2.12.11)

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
;; M-x `traverse-deep-rfind'
;; When searching is done and you are in traverse buffer
;; some actions are provided for navigate and for replacing regexps
;; Use "C-h m" for more info.
;; You can use customize to set some variables :
;; (customize-group "traversedir")

;; Other functions are provided:
;; `traverse-cp-or-mv-extfiles-in-dir'
;; `traverse-build-tags-in-project'

;; Use customize to configure or setq differents variables.
;; 
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
    (define-key map [?R] 'traverse-search-and-replace-all)
    (define-key map [?N] 'traverse-go-forward)
    (define-key map [?P] 'traverse-go-backward)
    (define-key map [(shift down)] 'traverse-scroll-down-other-window)
    (define-key map [(shift up)] 'traverse-scroll-up-other-window)
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
    ".xbm" ".gpg"
    ".svg")
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

(defcustom traverse-use-avfs
  nil
  "Enable support for avfs"
  :group 'traversedir
  :type 'boolean)

(defcustom traverse-avfs-default-directory
  "~/.avfs"
  "Default directory for avfs"
  :group 'traversedir
  :type 'string)

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

(defsubst traverse-hash-readlines (file table)
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
founded in the hash-table created by `traverse-hash-readlines'
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

(defun traverse-button-func (button)
  "The function called by buttons in traverse buffer"
  (let* ((list-line (split-string (thing-at-point 'line)))
         (nline (nth 1 list-line))
         (regex)
         (fname (button-label (button-at (point)))))
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "for ")
        (setq regex
              (buffer-substring (point)
                          (- (line-end-position) 1)))))
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

;;;###autoload
(defun traverse-search-and-replace (str &optional regex)
  "Replace regex with `str', replacement is
performed only on current line"
  (interactive "sNewstring: ")
  (if (eq (current-buffer) (get-buffer "*traverse-lisp*"))
      (progn
        (let ((pos (point)))
          (goto-char (point-min))
          (when (not regex)
            (when (re-search-forward "for ")
              (setq regex
                    (buffer-substring (point)
                                      (- (line-end-position) 1)))))
          (goto-char pos)
          (if (button-at (point))
              (progn
                (save-window-excursion
                  (let ((fname (button-label (button-at (point))))
                        (flag-w nil))
                    (setq fname (replace-regexp-in-string "\\[" "" fname))
                    (setq fname (replace-regexp-in-string "\\]" "" fname))
                    (setq fname (expand-file-name fname))
                    (push-button)
                    ;; We are now in the file buffer
                    (with-current-buffer (find-buffer-visiting fname) 
                      (if (and (file-writable-p fname)
                               (not (backup-file-name-p fname)))
                          (let ((beg (point)))
                            (goto-char (+ beg (length regex)))
                            (delete-region beg (point))
                            (insert str)
                            (save-buffer)
                            (highlight-regexp str 'hi-pink)
                            (sit-for 1)
                            (kill-buffer (current-buffer))
                            (setq flag-w t))
                          (kill-buffer (current-buffer))))
                ;; We are back in traverse-buffer
                (beginning-of-line)
                (delete-region (point) (line-end-position))
                (delete-blank-lines)
                (forward-line 1)
                (if flag-w
                    (message "<%s> Replaced by <%s> in [%s]"
                             (propertize regex
                                         'face 'traverse-regex-face)
                             (propertize str
                                         'face 'traverse-match-face)
                             fname)
                    (message "Skipping: File not writable or under vc")))))
              (message "We are not on a button!"))))
      (error "You are not in a traverse-buffer, run first traverse-deep-rfind")))

;;;###autoload
(defun traverse-search-and-replace-all (str)
  "Launch search and replace interactively on all occurences
commands provided here are: (n)ext (a)ll (s)kip (x)stop"
  (interactive "sNewstring: ")
  (if (eq (current-buffer) (get-buffer "*traverse-lisp*"))
      (progn
        (goto-char (point-min))
        (let ((mem-srd traverse-show-regexp-delay)
              (action "")
              (count 0)
              (regex (when (re-search-forward "for ")
                       (buffer-substring (point)
                                    (- (line-end-position) 1)))))
          (unwind-protect
               (progn
                 (setq traverse-show-regexp-delay 0)
                 (when (not (button-at (point)))
                   (goto-char (point-min))
                   (forward-button 1))
                 (catch 'break
                   (while (button-at (point))
                     (catch 'continue
                       (if (eq action '?a)
                           ;; replace all without asking
                           (progn
                             (traverse-search-and-replace str regex)
                             (incf count))
                           ;; ask for next action and set it
                           (setq action (read-event (concat (propertize "Next("
                                                                        'face 'traverse-match-face)
                                                            (propertize "n "
                                                                        'face 'traverse-path-face)
                                                            (propertize ") All("
                                                                        'face 'traverse-match-face)
                                                            (propertize "a"
                                                                        'face 'traverse-path-face)
                                                            (propertize ") Skip("
                                                                        'face 'traverse-match-face)
                                                            (propertize "s"
                                                                        'face 'traverse-path-face)
                                                            (propertize ") Stop("
                                                                        'face 'traverse-match-face)
                                                            (propertize "x"
                                                                        'face 'traverse-path-face)
                                                            (propertize ") :"
                                                                        'face 'traverse-match-face))))
                           (case action
                             ('?n (progn
                                    (traverse-search-and-replace str regex)
                                    (incf count)
                                    (throw 'continue nil)))
                             ('?a (progn
                                    (message "Replacing all, you can %s at any time with %s"
                                             (propertize "STOP"
                                                         'face 'traverse-match-face)
                                             (propertize "<C-g>"
                                                         'face 'traverse-match-face))
                                    (sit-for 3)
                                    (traverse-search-and-replace str regex)
                                    (incf count)
                                    (throw 'continue nil)))
                             ('?s (progn
                                    (delete-region (point) (line-end-position))
                                    (delete-blank-lines)
                                    (forward-button 1)
                                    (throw 'continue nil)))
                             ('?x (progn
                                    (throw 'break nil)))
                             (t (progn
                                  (error "Unknow command, operation Aborted")
                                  (throw 'break nil)))))))))
            (if (eq action '?x)
                ;; action is stopped
                (progn
                  (setq traverse-show-regexp-delay mem-srd)
                  (message "[%s] Occurences of %s replaced by <%s>"
                           (propertize (int-to-string count)
                                       'face 'traverse-match-face)
                           (propertize regex
                                       'face 'traverse-regex-face)
                           (propertize str
                                       'face 'traverse-path-face)))
                ;; action is finish
                (setq traverse-show-regexp-delay mem-srd)
                (when (re-search-backward "^Found")
                  (beginning-of-line)
                  (delete-region (point) (line-end-position))
                  (highlight-regexp str "hi-pink")
                  (highlight-regexp "^\\[.\\]" "hi-green")
                  (insert (format "[%s] Occurences of <%s> replaced by <%s>"
                                  count
                                  regex
                                  str)))))))
      (error "You are not in a traverse-buffer, run first traverse-deep-rfind")))


(defun traverse-file-process (regex fname &optional full-path)
  "Default function to process files  and insert matched lines
in *traverse-lisp* buffer"
  (clrhash traverse-table)
  (traverse-hash-readlines fname traverse-table)
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

(defun traverse-prepare-buffer ()
  "Prepare traverse buffer"
  (set-buffer (get-buffer-create "*traverse-lisp*"))
  (erase-buffer)
  (hi-lock-mode 1)
  (goto-char (point-min))
  (traversedir-mode)
  (insert " *Traverse-lisp-output*\n\n\n")
  (highlight-regexp " \\*Traverse-lisp-output\\*$" "hi-pink")
  (display-buffer "*traverse-lisp*")
  (insert  "Wait Lisp searching...\n\n")
  (sit-for 1))

(defun traverse-find-in-file (fname regexp &optional full-path)
  "Traverse search regex in a single file"
  (interactive "fFileName: \nsRegexp: ")
  (traverse-prepare-buffer)
  (let ((prefarg (not (null current-prefix-arg))))
    (when
        (and (file-regular-p fname)
             (not (file-symlink-p fname)))
      (traverse-file-process regexp fname prefarg))
    (goto-char (point-min))
    (when (re-search-forward "^Wait")
      (beginning-of-line)
      (kill-line)
      (insert (format "Found %s occurences for %s:\n"
                      traverse-count-occurences
                      regexp))
      (message "%s Occurences found for %s"
               (propertize (int-to-string traverse-count-occurences)
                           'face 'traverse-match-face)
               (propertize regexp
                           'face 'traverse-regex-face))
      (highlight-regexp regexp) 
      (setq traverse-count-occurences 0))))
  

;;;###autoload
(defun traverse-deep-rfind (tree regexp &optional only)
  "Main function that call walk, if only is omitted it
will be set as nil and search will be proceeded on all files
except on files that are in `traverse-ignore-files'
Called with prefix-argument (C-u) absolute path is displayed"
  (interactive "DTree: \nsRegexp: \nsCheckOnly: ")
  (save-excursion
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
                                        (let ((prefarg (not (null current-prefix-arg))))
                                          (if only
                                              (when (equal (file-name-extension y t) only)
                                                (funcall traverse-file-function regexp y prefarg))
                                              (funcall traverse-file-function regexp y prefarg)))
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
        (setq traverse-count-occurences 0)))))

;;; Dired functions
(defun traverse-search-in-dired-dir-at-point (regex &optional only)
  "Launch `traverse-deep-rfind' from `dired-mode'"
  (interactive "sRegexp: \nsCheckOnly: ")
  (if (eq major-mode 'dired-mode)
      (let ((tree (dired-get-filename)))
        (if (file-directory-p tree)
            (traverse-deep-rfind tree regex only)
            (message "Sorry! %s is not a Directory" tree)))
      (message "Hoops! We are not in Dired!")))


(defun traverse-search-in-dired-file-at-point (regex)
  (interactive "sRegexp: ")
  (if (eq major-mode 'dired-mode)
      (let ((fname (dired-get-filename)))
        (if (file-regular-p fname)
            (traverse-find-in-file fname regex)
            (message "Sorry! %s is not a regular file" fname)))
      (message "Hoops! We are not in Dired!")))


(defun traverse-dired-browse-archive ()
  "This function use AVFS and FUSE, so be sure
to have these programs and modules installed on your system"
  (interactive)
  (when traverse-use-avfs
    (let ((file-at-point (dired-get-filename)))
      (if (or (equal (file-name-extension file-at-point) "gz")
              (equal (file-name-extension file-at-point) "bz2")
              (equal (file-name-extension file-at-point) "zip"))
          (progn
            (when (not (cddr (directory-files traverse-avfs-default-directory)))
              (shell-command "mountavfs"))
            (find-file (concat traverse-avfs-default-directory file-at-point "#")))
          (find-file file-at-point)))))


(defun traverse-dired-search-in-archive (regexp &optional only)
  "This function use AVFS and FUSE, so be sure
to have these programs installed on your system and FUSE module
enabled in your kernel.
This function is disabled by default, enable it setting
traverse-use-avfs to non--nil"
  (interactive "sRegexp: \nsCheckOnly: ")
  (when traverse-use-avfs
    (let ((file-at-point (dired-get-filename)))
      (if (or (equal (file-name-extension file-at-point) "gz")
              (equal (file-name-extension file-at-point) "bz2")
              (equal (file-name-extension file-at-point) "zip"))
          (progn
            (when (not (cddr (directory-files traverse-avfs-default-directory)))
              (shell-command "mountavfs"))
            (traverse-deep-rfind (concat traverse-avfs-default-directory file-at-point "#")
                                 regexp
                                 only))
          (message "That's not a compressed file")))))

(defun file-compressed-p (fname)
  "Return t if fname is a compressed file"
  (let ((ext (file-name-extension fname)))
    (cond ((equal ext "gz")
           t)
          ((equal ext "bz2")
           t)
          ((equal ext "zip")
           t)
          (t nil))))

(defun traverse-dired-search-regexp-in-anything-at-point (regexp &optional only)
  "Generic function for dired"
  (interactive
   (let ((f-or-d-name (dired-get-filename)))
     (cond ((and (file-regular-p f-or-d-name)
                 (not (file-compressed-p f-or-d-name)))
            (list (read-string "Regexp: ")))
           ((or (file-directory-p f-or-d-name)
                (and (file-regular-p f-or-d-name)
                     (file-compressed-p f-or-d-name)))
            (list (read-string "Regexp: ")
                  (read-string "CheckOnly: "))))))
  (let ((fname (dired-get-filename)))
     (cond ((and (file-regular-p fname)
                 (not (file-compressed-p fname)))
            (traverse-search-in-dired-file-at-point regexp))
           ((file-directory-p fname)
            (traverse-search-in-dired-dir-at-point regexp only))
           ((and (file-regular-p fname)
                 (file-compressed-p fname))
            (traverse-dired-search-in-archive regexp only)))))
    
;;;; Navigate in traverse
(defun traverse-go-forward-or-backward (num)
  (other-window -1)
  (when (buffer-file-name (current-buffer))
    (save-buffer)
    (kill-buffer (current-buffer)))
  (other-window -1)
  (forward-button num)
  (push-button)
  (other-window -1))

(defun traverse-go-forward (&optional num)
  (interactive "p")
  (traverse-go-forward-or-backward (or num 1)))
  
(defun traverse-go-backward (&optional num)
  (interactive "p")
  (traverse-go-forward-or-backward (- (or num 1))))

(defun traverse-scroll-down-other-window ()
  (interactive)
   (when (equal (current-buffer)
               (get-buffer "*traverse-lisp*"))
    (scroll-other-window 1)))

(defun traverse-scroll-up-other-window ()
  (interactive)
   (when (equal (current-buffer)
               (get-buffer "*traverse-lisp*"))
  (scroll-other-window -1)))


;;;; Utils
;;;###autoload
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

;; Experimental ==> Huge projects not supported (Tags files become to big)
;;;###autoload
(defun traverse-build-tags-in-project (dir ext &optional new-file)
  "Build an etags file in current project.
If `new-file' is non-nil (do it with C-u) build a new file
instead of appending to the current one.
Many file extensions can be enter at `ext' prompt.
Tag file will be build in `dir'"
  (interactive "Ddir: \nsExt: ")
  (let ((ext-list (split-string ext))
        (count 0))
    (when current-prefix-arg
      (setq new-file t))
    (when new-file
      (delete-file (expand-file-name "TAGS" dir)))
    (dolist (i ext-list)
      (traverse-walk-directory dir
                               #'(lambda (x)
                                   (when (equal (file-name-extension x t) i)
                                     (message "Tagging [%s]" (propertize x
                                                                         'face 'traverse-path-face))
                                     (incf count)
                                     (call-process-shell-command (format "etags %s -a %s"
                                                            x
                                                            (expand-file-name "TAGS" dir)))))
                               nil
                               traverse-ignore-dirs))
    (message "%s Files tagged" (propertize (int-to-string count)
                                           'face 'traverse-match-face))))

(provide 'traverselisp)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; traverselisp.el ends here


