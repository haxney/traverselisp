;;; traverselisp.el -- Search and replace...
;;
;; Filename: traverselisp.el
;; Description: A clone of rgrep wrote all in lisp.
;; Also: walk through directories and perform diverses actions on files.
;; Author: Thierry Volpiatto 
;; Maintainer: Thierry Volpiatto 
;; Created: ven aoû  8 16:23:26 2008 (+0200)
;;
;; Copyright (C) 2008, Thierry Volpiatto, all rights reserved
;; Last-Updated: dim déc 14 08:24:12 2008 (+0100)
;;           By: thierry
;;     Update #: 438
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
;; Put this file in your load-path
;; Add to your .emacs:
;;
;; (require 'traverselisp)
;;
;; Set up your prefered keys for dired and globals as usual
;;
;; Here is my config with version-1.16:
;; ===================================
;; (require 'traverselisp)
;; (setq traverse-use-avfs t)
;; (global-set-key (kbd "<f5> f") 'traverse-deep-rfind)
;; (global-set-key (kbd "<f5> u") 'traverse-build-tags-in-project)
;; (global-set-key (kbd "C-c C-o") 'traverse-occur-current-buffer)
;; (define-key dired-mode-map (kbd "A") 'traverse-dired-search-regexp-in-anything-at-point)
;; (define-key dired-mode-map (kbd "C-c C-z") 'traverse-dired-browse-archive)
;; (define-key dired-mode-map (kbd "C-c t") 'traverse-dired-find-in-all-files)
;; (add-to-list 'traverse-ignore-files ".ledger-cache")

;; You can use customize to set some variables : (eval with C-x C-e)
;; (customize-group "traversedir")

;; Usage:
;; =====
;; M-x `traverse-deep-rfind'
;; When searching is done and you are in traverse buffer
;; some interactive actions are provided for navigate and for replacing regexps
;; Use "C-h m" for more info while in traverse-buffer.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Special commands:
;; key             binding
;; ---             -------

;; ESC             Prefix Command
;; N               traverse-go-forward
;; P               traverse-go-backward
;; R               traverse-search-and-replace-all ==> [interactive menu]
;; S               traverse-search-and-replace
;; q               traverse-quit
;; <S-down>        traverse-scroll-down-other-window
;; <S-up>          traverse-scroll-up-other-window
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; You can also use traverse from DIRED:
;; M-x `traverse-dired-search-regexp-in-anything-at-point'
;; This function work on directory, files, (1)compressed files (AVFS)
;; and marked files:
;; If you have marked files search will be performed on these files.
;; If no files are marked, traverse will search in element at point
;; with appropriate function.
;; However, you can use specialized functions, check this file to see all
;; the interactives functions.
;;
;; M-x `traverse-dired-find-in-all-files'
;; Search in all regular files in the current dired buffer
;;
;; M-x `traverse-occur-current-buffer'
;; Just like occur but you can navigate and replace regexp.
;;
;; M-x `traverse-dired-browse-archive'
;; This function use (1)AVFS to browse archive tar.gz, bz2 etc..
;; Other functions are provided:
;; `traverse-cp-or-mv-extfiles-in-dir'
;; `traverse-build-tags-in-project'
;;
;; (1)NOTE: You have to install AVFS and enable fuse in your kernel if
;; you want to browse and search in archives.
;; Please see the doc of your distrib.
;; and the doc of AVFS
;; http://sourceforge.net/projects/avf
;; If you don't want to use AVFS in traverse, set `traverse-use-avfs'
;; to nil (or do nothing because it's the default)
;; 
;; You can also use traverselisp.el in anything.el with the appropriate sources:
;; http://www.emacswiki.org/emacs/AnythingSources
;;
;; Contact:
;; =======
;; thierry dot volpiatto hat gmail dot com
;; You can get the developpement version of the file here with hg:
;; hg clone http://freehg.org/u/thiedlecques/traverselisp/

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change log:
;; http://freehg.org/u/thiedlecques/traverselisp/ 
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
;; Version:
(defconst traverse-version "1.26")

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
    (define-key map [?|] 'traverse-toggle-split-window-h-v)
    map)
  "Keymap used for traversedir commands.")

(define-derived-mode traversedir-mode text-mode "traversedir"
  "Major mode to search regexp in files recursively.

Special commands:
\\{traversedir-mode-map}")

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
    ".svg" ".rej")
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

;;; Faces for traverse
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
(defface traverse-overlay-face '((t (:background "MediumAquamarine" :underline t)))
  "Face for highlight line in matched buffer."
  :group 'traverse-faces)


;;; User's variable (you can set these variables)
(defvar traverse-match-overlay-face 'traverse-overlay-face
  "Use the default traverse face for overlay")
(defvar traverse-show-regexp-delay 1
  "Delay in seconds where regexp found is highlighted")
(defvar traverse-keep-indent nil
  "Keep indentation in traverse buffer if non nil")
(defvar traverse-occur-use-miniwindow nil
  "Use a side miniwindow to display results")
(defvar traverse-miniwindow-width 30
  "If nil split window equally")

;;; Internal use only (DON'T modify)
(defvar traverse-count-occurences 0)
(defvar traverse-table (make-hash-table))
(defvar traverse-occur-overlay nil)

(defun traverse-lisp-version ()
  "Give version number of traverse"
  (interactive)
  (message "traverse-lisp-version-%s" traverse-version))

;;; Main backend functions

(defun traverse-quit ()
  "Quit and kill traverse buffer"
  (interactive)
  (when traverse-occur-overlay
    (delete-overlay traverse-occur-overlay))
  (quit-window t)
  (other-window 1)
  (delete-other-windows))

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

(defsubst traverse-hash-readlines (file table)
  "Load all the lines of a file in an hash-table
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

(defsubst traverse-hash-readlines-from-buffer (buffer table)
  "Load all the lines of a buffer in an hash-table
with the number of line as key.
\\(emulate object.readlines() of python)"
  (let* ((my-string (with-temp-buffer
                       (insert-buffer-substring buffer)
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

(defun traverse-occur-color-current-line ()
  "Highlight and underline current position"
  (if (not traverse-occur-overlay)
      (setq traverse-occur-overlay
            (make-overlay
             (line-beginning-position) (1+ (line-end-position))))
      (move-overlay traverse-occur-overlay
                    (line-beginning-position) (1+ (line-end-position))))
  (overlay-put traverse-occur-overlay
               'face traverse-match-overlay-face))

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
      (if (bufferp (get-buffer fname))
          (switch-to-buffer-other-window (get-buffer fname))
          (find-file-other-window fname))
      (goto-line (string-to-number nline))
      (setq case-fold-search t)
      (beginning-of-line)
      (when (re-search-forward regex nil nil)
        (goto-char (- (point) (length regex)))
        (traverse-occur-color-current-line)))))

;;;; Replace functions

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
                            (sit-for traverse-show-regexp-delay)
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
                                    (setq traverse-show-regexp-delay 1)
                                    (traverse-search-and-replace str regex)
                                    (incf count)
                                    (throw 'continue nil)))
                             ('?a (progn
                                    (message "Replacing all, you can %s at any time with %s"
                                             (propertize "STOP"
                                                         'face 'traverse-match-face)
                                             (propertize "<C-g>"
                                                         'face 'traverse-match-face))
                                    (setq traverse-show-regexp-delay 0.1)
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


(defun* traverse-file-process (regex fname &optional full-path &key (fn 'traverse-hash-readlines))
  "Default function to process files  and insert matched lines
in *traverse-lisp* buffer"
  (clrhash traverse-table)
  (funcall fn fname traverse-table)
  (let ((matched-lines (traverse-find-all-regex-in-hash regex traverse-table)))
    (when matched-lines 
      (dolist (i matched-lines) ;; each element is of the form '(key value)
        (let ((line-to-print (if traverse-keep-indent
                                 (second i)
                                 (replace-regexp-in-string "\\(^ *\\)" "" (second i)))))
          (and (cond ((eq fn 'traverse-hash-readlines)
                      (insert-button (format "[%s]" (if full-path
                                                        fname
                                                        (file-relative-name fname
                                                                            default-directory)))
                                     'action 'traverse-button-func
                                     'face "hi-green"))

                     ((eq fn 'traverse-hash-readlines-from-buffer)
                      (insert-button (format "[%s]" (buffer-name fname))
                                     'action 'traverse-button-func
                                     'face "hi-green")))
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


(defun* traverse-buffer-process-ext (regex buffer &key (lline traverse-length-line))
  "Function to process buffer in external program
like anything"
  (clrhash traverse-table)
  (traverse-hash-readlines-from-buffer buffer traverse-table)
  (let ((matched-lines (traverse-find-all-regex-in-hash regex traverse-table)))
    (when matched-lines
      (dolist (i matched-lines) ;; each element is of the form '(key value)
        (let* ((line-to-print (if traverse-keep-indent
                                  (second i)
                                  (replace-regexp-in-string "\\(^ *\\)" "" (second i))))
               (temp-list-line (split-string line-to-print regex))
               (line-to-print-hight (concat (nth 0 temp-list-line)
                                            (propertize regex
                                                        'face 'traverse-regex-face)
                                            (nth 1 temp-list-line))))
          (insert (concat " "
                          (propertize (int-to-string (+ (first i) 1))
                                      'face 'traverse-match-face)
                          ":"
                          (if (> (length line-to-print-hight)
                                 lline)
                              (substring line-to-print-hight
                                         0
                                         lline)
                              line-to-print-hight)
                          "\n")))))))

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

;;;###autoload
(defun traverse-find-in-file (fname regexp &optional full-path)
  "Traverse search regex in a single file"
  (interactive "fFileName: \nsRegexp: ")
  (traverse-prepare-buffer)
  (let ((prefarg (not (null current-prefix-arg))))
    (if (and (not (bufferp fname))
             (file-regular-p fname)
             (not (file-symlink-p fname)))
        (traverse-file-process regexp fname prefarg)
        (traverse-file-process regexp fname prefarg :fn 'traverse-hash-readlines-from-buffer))
    (goto-char (point-min))
    (when (re-search-forward "^Wait")
      (beginning-of-line)
      (delete-region (point) (line-end-position))
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
(defun traverse-occur-current-buffer (regexp)
  (interactive (list
                (read-from-minibuffer "Regexp: "
                                      nil nil nil nil (thing-at-point 'sexp))))
  (let ((buf-fname (buffer-file-name (current-buffer))))
    (if traverse-occur-use-miniwindow
        (progn
          (delete-other-windows)
          (split-window-horizontally traverse-miniwindow-width))
        (delete-other-windows)
        (split-window-vertically))
    (other-window 1)
    (if buf-fname
        (traverse-find-in-file buf-fname regexp)
        (traverse-find-in-file (current-buffer) regexp)))
  (switch-to-buffer-other-window "*traverse-lisp*"))
    
;;;###autoload
(defun traverse-deep-rfind (tree regexp &optional only)
  "Main function that call walk, if only is omitted it
will be set as nil and search will be proceeded on all files
except on files that are in `traverse-ignore-files'
Called with prefix-argument (C-u) absolute path is displayed"
  (interactive "DTree: \nsRegexp: \nsCheckOnly: ")
  (save-excursion
    (traverse-prepare-buffer)
    (let ((init-time (cadr (current-time)))
          (only-list (split-string only)))
      (unwind-protect
           (traverse-walk-directory tree
                                    #'(lambda (y)
                                        (let ((prefarg (not (null current-prefix-arg))))
                                          (if only-list
                                              (when (member (file-name-extension y t) only-list)
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
                                    (unless only-list
                                      traverse-ignore-files)
                                    traverse-ignore-dirs)
        (setq traverse-count-occurences (if (< traverse-count-occurences 0)
                                            0
                                            traverse-count-occurences))
        (if (eq traverse-count-occurences 0)
            (progn
              (goto-char (point-min))
              (when (re-search-forward "^Wait")
                (beginning-of-line)
                (delete-region (point) (line-end-position))
                (insert "Oh!No! Nothing found!")))
            (goto-char (point-min))
            (when (re-search-forward "^Wait")
              (beginning-of-line)
              (delete-region (point) (line-end-position))
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
;;;###autoload
(defun traverse-search-in-dired-dir-at-point (regex &optional only)
  "Launch `traverse-deep-rfind' from `dired-mode'"
  (interactive "sRegexp: \nsCheckOnly: ")
  (if (eq major-mode 'dired-mode)
      (let ((tree (dired-get-filename)))
        (if (file-directory-p tree)
            (traverse-deep-rfind tree regex only)
            (message "Sorry! %s is not a Directory" tree)))
      (message "Hoops! We are not in Dired!")))

;;;###autoload
(defun traverse-search-in-dired-file-at-point (regex)
  (interactive "sRegexp: ")
  (if (eq major-mode 'dired-mode)
      (let ((fname (dired-get-filename)))
        (if (file-regular-p fname)
            (traverse-find-in-file fname regex)
            (message "Sorry! %s is not a regular file" fname)))
      (message "Hoops! We are not in Dired!")))

;;;###autoload
(defun traverse-dired-browse-archive ()
  "This function use AVFS and FUSE, so be sure
to have these programs and modules installed on your system"
  (interactive)
  (when traverse-use-avfs
    (let ((file-at-point (dired-get-filename)))
      (if (file-compressed-p file-at-point)
          (progn
            (when (not (cddr (directory-files traverse-avfs-default-directory)))
              (shell-command "mountavfs"))
            (find-file (concat traverse-avfs-default-directory file-at-point "#")))
          (find-file file-at-point)))))

;;;###autoload
(defun traverse-dired-search-in-archive (regexp &optional only)
  "This function use AVFS and FUSE, so be sure
to have these programs installed on your system and FUSE module
enabled in your kernel.
This function is disabled by default, enable it setting
traverse-use-avfs to non--nil"
  (interactive "sRegexp: \nsCheckOnly: ")
  (when traverse-use-avfs
    (let ((file-at-point (dired-get-filename)))
      (if (file-compressed-p file-at-point)          
          (progn
            (when (not (cddr (directory-files traverse-avfs-default-directory)))
              (shell-command "mountavfs"))
            (traverse-deep-rfind (concat traverse-avfs-default-directory file-at-point "#")
                                 regexp
                                 only))
          (message "That's not a compressed file")))))

;;;###autoload
(defun traverse-dired-find-in-marked-files (regexp &optional full-path)
  "Traverse search regex in marked files
if some of the marked files are directories ignore them
if no marked files use file at point"
  (interactive "sRegexp: ")
  (let ((prefarg (not (null current-prefix-arg)))
        (fname-list (traverse-dired-get-marked-files)))
    (traverse-prepare-buffer)
    (dolist (i fname-list)
      (traverse-file-process regexp i prefarg))
    (goto-char (point-min))
    (when (re-search-forward "^Wait")
      (beginning-of-line)
      (delete-region (point) (line-end-position))
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

(defun traverse-dired-find-in-all-files (regexp &optional full-path)
  "Traverse search regex in all files of current dired buffer
except compressed files and symlinks"
  (interactive "sRegexp: ")
  (let ((prefarg (not (null current-prefix-arg)))
        (all-files (traverse-list-directory (dired-current-directory))))
    (traverse-prepare-buffer)
    (dolist (i all-files)
      (when (and (file-regular-p i)
                 (not (file-symlink-p i))
                 (not (file-compressed-p i))
                 (not (member (file-name-extension i t)
                              traverse-ignore-files))
                 (not (member (file-name-nondirectory i)
                              traverse-ignore-files)))
        (traverse-file-process regexp i prefarg)))
    (goto-char (point-min))
    (when (re-search-forward "^Wait")
      (beginning-of-line)
      (delete-region (point) (line-end-position))
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

(defun traverse-dired-get-marked-files ()
  "Get a list of all marked files for traverse"
  (let ((fname-list nil))
    (dolist (i (dired-get-marked-files))
      (when (and (not (file-directory-p i))
                 (not (file-compressed-p i)))
        (push i fname-list)))
    fname-list))

(defun traverse-dired-has-marked-files ()
  "Check if dired has marked files for traverse:
not compressed
not directory"
  (let ((fm-list (traverse-dired-get-marked-files)))
    (if fm-list
        t
        nil)))

;;;###autoload
(defun traverse-dired-search-regexp-in-anything-at-point (regexp &optional only)
  "Generic function for dired
Search in:
file at point
or
marked files
or
directory at point (recursion)
or
in compressed archive at point if traverse-use-avfs is non--nil"
  (interactive
   (let ((f-or-d-name (dired-get-filename)))
     (cond ((traverse-dired-has-marked-files)
            (list (read-string "Regexp: ")))
           ((or (file-directory-p f-or-d-name)
                (and (file-regular-p f-or-d-name)
                     (file-compressed-p f-or-d-name)))
            (list (read-string "Regexp: ")
                  (read-string "CheckOnly: "))))))
  (let ((fname (dired-get-filename)))
     (cond ((traverse-dired-has-marked-files)
            (traverse-dired-find-in-marked-files regexp))
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


(defun traverse-window-split-h-or-t ()
  "Give current split window position under
symbol form.
Possible value: 'hor or 'ver"
  (cdr (assoc 'dir (bw-get-tree))))

;;;###autoload
(defun traverse-toggle-split-window-h-v ()
  "From traverse buffer toggle split window
horizontally or vertically ala ediff"
  (interactive)
  (when (eq (count-windows) 2)
    (balance-windows)
    (let ((buffA (current-buffer))
          (buffB)
          (split-pos (traverse-window-split-h-or-t)))
      (save-excursion
        (other-window 1)
        (setq buffB (current-buffer))
        (delete-window))
      (if (eq split-pos 'hor)
          (split-window-vertically)
          (split-window-horizontally))
      (set-buffer (get-buffer buffB))
      (display-buffer (current-buffer)))))

(provide 'traverselisp)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (emacswiki-post "traverselisp.el")
;;; traverselisp.el ends here


