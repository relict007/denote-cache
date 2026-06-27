;;; denote-cache.el --- ONE LINE DESCRIPTION HERE -*- lexical-binding: t -*-

;; Copyright (C) 2023  COPYRIGHT HOLDER HERE

;; Author: AUTHOR  relict007 <utils+sr.ht@kotlak.com>
;; Maintainer: AUTHOR NAME HERE relict007 <utils+sr.ht@kotlak.com>
;; URL: https://git.sr.ht/~relict007/denote-cache
;; Mailing-List: MAILING LIST URL HERE
;; Version: 0.0.0
;; Package-Requires: ((emacs "28.1") (denote "1.2.0"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; YOUR DOCUMENTATION HERE.

;;; Code:

(require 'denote)
(require 'xref)
(require 'cl-lib)
(require 'cl-seq)


;; FIXME 2023-05-08: A `defcustom' must have a `:type'.  Evaluate:
;; (info "(elisp) Customization Types")
;;
;; FIXME 2023-05-08: Instead of a lambda, it is better to define a
;; named function and document it.  This makes it easier for people to
;; test your code.

(defconst denote-cache--key-id "denote-cache--key-id"
  "Key that we use for a note\'s id in the hash table")

(defconst denote-cache--key-title "denote-cache--key-title"
  "Key that we use for a note\'s title in the hash table")

(defconst denote-cache--key-ftime "denote-cache--key-ftime"
  "Key that we use for a note\'s ftime in the hash table")

(defconst denote-cache--key-ctime "denote-cache--key-ctime"
  "Key that we use for a note\'s creation time in the hash table")

(defconst denote-cache--key-extension "denote-cache--key-extension"
  "Key that we use for a note\'s file extension in the hash table")

(defconst denote-cache--key-keywords "denote-cache--key-keywords"
  "Key that we use for a note\'s keywords in the hash table")

(defconst denote-cache--key-relative-path "denote-cache--key-relative-path"
  "Key that we use for a note\'s denote-directory relative path in the hash table")

(defun denote-cache-extra-processing-function-default (file)
  "Default extra processing function.
This is called with the Denote FILE which is being added to the
cache.  This function is supposed to return an alist with
key/value pairs.  Key must be an string, value can be any elisp
object.  The function `denote-cache-get-value' can then be used to
retrieve the saved value for the key FILE is the complete path of
the Denote note."
  nil)

(defcustom denote-cache-extra-processing-function #'denote-cache-extra-processing-function-default
  "User customizable function for saving custom data in cache.
key/value data related to a Denote file in the cache.  This
function is called at the time a Denote file is being added to
the cache. Users can set this to a custom function which will be
called with the denote FILE as the argument.  This custom function
is supposed to return an alist with key/value pairs which will be
added to the cache.

FILE is the complete path of the Denote note.  This function is
called for all Denote files, including binary/non-text
files. Don\'t assume that it will only be a txt/org/md etc file.

 Also see
`denote-cache-extra-processing-function-default'"
  :type 'function)

(defvar denote-cache--cache (make-hash-table :test 'equal) "info cache.")
(defvar denote-cache--links-cache '() "links cache")
(defvar denote-cache--performance-hack-all-files nil "temporary list of all denote files.")
(defvar denote-cache--performance-hack-all-text-files nil "temporary list of all denote text files.")


(defvar denote-cache-post-cache-update-hook nil
  "Hook for cache updates")

(defun denote-cache--is-denote-file (file)
  "Return non-nil if FILE is a Denote file.
A file qualifies if it is recognized as a Denote note by
`denote-file-is-note-p', or if it has a Denote identifier and a
path relative to `denote-directory'."
  (or (denote-file-is-note-p file) (and (denote-file-has-identifier-p file) (denote-get-file-name-relative-to-denote-directory file))))

(defun denote-cache--run-post-cache-update-hook ()
  "Run the post cache save hooks"
  (run-hooks 'denote-cache-post-cache-update-hook))

(defun denote-cache--retrieve-backlinks (file)
  "Retrieve backlinks for FILE using denote native apis.
Does not use cache."
  (denote-link-return-backlinks file))

(defun denote-cache--retrieve-forwardlinks (file)
  "Retrieve forward links for FILE using denote native apis.
Does not use cache."
  ;;TODO this is a temprary hack to speedup indexing
  (when (denote-file-has-supported-extension-p file)
        (denote-get-links file)))

(defun denote-cache--handle-file-add (file)
  "Handle event of FILE being added."
  (when (denote-cache--is-denote-file file)
    (message (concat "adding file " file))
    (denote-cache--add-file-in-cache file)
    (denote-cache--run-post-cache-update-hook)))

(defun denote-cache--handle-file-delete (file)
  "Handle event of FILE being deleted"
  (when (denote-cache--is-denote-file file)
        (denote-cache--delete-file-from-cache file)
        (denote-cache--run-post-cache-update-hook)))

(defun denote-cache--post-rename-file-hook (old-file new-file-or-dir &rest _args)
  "Hook to run after a file is renamed.
OLD-FILE is the original file path before renaming.
NEW-FILE-OR-DIR is the new file path or destination directory after renaming."
  (message (concat "post rename " new-file-or-dir))
  (denote-cache--handle-file-add new-file-or-dir))

(defun denote-cache--pre-rename-file-hook (old-file new-file-or-dir &rest _args)
  "Hook to run before a file is renamed.
OLD-FILE is the original file path that will be renamed.
NEW-FILE-OR-DIR is the intended new file path or destination directory."
  (message (concat "pre rename " new-file-or-dir))
  (denote-cache--handle-file-delete old-file))

(defun denote-cache--delete-file-hook (file &optional _trash)
  "Hook to run after FILE is deleted."
  (denote-cache--handle-file-delete file))

(defun denote-cache--get-file-creation-time (file)
  "Get the file creation time on Linux using stat, return it as a Lisp timestamp."
  (let ((output (shell-command-to-string (concat "stat --format='%W' " (shell-quote-argument file)))))
    (if (string-match "\\([0-9]+\\)" output)
        (let ((timestamp (string-to-number (match-string 1 output))))
          (seconds-to-time timestamp))
      nil)))

;;;###autoload
(define-minor-mode denote-cache-autosync-mode
  "Enable autosync mode where denote-cache watches for file updates
within emacs and automatically refreshes the cache. There still
can be certain scenarios where it misses the file update, in that
case run `denote-cache-update-cache'"
  ;;:group 'denote-cache
  :global t
  :init-value nil
  (let ((enabled denote-cache-autosync-mode))
    (cond
     (enabled
      (advice-add #'rename-file :before  #'denote-cache--pre-rename-file-hook)
      (advice-add #'rename-file :after  #'denote-cache--post-rename-file-hook)
      (advice-add #'vc-rename-file :before  #'denote-cache--pre-rename-file-hook)
      (advice-add #'vc-rename-file :after  #'denote-cache--post-rename-file-hook)
      (advice-add #'dired-rename-file :before  #'denote-cache--pre-rename-file-hook)
      (advice-add #'dired-rename-file :after  #'denote-cache--post-rename-file-hook)
      (advice-add #'delete-file :before #'denote-cache--delete-file-hook)
      (advice-add #'vc-delete-file :before #'denote-cache--delete-file-hook))
     (t
      (advice-remove #'rename-file #'denote-cache--pre-rename-file-hook)
      (advice-remove #'rename-file #'denote-cache--post-rename-file-hook)
      (advice-remove #'vc-rename-file #'denote-cache--pre-rename-file-hook)
      (advice-remove #'vc-rename-file #'denote-cache--post-rename-file-hook)
      (advice-remove #'dired-rename-file #'denote-cache--pre-rename-file-hook)
      (advice-remove #'dired-rename-file #'denote-cache--post-rename-file-hook)
      (advice-remove #'delete-file #'denote-cache--delete-file-hook)
      (advice-remove #'vc-delete-file #'denote-cache--delete-file-hook)))))

(defun denote-cache--retrieve-info (file)
  "Retrieve all the info about FILE (no cache)."
  (let* ((filetype (denote-filetype-heuristics file))
         (note-id (denote-retrieve-filename-identifier file))
         (title (denote-retrieve-title-or-filename file filetype))
         (file-attrs (file-attributes file))
         (ftime (file-attribute-modification-time file-attrs))
         (ctime (denote-cache--get-file-creation-time file))
         (keywords (denote-extract-keywords-from-path file))
         (keywords-sorted (if (null keywords) keywords (denote-keywords-sort keywords)))
         (extension (downcase (file-name-extension file)))
         (relative-path (file-name-directory (file-relative-name file denote-directory)))
         (relative-path-without-trailing-slash (when relative-path (substring relative-path 0 -1)))
         (info (make-hash-table :test 'equal))
         (extra (funcall denote-cache-extra-processing-function file)))
    (puthash denote-cache--key-id note-id info)
    (puthash denote-cache--key-title title info)
    (puthash denote-cache--key-ftime ftime info)
    (puthash denote-cache--key-ctime ctime info)
    (puthash denote-cache--key-keywords keywords-sorted info)
    (puthash denote-cache--key-extension extension info)
    (when relative-path-without-trailing-slash
      (puthash denote-cache--key-relative-path relative-path-without-trailing-slash info))
    
    (when extra
      (dolist (e extra)
        (let* ((key (car e))
               (value (cdr e)))          
          ;; (message (concat "adding " key ", value " value))
          (puthash key value info))))
    info))

(defun denote-cache--add-links (file)
  "Add forward links from FILE to `denote-cache--links-cache'.
Each link is stored as a cons cell (FILE . TARGET) where TARGET is
a file that FILE links to."
  (let* ((forwardlinks (denote-cache--retrieve-forwardlinks file)))
    (dolist (link forwardlinks)
      (add-to-list 'denote-cache--links-cache (cons file link)))))

(defun denote-cache--delete-links (file)
  "Remove all link entries involving FILE from `denote-cache--links-cache'.
Removes both forward links where FILE is the source and backlinks
where FILE is the target."
  (setq denote-cache--links-cache
        (cl-delete-if (lambda (pair)
                        (or (equal (car pair) file) (equal (cdr pair) file)))
                      denote-cache--links-cache)))

(defun denote-cache--update-links (file)
  "Update link entries for FILE in `denote-cache--links-cache'.
Deletes existing link entries for FILE and re-adds them by
re-scanning FILE's forward links."
  (denote-cache--delete-links file)
  (denote-cache--add-links file))

(defun denote-cache--add-file-in-cache (file)
  "Add FILE in the cache while retrieving its info."
  (puthash file (denote-cache--retrieve-info file) denote-cache--cache)
  (denote-cache--add-links file))

(defun denote-cache--delete-file-from-cache (file)
  "Delete FILE from cache while updating forward and backlinks."
  (remhash file denote-cache--cache)
  (denote-cache--delete-links file))

(defun denote-cache--update-file-in-cache (file)
  "Update FILE in cache."
  (message (concat "updated file " file))
  (puthash file (denote-cache--retrieve-info file) denote-cache--cache)
  (denote-cache--update-links file))

(defun denote-cache--org-capture-after-finalize-hook ()
  "Hook to run after `org-capture' finalize."
  (denote-cache-update-cache))

(defun denote-cache--after-save-hook ()
  "Hook to run after a file is saved"
  ;; (message "save hook")
  (when-let* ((file (buffer-file-name))
              ;;((message (concat "file" (denote-cache--is-denote-file file))))
              (isnote (denote-cache--is-denote-file file)))
    (message (concat "after save hook " file))
    (denote-cache--update-file-in-cache file)
    (denote-cache--run-post-cache-update-hook)))

(add-hook 'after-save-hook #'denote-cache--after-save-hook)
(add-hook 'org-capture-after-finalize-hook #'denote-cache--org-capture-after-finalize-hook)

(defun denote-cache--performance-all-files-wrapper (&optional files-matching-regexp omit-current text-only exclude-regexp has-identifier)
  "Return the pre-fetched list of all Denote files.
This overrides `denote-directory-files' during cache updates to
avoid redundant filesystem scans.  Arguments FILES-MATCHING-REGEXP,
OMIT-CURRENT, TEXT-ONLY, EXCLUDE-REGEXP, and HAS-IDENTIFIER are
accepted for signature compatibility but ignored."
  denote-cache--performance-hack-all-files)

(defun denote-cache--performance-all-text-files-wrapper ()
  "Return the pre-fetched list of all Denote text files.
This overrides `denote-directory-text-only-files' during cache
updates to avoid redundant filesystem scans."
  denote-cache--performance-hack-all-text-files)
  
(defun denote-cache-rebuild-cache()
  "Rebuild the whole cache.
It removes all previous cached data and builds the cache again.
See also `denote-cache-update-cache'"
  (interactive)
  (clrhash denote-cache--cache)
  (denote-cache-update-cache))

(defun denote-cache-update-cache()
  "Update all the cache. This finds out added, modified or deleted
files from the present cache state and appropriately updates the
cache with the new data.
See also `denote-cache-rebuild-cache'"
  (interactive)
  (setq denote-cache--performance-hack-all-files (denote-directory-files))
  (setq denote-cache--performance-hack-all-text-files (denote-directory-files nil nil t))
  (advice-add #'denote-directory-files :override  #'denote-cache--performance-all-files-wrapper)
  (advice-add #'denote-directory-text-only-files :override  #'denote-cache--performance-all-text-files-wrapper)
  (let* ((files denote-cache--performance-hack-all-files)
         (cached-files (denote-cache-get-all-files-from-cache))
         (new-files (denote-cache--utils-remove-matching-items files cached-files))
         (deleted-files (denote-cache--utils-remove-matching-items cached-files files))
         (common-files (denote-cache--util-common-items files cached-files))
         (updated-files (cl-remove-if (lambda (f)
                                        (let*((ftime (file-attribute-modification-time (file-attributes f)))
                                              (ftime-cached (denote-cache-get-ftime f)))
                                          (equal ftime ftime-cached))) common-files)))
    (dolist (f updated-files)
      (message (concat "updated: " f))
      (denote-cache--update-file-in-cache f))

    (dolist (f new-files)
      (denote-cache--add-file-in-cache f)
      (message (concat "added: " f)))

    (dolist (f deleted-files)
      (message (concat "deleted: " f))
      (denote-cache--delete-file-from-cache f))

    (denote-cache--run-post-cache-update-hook)
    (message "update done"))
  
  (advice-remove #'denote-directory-files #'denote-cache--performance-all-files-wrapper)
  (advice-remove #'denote-directory-text-only-files #'denote-cache--performance-all-text-files-wrapper)
  (setq denote-cache--performance-hack-all-files nil)
  (setq denote-cache--performance-hack-all-text-files nil))

(defun denote-cache--util-common-items (list another-list)
  "Return the items that appear in both LIST and ANOTHER-LIST.
Comparison is done with `equal'."
  (cl-intersection list another-list :test #'equal))

(defun denote-cache--utils-remove-matching-items (list items-to-remove)
  "Remove all items from LIST that are also present in ITEMS-TO-REMOVE."
  (cl-set-difference list items-to-remove :test #'equal))

(defun denote-cache-get-all-files-from-cache ()
  "Get all denote note files."
  (hash-table-keys denote-cache--cache))

(defun denote-cache--get-file-info (file)
  "Return the cached info hash table for FILE, or nil if not cached."
  (gethash file denote-cache--cache))

(defun denote-cache-get-id (file)
  "Return the Denote identifier for FILE from the cache."
  (gethash denote-cache--key-id (denote-cache--get-file-info file)))

(defun denote-cache-get-title (file)
  "Return the title of FILE from the cache."
  (gethash denote-cache--key-title (denote-cache--get-file-info file)))

(defun denote-cache-get-ftime (file)
  "Return the last modification time of FILE from the cache.
The value is a Lisp timestamp as returned by `file-attribute-modification-time'."
  (gethash denote-cache--key-ftime (denote-cache--get-file-info file)))

(defun denote-cache-get-ctime (file)
  "Return the creation time of FILE from the cache.
The value is a Lisp timestamp obtained via `stat' on Linux."
  (gethash denote-cache--key-ctime (denote-cache--get-file-info file)))

(defun denote-cache-get-keywords (file)
  "Return the sorted list of keywords for FILE from the cache."
  (gethash denote-cache--key-keywords (denote-cache--get-file-info file)))

(defun denote-cache-get-extension (file)
  "Return the lowercased file extension for FILE from the cache."
  (gethash denote-cache--key-extension (denote-cache--get-file-info file)))

(defun denote-cache-get-relative-path (file)
  "Returns relative path of the FILE with respect to
`denote-directory' For example, if FILE is
\'/home/user/notes/denote/books/scifi/20210908T123157--prelude-to-foundation__finished_book.org\'
and `denote-directory' is \'/home/user/notes/denote/\', this
function will return \'books/scifi\'. If FILE is directly in
`denote-directory', this will return `nil'"
  (gethash denote-cache--key-relative-path (denote-cache--get-file-info file)))

(defun denote-cache-get-backlinks (file)
  "Return a list of files that link to FILE."
  (mapcar
   #'car
   (cl-remove-if-not
    (lambda (pair)
      (equal (cdr pair) file))
    denote-cache--links-cache)))

(defun denote-cache-get-forwardlinks (file)
  "Return a list of files that FILE links to."
  (mapcar
   #'cdr
   (cl-remove-if-not
    (lambda (pair)
      (equal (car pair) file))
    denote-cache--links-cache)))


(defun denote-cache-get-value (file key)
  "Get value for KEY associated with FILE from the cache that was
saved using `denote-cache-extra-processing-function'"
  (when-let ((info (denote-cache--get-file-info file)))
      (gethash key info)))

(provide 'denote-cache)
;;; denote-cache.el ends here
