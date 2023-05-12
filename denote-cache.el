;;; denote-cache.el --- ONE LINE DESCRIPTION HERE -*- lexical-binding: t -*-

;; Copyright (C) 2023  COPYRIGHT HOLDER HERE

;; Author: AUTHOR NAME HERE relict007 <utils+sr.ht@kotlak.com>
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
(defcustom denote-cache-extra-processing-function (lambda (file)) "Extra processing of files.")

(defvar denote-cache--cache (make-hash-table :test 'equal) "info cache.")
(defvar denote-cache--links-cache '() "links cache")
(defvar denote-cache--performance-hack-all-files nil "temporary list of all denote files.")
(defvar denote-cache--performance-hack-all-text-files nil "temporary list of all denote text files.")


(defvar denote-cache-post-cache-update-hook nil
  "Hook for cache updates")

(defun denote-cache--is-denote-file (file)
  ;;(or (denote-file-is-note-p file) (denote-file-has-identifier-p file))
  (or (denote-file-is-note-p file) (and (denote-file-has-identifier-p file) (denote-get-file-name-relative-to-denote-directory file)))
  )

(defun denote-cache--run-post-cache-update-hook ()
  "Run the post cache save hooks"
  (run-hooks 'denote-cache-post-cache-update-hook)
  )

(defun denote-cache--retrieve-backlinks (file)
  "Retrieve backlinks using denote native apis.  No cache."
  (condition-case err
      (denote-link-return-backlinks file)
    (user-error '())))

(defun denote-cache--retrieve-forwardlinks (file)
  "Retrieve forward links, no cache."
  ;;TODO this is a temprary hack to speedup indexing
  (if (equal (file-name-extension file) "org")
      (condition-case err
          (denote-link-return-links file)
        (user-error '()))
    '()))

(defun denote-cache--handle-file-add (file)
  "Handle event of FILE being added."
  (when (denote-cache--is-denote-file file)
    (message (concat "adding file " file))
    (denote-cache--add-file-in-cache file)
    (denote-cache--run-post-cache-update-hook)
    ))

(defun denote-cache--handle-file-delete (file)
  "Handle event of FILE being deleted"
  (message (concat "delete " file))
  
  (when (denote-cache--is-denote-file file)
        (message (concat "actuauly delete " file))
        (denote-cache--delete-file-from-cache file)
        (denote-cache--run-post-cache-update-hook)))

;; FIXME 2023-05-08: Document OLD-FILE NEW-FILE-OR-DIR
(defun denote-cache--post-rename-file-hook (old-file new-file-or-dir &rest _args)
  "Hook to run after a file is renamed."
  (message (concat "post rename " new-file-or-dir))
  (denote-cache--handle-file-add new-file-or-dir)
  )

;; FIXME 2023-05-08: Document OLD-FILE NEW-FILE-OR-DIR
(defun denote-cache--pre-rename-file-hook (old-file new-file-or-dir &rest _args)
  "Hook to run before a file is renamed."
  (message (concat "pre rename " new-file-or-dir))
  (denote-cache--handle-file-delete old-file)
  )

(defun denote-cache--delete-file-hook (file &optional _trash)
  "Hook to run after FILE is deleted."
  (denote-cache--handle-file-delete file))

;;;###autoload
(define-minor-mode denote-cache-autosync-mode
  "Denote cache autosync mode."
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
      (advice-add #'vc-delete-file :before #'denote-cache--delete-file-hook)
      )
     (t
      (advice-remove #'rename-file #'denote-cache--pre-rename-file-hook)
      (advice-remove #'rename-file #'denote-cache--post-rename-file-hook)
      (advice-remove #'vc-rename-file #'denote-cache--pre-rename-file-hook)
      (advice-remove #'vc-rename-file #'denote-cache--post-rename-file-hook)
      (advice-remove #'dired-rename-file #'denote-cache--pre-rename-file-hook)
      (advice-remove #'dired-rename-file #'denote-cache--post-rename-file-hook)
      (advice-remove #'delete-file #'denote-cache--delete-file-hook)
      (advice-remove #'vc-delete-file #'denote-cache--delete-file-hook)
      ))))

(defun denote-cache--retrieve-info (file)
  "Retrieve all the info about FILE (no cache)."
  (let*(
        (filetype (denote-filetype-heuristics file))
        (title (if (eq filetype 'org) (or (denote-retrieve-title-value file filetype) (denote-retrieve-filename-title file)) (denote-retrieve-filename-title file)))
        (ftime (file-attribute-modification-time (file-attributes file)))
        (keywords (denote-extract-keywords-from-path file))
        (keywords-sorted (if (null keywords) keywords (denote-keywords-sort keywords)))
        (extension (downcase (file-name-extension file)))
        (info (make-hash-table :test 'equal))
        (extra (funcall denote-cache-extra-processing-function file))
        )
    (puthash "title" title info)
    (puthash "ftime" ftime info)
    (puthash "filetype" filetype info)
    (puthash "keywords" keywords-sorted info)
    (puthash "extension" extension info)
    (when extra
      (dolist (e extra)
        (let* ((key (car e))
               (value (cdr e)))
          ;; (message (concat "adding " key ", value " value))
          (puthash key value info))))
    info))

;; FIXME 2023-05-08: Add missing doc string.  Document FILE.
(defun denote-cache--add-links (file)
  ;; (message (concat "adding links " file))
  ;; (message (concat "links " (prin1-to-string denote-cache--links-cache)))
  (let* ((backlinks (denote-cache--retrieve-backlinks file))
         (forwardlinks (denote-cache--retrieve-forwardlinks file)))
    (dolist (link backlinks)
      (add-to-list 'denote-cache--links-cache (cons link file)))
    (dolist (link forwardlinks)
      (add-to-list 'denote-cache--links-cache (cons file link)))))

;; FIXME 2023-05-08: Add missing doc string.  Document FILE.
(defun denote-cache--delete-links (file)
  ;; (message (concat "delete links " file))
  (setq denote-cache--links-cache
        (cl-delete-if (lambda (pair)
                        (or (equal (car pair) file) (equal (cdr pair) file)))
                      denote-cache--links-cache)))

;; FIXME 2023-05-08: Add missing doc string.  Document FILE.
(defun denote-cache--update-links (file)
  ;; (message (concat "update links " file))  
  (denote-cache--delete-links file)
  (denote-cache--add-links file)  
  )

(defun denote-cache--add-file-in-cache (file)
  "Add FILE in the cache while retrieving its info."
  (puthash file (denote-cache--retrieve-info file) denote-cache--cache)
  (denote-cache--add-links file)
  )

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
  "Hook to run after org-capture finalize."
  (denote-cache-update-cache))

(defun denote-cache--after-save-hook ()
  "Hook to run after a file is saved"
  ;; (message "save hook")
  (when-let* (
              (file (buffer-file-name))
              ;;((message (concat "file" (denote-cache--is-denote-file file))))
              (isnote (denote-cache--is-denote-file file)) 
              )
    (message (concat "after save hook " file))
    (denote-cache--update-file-in-cache file)
    (denote-cache--run-post-cache-update-hook)))

(add-hook 'after-save-hook #'denote-cache--after-save-hook)
(add-hook 'org-capture-after-finalize-hook #'denote-cache--org-capture-after-finalize-hook)

(defun denote-cache--performance-all-files-wrapper ()
  denote-cache--performance-hack-all-files
  )

(defun denote-cache--performance-all-text-files-wrapper ()
  denote-cache--performance-hack-all-text-files
  )
  
(defun denote-cache-rebuild-cache()
  "Rebuild cache."
  (interactive)
  (clrhash denote-cache--cache)
  (denote-cache-update-cache)
  )

(defun denote-cache-update-cache()
  "Update all the cache."
  (interactive)
  (setq denote-cache--performance-hack-all-files (denote-directory-files))
  (setq denote-cache--performance-hack-all-text-files (denote-directory-text-only-files))
  (advice-add #'denote-directory-files :override  #'denote-cache--performance-all-files-wrapper)
  (advice-add #'denote-directory-text-only-files :override  #'denote-cache--performance-all-text-files-wrapper)
  (let* (
         ;;(text-files (denote-directory-text-only-files))
         (files denote-cache--performance-hack-all-files)
         (cached-files (denote-cache-get-all-files-from-cache))
         (new-files (denote-cache--utils-remove-matching-items files cached-files))
         (deleted-files (denote-cache--utils-remove-matching-items cached-files files))
         (common-files (denote-cache--util-common-items files cached-files))
         (updated-files (cl-remove-if (lambda (f)
                                        (let*(
                                              (ftime (file-attribute-modification-time (file-attributes f)))
                                              (ftime-cached (denote-cache-get-ftime f))
                                              )
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
    (message "update done")
    )
  (advice-remove #'denote-directory-files #'denote-cache--performance-all-files-wrapper)
  (advice-remove #'denote-directory-text-only-files #'denote-cache--performance-all-text-files-wrapper)
  (setq denote-cache--performance-hack-all-files nil)
  (setq denote-cache--performance-hack-all-text-files nil)
  )

;; FIXME 2023-05-08: Add missing doc string.  Document LIST, ANOTHER-LIST.
(defun denote-cache--util-common-items (list another-list)
    (cl-intersection list another-list :test #'equal))

(defun denote-cache--utils-remove-matching-items (list items-to-remove)
  "Remove all items from LIST that are also present in ITEMS-TO-REMOVE."
  ;;(cl-remove-if (lambda (item) (memq item items-to-remove)) list))
  (cl-set-difference list items-to-remove :test #'equal))

(defun denote-cache-get-all-files-from-cache ()
  "Get all denote note files."
  (hash-table-keys denote-cache--cache))

;; FIXME 2023-05-08: Add missing doc string.  Document FILE.
(defun denote-cache--get-file-info (file)
  (gethash file denote-cache--cache))

;; FIXME 2023-05-08: Add missing doc string.  Document FILE.
(defun denote-cache-get-title (file)
  (gethash "title" (denote-cache--get-file-info file)))

;; FIXME 2023-05-08: Add missing doc string.  Document FILE.
(defun denote-cache-get-ftime (file)
  (gethash "ftime" (denote-cache--get-file-info file)))

;; FIXME 2023-05-08: Add missing doc string.  Document FILE.
(defun denote-cache-get-filetype (file)
  (gethash "filetype" (denote-cache--get-file-info file)))

;; FIXME 2023-05-08: Add missing doc string.  Document FILE.
(defun denote-cache-get-keywords (file)
  (gethash "keywords" (denote-cache--get-file-info file)))

;; FIXME 2023-05-08: Add missing doc string.  Document FILE.
(defun denote-cache-get-extension (file)
  (gethash "extension" (denote-cache--get-file-info file)))

;; FIXME 2023-05-08: Add missing doc string.  Document FILE.
(defun denote-cache-get-backlinks (file)
  (setq links (cl-remove-if-not
               (lambda (pair)
                 (equal (cdr pair) file))
               denote-cache--links-cache))
  (mapcar
   (lambda (pair)
     (car pair))
   links))

;; FIXME 2023-05-08: Add missing doc string.  Document FILE.
(defun denote-cache-get-forwardlinks (file)
  (setq links (cl-remove-if-not
               (lambda (pair)
                 (equal (car pair) file))
               denote-cache--links-cache))
  (mapcar
   (lambda (pair)
     (cdr pair))
   links))

(defun denote-cache-get-value (file key)
  "Get value for KEY pair associated with FILE."
  (if-let ((info (denote-cache--get-file-info file)))
      (gethash key info)
    (message (concat "file " file " not found in cache"))))

(provide 'denote-cache)
;;; denote-cache.el ends here
