(require 'denote)
(require 'xref)
(require 'cl-lib)
(require 'cl-seq)

(defvar denote-cache--cache (make-hash-table :test 'equal) "info cache")
(defvar denote-cache--links-cache '() "links cache")

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

(defun denote-cache--retrieve-backlinks (file text-files)
  "Retrieve backlinks using denote native apis. No cache"
  (setq identifier (denote-retrieve-filename-identifier file))
  (delete file (sort
   (delete-dups
    (mapcar #'xref-location-group
            (mapcar #'xref-match-item-location
                    (xref-matches-in-files identifier text-files))))
   #'string-lessp))
  )

(defun denote-cache--retrieve-forwardlinks (file)
  "Retrieve forward links, no cache"
  (let* (
         (file-type (denote-filetype-heuristics file))
         (regexp (denote--link-in-context-regexp file-type))
         (files (denote-link--expand-identifiers regexp)))
      (delete file files)
    )
  )

(defun denote-cache--handle-file-add (file)
  "Handle event of a file being added"
  (if (denote-cache--is-denote-file file)      
      (progn
        (message (concat "adding file " file))
        (denote-cache--add-file-in-cache file)
        (denote-cache--run-post-cache-update-hook))))

;; (defun denote-cache--handle-file-update (file)
;;   "Handle event of a file being updated"
;;   (message (concat "handle file update " file))
;;   (if (denote-cache--is-denote-file file)
;;       (progn
;;         (message (concat "actually updating file " file))
;;         (denote-cache--update-file-in-cache file)
;;         (denote-cache--run-post-cache-update-hook))))

(defun denote-cache--handle-file-delete (file)
  "Handle event of a file being deleted"
  (message (concat "delete " file))
  (if (denote-cache--is-denote-file file)
      (progn
        (message (concat "actuauly delete " file))
        (denote-cache--delete-file-from-cache file)
        (denote-cache--run-post-cache-update-hook))))

(defun denote-cache--post-rename-file-hook (old-file new-file-or-dir &rest _args)
  "Hook to run after a file is renamed"
  (message (concat "post rename " new-file-or-dir))
  (denote-cache--handle-file-add new-file-or-dir)
  )

(defun denote-cache--pre-rename-file-hook (old-file new-file-or-dir &rest _args)
  "Hook to run before a file is renamed"
  (message (concat "pre rename " new-file-or-dir))
  (denote-cache--handle-file-delete old-file)
  )

(defun denote-cache--delete-file-hook (file &optional _trash)
  "Hook to run after a file is deleted"  
  (denote-cache--handle-file-delete file))

(define-minor-mode denote-cache-autosync-mode
  "denote cache autosync mode"
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

(defun denote-cache--retrieve-info (file &optional text-files)
  "Retrieve all the info about the file (no cache). Extra
  text-files paramter to speed up things"
  (let*(
        (filetype (denote-filetype-heuristics file))
        (title (if (eq filetype 'org) (or (denote-retrieve-title-value file filetype) (denote-retrieve-filename-title file)) (denote-retrieve-filename-title file)))
        (ftime (file-attribute-modification-time (file-attributes file)))
        (keywords (denote-extract-keywords-from-path file))
        (keywords-sorted (if (null keywords) keywords (denote-keywords-sort keywords)))
        (info (make-hash-table :test 'equal))
        )
    (puthash "title" title info)
    (puthash "ftime" ftime info)
    (puthash "filetype" filetype info)
    (puthash "keywords" keywords-sorted info)
    info
    )   
  )

(defun denote-cache--add-links (file)
  ;; (message (concat "adding links " file))
  ;; (message (concat "links " (prin1-to-string denote-cache--links-cache)))
  (let* ((backlinks (denote-cache--retrieve-backlinks file (denote-directory-text-only-files)))
         (forwardlinks (denote-cache--retrieve-forwardlinks file)))
    (dolist (link backlinks)
      (add-to-list 'denote-cache--links-cache (cons link file))      
      )
    (dolist (link forwardlinks)
      (add-to-list 'denote-cache--links-cache (cons file link))      
      )
    )
  )

(defun denote-cache--delete-links (file)
  ;; (message (concat "delete links " file))
  (setq denote-cache--links-cache
        (cl-delete-if (lambda (pair)
                        (or (equal (car pair) file) (equal (cdr pair) file)))
                      denote-cache--links-cache)))

(defun denote-cache--update-links (file)
  ;; (message (concat "update links " file))
  (denote-cache--delete-links file)
  (denote-cache--add-links file)  
  )

(defun denote-cache--add-file-in-cache(file &optional text-files)
  "Add file in the cache while retrieving its info"
  (puthash file (denote-cache--retrieve-info file text-files) denote-cache--cache)
  (denote-cache--add-links file)
  )

(defun denote-cache--delete-file-from-cache (file)
  "Delete file from cache while updating forward and backlinks"
  (remhash file denote-cache--cache)
  (denote-cache--delete-links file))

(defun denote-cache--update-file-in-cache (file &optional text-files)
  "Update file in cache"
  (message (concat "updated file " file))
  (puthash file (denote-cache--retrieve-info file text-files) denote-cache--cache)
  (denote-cache--update-links file))

(defun denote-cache--org-capture-after-finalize-hook ()
  "Hook to run after org-capture finalize"
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


(defun denote-cache-rebuild-cache()
  "Rebuild cache"
  (interactive)
  (setq denote-cache--cache nil)
  (denote-cache-update-cache)
  )

(defun denote-cache-update-cache()
  "Update all the cache"
  (interactive)
  (let* (
         (text-files (denote-directory-text-only-files))
         (files (denote-directory-files))
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
      (denote-cache--update-file-in-cache f text-files)
      )

    (dolist (f new-files)
      (message (concat "added: " f))
      (denote-cache--add-file-in-cache f text-files)
      )

    (dolist (f deleted-files)
      (message (concat "deleted: " f))
      (denote-cache--delete-file-from-cache f)
      )

    (denote-cache--run-post-cache-update-hook)
    (message "update done")
    )
  )


(defun denote-cache--util-common-items (list another-list)
    (cl-intersection list another-list :test #'equal))

(defun denote-cache--utils-remove-matching-items (list items-to-remove)
  "Remove all items from LIST that are also present in ITEMS-TO-REMOVE."
  ;;(cl-remove-if (lambda (item) (memq item items-to-remove)) list))
  (cl-set-difference list items-to-remove :test #'equal))

(defun denote-cache-get-all-files-from-cache ()
  "Get all denote note files"
  (hash-table-keys denote-cache--cache))

(defun denote-cache--get-file-info (file)
  (gethash file denote-cache--cache))

(defun denote-cache-get-title (file)
  (gethash "title" (denote-cache--get-file-info file)))

(defun denote-cache-get-ftime (file)
  (gethash "ftime" (denote-cache--get-file-info file)))

(defun denote-cache-get-filetype (file)
  (gethash "filetype" (denote-cache--get-file-info file)))

(defun denote-cache-get-keywords (file)
  (gethash "keywords" (denote-cache--get-file-info file)))

(defun denote-cache-get-backlinks (file)
  (setq links (cl-remove-if-not (lambda (pair)
                      (equal (cdr pair) file)
                      ) denote-cache--links-cache))
  (mapcar (lambda (pair)
            (car pair)
            ) links))

(defun denote-cache-get-forwardlinks (file)
  (setq links (cl-remove-if-not (lambda (pair)
                      (equal (car pair) file)
                      ) denote-cache--links-cache))
  (mapcar (lambda (pair)
            (cdr pair)
            ) links))


(provide 'denote-cache)
