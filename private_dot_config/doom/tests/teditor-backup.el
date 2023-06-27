;;; tests/tconfig.el -*- lexical-binding: t; -*-
(require 'org)

;; add to load path to make sure this can run in batch mode
(add-to-list 'load-path (expand-file-name
                         (file-name-concat
                          (file-name-directory (or load-file-name
                                                   (buffer-file-name)))
                          "..")))
(require 'editor-backup)

(defun with-temp-org-file (content func)
  "Create a temporary org file with CONTENT, then execute FUNC with the temp
   file path as an argument. Ensure the temporary file is deleted afterward."
  (let ((temp-file (make-temp-file "temp-org-file" nil ".org" content)))
    (unwind-protect
        (funcall func temp-file)
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(ert-deftest test-tetov/org-file-has-any-tag-p-w-single-toplevel-tag ()
  (with-temp-org-file
   "* TODO Test :crypt:\n** DONE SubTest"
   (lambda (temp-file)
     (progn
       (should (tetov/org-file-has-any-tag-p temp-file '("crypt")))))))

(ert-deftest test-tetov/org-file-has-any-tag-p-two-tags-toplevel ()
  (with-temp-org-file
   "* TODO Test :ATTACH:export:\n** DONE SubTest"
   (lambda (temp-file)
     (progn
       ;; Test that missing single tag is not matched
       (should-not (tetov/org-file-has-any-tag-p temp-file '("crypt")))
       ;; Test that two missing tags are not matched
       (should-not (tetov/org-file-has-any-tag-p temp-file '("crypt" "noexport")))
       ;; Test that single tag is found
       (should (tetov/org-file-has-any-tag-p temp-file '("export")))
       ;; Test that multiple matching tags matches
       (should (tetov/org-file-has-any-tag-p temp-file '("export" "ATTACH")))
       ;; Test that multiple tags matches where one tag is found in file
       (should (tetov/org-file-has-any-tag-p temp-file '("export" "crypt" "noexport")))))))

(ert-deftest test-tetov/org-file-has-any-tag-p-tag-in-parent-and-child ()
  (with-temp-org-file
   "* TODO Test :crypt:\n** DONE SubTest :ATTACH:"
   (lambda (temp-file)
     (progn
       ;; Test that multiple tags matches where tags are in parent and child tree
       (should (tetov/org-file-has-any-tag-p temp-file '("export" "crypt" "noexport")))
       ;; Test that multiple tags does not match where tags are in parent and child tree
       (should-not (tetov/org-file-has-any-tag-p temp-file '("noexport" "decrypt" "DETACH")))))))

(ert-deftest test-tetov/org-file-has-any-tag-p-tag-in-child ()
  (with-temp-org-file
   "* TODO Test\n** DONE SubTest :crypt:"
   (lambda (temp-file)
     (progn
       ;; Test that a file with tag in subtree matches
       (should (tetov/org-file-has-any-tag-p temp-file '("crypt")))
       ;; Test that a file with tag not in subtree doesn't match
       (should-not (tetov/org-file-has-any-tag-p temp-file '("ATTACH")))))))

(ert-deftest test-tetov/org-file-has-any-tag-p-tag-in-sibling ()
  "Test that a file with tag in second tree matches"
  (with-temp-org-file
   "* TODO Test\n* DONE SiblingTest :crypt:"
   (lambda (temp-file)
     (progn
       (should (tetov/org-file-has-any-tag-p temp-file '("crypt")))))))

(ert-deftest test-tetov/backup-each-save-filter-org-file-wo-tags ()
  "Test that a file with no crypt tag returns true"
  (with-temp-org-file
   "* TODO Test\n** DONE SubTest"
   (lambda (temp-file)
     (progn
       (should (tetov/backup-each-save-filter temp-file))))))

(ert-deftest test-tetov/backup-each-save-filter-org-file-w-crypt-tag ()
  "Test that a file with a crypt tag returns false"
  (with-temp-org-file
   "* TODO Test :crypt:\n** DONE SubTest :crypt:"
   (lambda (temp-file)
     (progn
       (should-not (tetov/backup-each-save-filter temp-file))))))

(ert-deftest test-tetov/backup-each-save-filter-ignored-filename-patterns ()
  (mapc (lambda (file-name)
          (progn
            (should-not (tetov/backup-each-save-filter file-name))))
        '("test.emacs-places" "semantic.cache")))

(provide 'teditor-backup)
