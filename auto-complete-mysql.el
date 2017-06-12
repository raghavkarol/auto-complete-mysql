;; Copyright 2016 Raghav Karol
;;
;; Author: raghav.karol@gmail.com
;; Version: 0.1
;; Keywords:
;; X-URL: not distributed yet

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'auto-complete-mysql)

;; (mysql-fetch-column-doc "user" "id")
;; (setq schema 'cloud)
;; (setq table-name 'device)
;; (setq column-name 'id)

;;; Code:

;;; MySql Functions
(defvar mysql-user "root")
(defvar mysql-password "root")
(defvar mysql-host "127.0.0.1")
(defvar mysql-error-regexp "^ERROR [0-9]+ (.+?): ")

(defun mysql-shell-query (sql)
  (let ((cmd (format "MYSQL_PWD=%s mysql -u %s -h %s -e \"%s\" "
                     mysql-password
                     mysql-user
                     mysql-host
                     sql)))
    (mysql-output-table (shell-command-to-string cmd))))

(defsubst mysql-output-table (output)
  (when output
    (if (string-match mysql-error-regexp output)
        (error "%s" output)
      (mapcar (lambda (str) (split-string str "\t"))
              (butlast (split-string output "\n"))))))

;;; Helper functions
(setq enable-debug nil)
(defun debug-print (msg)
  (when enable-debug
    (message msg)))

(defun to-sql-list (list)
  (mapconcat 'identity (mapcar (lambda (s) (format "'%s'" s)) list) ","))

;; Hastable test function
(defun equal-string= (a b)
  (eq t (compare-strings a nil nil b nil nil t)))

(defun equal-string-hash (a)
  (sxhash (upcase a)))

(define-hash-table-test 'equal-string
  'equal-string= 'equal-string-hash)

;; Taken from jedi.el
(defun completion-candidate (word summary)
  (popup-make-item word :summary summary))

;;; Find text that matches auto-completable prefixes in the buffer
(setq alias-to-table-alist ())
;; userdb.user_rdb u
;;        +- ac-point
(defun update-alias-table-alist(alias table)
  (cond ((string= table (cdr (assoc alias alias-to-table-alist))) nil)
        ('t (setq alias-to-table-alist (acons alias table alias-to-table-alist)))))

(defun alias-to-table-name (table)
  (let ((resolved-name (cdr (assoc table alias-to-table-alist))))
    (cond (resolved-name) ('t table))))

(defun completion-prefix-before-dot ()
  (setq prefix-start
        (save-excursion
          (re-search-backward "\\.")
          (backward-sexp)
          (point)))
  (setq prefix-end (- ac-point 1))
  (debug-print
   (format "completion-prefix-before-dot >>> %s %s"
           prefix-start
           prefix-end))
  (buffer-substring-no-properties prefix-start prefix-end))

(defun table-name ()
  (let ((table-alias (completion-prefix-before-dot)))
    (alias-to-table-name table-alias)))

(defun schema-name ()
  (completion-prefix-before-dot))

;;; MySql Fetch functions
(defun mysql-fetch (query)
  (mapcar (lambda(col) (car col))
          (cdr (mysql-shell-query query))))

(defun mysql-fetch-schemas ()
  (debug-print
   (format "mysql-fetch-schemas ac-prefix: %s " ac-prefix))
  (mysql-fetch "show schemas"))

(defun mysql-fetch-tables (schema)
  (debug-print (format "mysql-fetch-tables ac-prefix: %s schema: %s" ac-prefix schema))
  (mysql-fetch (format "select table_name
                  from information_schema.tables
                 where table_schema='%s'" schema)))

;; (mysql-fetch-functions "recurly")
(defun mysql-fetch-functions (schema)
  (debug-print (format "mysql-fetch-functions ac-prefix: %s schema: %s" ac-prefix schema))
  (mysql-fetch (format "select specific_name
                          from information_schema.routines
                         where routine_type = 'FUNCTION' and routine_schema = '%s'" schema)))

(defun mysql-fetch-column-doc (table-name column-name)
  (car (mysql-fetch
          (format
            "select CONCAT(column_type, ' ', table_name, ' ', table_schema)
               from information_schema.columns
              where table_name='%s' and column_name='%s'"
             table-name column-name))))

;; (progn (let ((schema-name "userdb")
;;              (table-name "user_rdb"))
;;          (mysql-fetch-table-doc schema-name table-name)))
(defun mysql-fetch-table-doc (schema-name table-name)
  (car (mysql-fetch
          (format "select CONCAT(table_schema, ' ', table_type)
             from information_schema.tables
            where table_schema='%s' and table_name='%s'"
          schema-name table-name))))

;; (progn (let ((schema-name "userdb")
;;              (function-name "docid"))
;;          (mysql-fetch-function-doc schema-name function-name)))
(defun mysql-fetch-function-doc (schema-name function-name)
  (let* ((begin-marker " FUNCTION ")
         (end-marker "BEGIN")
         (function-create-string (mapconcat 'identity
                                   (mysql-fetch
                                    (format "show create function %s.%s \\G" schema-name function-name))
                                   "\n"))
         (start (string-match begin-marker function-create-string))
         (end (string-match end-marker function-create-string)))
    (substring function-create-string (+ start (length begin-marker)) end )))

(defun mysql-fetch-columns (table-name)
  (debug-print
   (format "mysql-fetch-columns ac-prefix: %s table: %s" ac-prefix table-name))
  (mysql-fetch
        (format "select column_name
             from information_schema.columns
            where table_name='%s'" table-name)))

(defun mysql-fetch-column-document (col-name)
  (mysql-fetch-column-doc (table-name) col-name))

(defun mysql-fetch-table-document (table-name)
  (mysql-fetch-table-doc (schema-name) table-name))

(defun mysql-fetch-function-document (function-name)
  (debug-print (format ">>> mysql-fetch-function-document %s" function-name))
  (mysql-fetch-function-doc (schema-name) function-name))

;;; Auto-complete SQL candidate generators
(setq schemas-cache (make-hash-table :test 'equal-string))
(defun candidates-sql-schemas ()
  (let ((schemas
         (or (gethash "schemas" schemas-cache)
             (puthash "schemas" (mysql-fetch-schemas) schemas-cache))))
    (mapcar (lambda (schema) (completion-candidate schema "schema")) schemas)))

(setq table-cache (make-hash-table :test 'equal-string))
(defun candidates-sql-tables ()
  (let ((schema (schema-name)))
    (when (member schema schemas-whitelist)
      (let* ((tables
             (or (gethash schema table-cache)
                 (puthash schema (mysql-fetch-tables schema) table-cache))))
        (mapcar (lambda (table) (completion-candidate table "table")) tables)))))

(setq functions-cache (make-hash-table :test 'equal-string))
(defun candidates-sql-functions ()
  (let ((schema (schema-name)))
    (when (member schema schemas-whitelist)
      (let* ((functions
              (or (gethash schema functions-cache)
                  (puthash schema (mysql-fetch-functions schema) functions-cache))))
        (mapcar (lambda (function) (completion-candidate function "function")) functions)))))

(setq columns-cache (make-hash-table :test 'equal-string))
(defun candidates-sql-columns ()
  (let ((table (table-name)))
    (when (member table tables-whitelist)
      (let ((columns
             (or (gethash table columns-cache)
                 (puthash table (mysql-fetch-columns table) columns-cache))))
        (mapcar (lambda (column) (completion-candidate column "column")) columns)))))

;;; Using Auto-complete source only for side effect of registering
;;; table alias
(defun watch-and-register-table-alias ()
  (let* ((point-alias-start
         (save-excursion
           (re-search-backward "\\ +")
           (+ (point) 1)))
        (point-alias-end
         (save-excursion
           (re-search-backward "\\ +")
           (re-search-forward "[_[:alnum:]]+") (point)))
        (point-table-start (+ ac-point 1))
        (point-table-end
         (save-excursion
           (re-search-backward "\\.")
           (re-search-forward "\\ +") (- (point) 1)))
        (alias (buffer-substring-no-properties point-alias-start point-alias-end))
        (table (buffer-substring-no-properties point-table-start point-table-end)))
    (cond (enable-debug
           (debug-print
            (format ">>> watch-and-register-table-alias %s table:[%s] alias:[%s] start-alias:[%s] end-alias:[%s]"
                    ac-point
                    table
                    alias
                    point-alias-start
                    point-alias-end)))
          ('t
           (message
            (format "auto-complete-sql registered alias:[%s] for table:[%s]"
                    alias
                    table))))
    (update-alias-table-alist alias table))
  nil)

;;; Auto-complete mode configuration
;; Must be added before the other auto-complete sources. If an alias
;; is also provided by another ac-source e.g., doc is provided as
;; document by schemas then source first in the list that provides a
;; completion will be chosen. This makes using doc as an alias
;; impossible but with the correct ordering cancelling a completion
;; and the continuing will get register the new alias.
(setq ac-0-sql-watch-and-register-table-alias
      '((requires . 1)
        (prefix . "\\(\\.[_[:alnum:]]+ +[_[:alnum:]]+\\)")
        (candidates .  watch-and-register-table-alias)))

(setq ac-1-sql-schemas
      '((requires . 2)
        (candidates . candidates-sql-schemas)))

(setq ac-1-sql-functions
      '((document . mysql-fetch-function-document)
        (requires . 1)
        (prefix . "\\.\\([_[:alnum:]]+\\)")
        (candidates .  candidates-sql-functions)))

(setq ac-1-sql-tables
      '((document . mysql-fetch-table-document)
        (requires . 1)
        (prefix . "\\.\\([_[:alnum:]]+\\)")
        (candidates .  candidates-sql-tables)))

(setq ac-1-sql-columns
      '((document . mysql-fetch-column-document)
        (requires . 1)
        (prefix . "\\.\\([_[:alnum:]]+\\)")
        (candidates .  candidates-sql-columns)))

;;; SQL inspesction functions
(defun print-row (row)
  (mapcar (lambda (column)
            (progn
              (princ "|")
              (princ column))) row)
  (princ "|")
  (terpri))

(defun show-table-definition ()
  (interactive)
  (let* ((temp-buffer-setup-hook
          (lambda ()
            (org-mode)))
         (temp-buffer-show-hook
          (lambda ()
            (org-table-align)
            (help-mode)))
         (selected-text
          (buffer-substring-no-properties (mark) (point)))
         (rows
          (mysql-shell-query (format "describe %s" selected-text))))
    (with-output-to-temp-buffer selected-text
      (print-row (car rows))
      (print-row '(-))
      (mapcar 'print-row (cdr rows)))))

(setq schemas-whitelist
       (mysql-fetch
        (format "show schemas")))

(setq tables-whitelist
      (mysql-fetch
        (format "select table_name
             from information_schema.tables
            where table_schema in (%s)"
           (to-sql-list schemas-whitelist))))

(provide 'auto-complete-mysql)

;;; auto-complete-mysql ends here
