# auto-complete-mysql

Emacs auto-complete mode ac-source for mysql-auto-completions. Can be used to get mysql auto-completions, schema, table, column and function completions, in any major mode e.g., org-mode, where you might write queries.

The lisp file provides functions that need to be added as `ac-sources` like:

```elisp
(setq-default ac-sources
              '(
                ...
                ac-0-sql-watch-and-register-table-alias
                ac-1-sql-schemas
                ac-1-sql-functions
                ac-1-sql-tables
                ac-1-sql-columns
                ...
                ))```

Possible auto-completions are dynamically pulled from a configurable mysql instances and cached. Abusing auto-complete with `ac-0-sql-watch-and-register-table-alias` this mode provides completions on table aliases also i.e.,

```SQL
 SELECT c.<xx> FROM customers c
 ```

Auto-completions will be provided for alias c as a table of type customers.
