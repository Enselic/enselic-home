
;;;### (autoloads (vc-clearcase-start-view vc-clearcase-edcs vc-clearcase-list-view-private-files
;;;;;;  vc-clearcase-label-diff-report vc-clearcase-update-view vc-clearcase-list-checkouts
;;;;;;  clearcase-file-not-found-handler cleartool-program vc-clearcase)
;;;;;;  "vc-clearcase" "vc-clearcase.el" (18271 42492))
;;; Generated autoloads from vc-clearcase.el

(let ((loads (get (quote vc-clearcase) (quote custom-loads)))) (if (member (quote "vc-clearcase") loads) nil (put (quote vc-clearcase) (quote custom-loads) (cons (quote "vc-clearcase") loads))))

(defvar cleartool-program "cleartool" "\
The name of the cleartool executable.")

(custom-autoload (quote cleartool-program) "vc-clearcase" t)
(defun vc-clearcase-registered (file)
 (let (wdview
       retcode
       (program cleartool-program))
   (setq wdview
         (with-output-to-string
           (with-current-buffer standard-output
             (setq retcode
                   (call-process
                    program nil t nil "pwv" "-short" "-wdview")))))
   ;;(message "Wdview for %s is %S" file wdview)
   (if (or (not (eq retcode 0))
           (eq (compare-strings "** NONE **" 0 10 wdview 0 10) t))
       nil
     (load "vc-clearcase")
     (vc-clearcase-registered file))))

(autoload (quote clearcase-file-not-found-handler) "vc-clearcase" "\
Handle opening of version-extended ClearCase files.
This function should be added to `find-file-not-found-functions'
to handle opening ClearCase files in the format
file.txt@@/main/0.  The function will visit the file first, than
will open the specified version in another window, using
`vc-version-other-window'

\(fn)" nil nil)

(cond ((boundp (quote find-file-not-found-functions)) (add-hook (quote find-file-not-found-functions) (quote clearcase-file-not-found-handler))) ((boundp (quote find-file-not-found-hooks)) (add-hook (quote find-file-not-found-hooks) (quote clearcase-file-not-found-handler))))

(autoload (quote vc-clearcase-list-checkouts) "vc-clearcase" "\
List the checkouts of the current user in DIR.
If PREFIX-ARG is present, an user name can be entered, and all
the views are searched for checkouts of the specified user.  If
the entered user name is empty, checkouts from all the users on
all the views are listed.

\(fn DIR &optional PREFIX-ARG)" t nil)

(autoload (quote vc-clearcase-update-view) "vc-clearcase" "\
Run a cleartool update command in DIR and display the results.
With PREFIX-ARG, run update in preview mode (no actual changes
are made to the views).

\(fn DIR PREFIX-ARG)" t nil)

(autoload (quote vc-clearcase-label-diff-report) "vc-clearcase" "\
Report the changed file revisions between labels.
A report is prepared in the *label-diff-report* buffer for the
files in DIR that have different revisions between LABEL-1
and LABEL-2'.

\(fn DIR LABEL-1 LABEL-2)" t nil)

(autoload (quote vc-clearcase-list-view-private-files) "vc-clearcase" "\
List the view private files in DIR.
You can edit the files using 'find-file-at-point'

\(fn DIR)" t nil)

(autoload (quote vc-clearcase-edcs) "vc-clearcase" "\
Fetch the config spec for VIEW-TAG and pop up a buffer with it.
In interactive mode, prompts for a view-tag name with the default
of the current file's view-tag.

\(fn VIEW-TAG)" t nil)

(autoload (quote vc-clearcase-start-view) "vc-clearcase" "\
Start the dynamic view for VIEW-TAG.
In interactive mode, prompts for a view-tag name.

\(fn VIEW-TAG)" t nil)

(define-key vc-prefix-map "e" (quote vc-clearcase-edcs))

(define-key vc-prefix-map "f" (quote vc-clearcase-start-view))

(define-key vc-prefix-map "j" (quote vc-clearcase-gui-vtree-browser))

(define-key vc-prefix-map "o" (quote vc-clearcase-list-checkouts))

(define-key vc-prefix-map "p" (quote vc-clearcase-update-view))

(define-key vc-prefix-map "t" (quote vc-clearcase-what-view-tag))

(define-key vc-prefix-map "w" (quote vc-clearcase-what-rule))

(define-key vc-prefix-map "y" (quote vc-clearcase-what-version))

(define-key-after vc-menu-map [separator-clearcase] (quote ("----")) (quote separator2))

(define-key-after vc-menu-map [vc-clearcase-what-version] (quote ("Show file version" . vc-clearcase-what-version)) (quote separator2))

(define-key-after vc-menu-map [vc-clearcase-what-rule] (quote ("Show configspec rule" . vc-clearcase-what-rule)) (quote separator2))

(define-key-after vc-menu-map [vc-clearcase-what-view-tag] (quote ("Show view tag" . vc-clearcase-what-view-tag)) (quote separator2))

(define-key-after vc-menu-map [vc-clearcase-gui-vtree-browser] (quote ("Browse version tree (GUI)" . vc-clearcase-gui-vtree-browser)) (quote separator2))

(defvar clearcase-global-menu (let ((m (make-sparse-keymap "Clearcase"))) (define-key m [vc-clearcase-report-bug] (quote (menu-item "Report bug in vc-clearcase..." vc-clearcase-report-bug :help "Report a bug in vc-clearcase.el"))) (define-key m [separator-clearcase-1] (quote ("----" (quote separator-1)))) (define-key m [vc-clearcase-label-diff-report] (quote (menu-item "Label diff report..." vc-clearcase-label-diff-report :help "Report file version differences between two labels"))) (define-key m [vc-clearcase-list-view-private-files] (quote (menu-item "List View Private Files..." vc-clearcase-list-view-private-files :help "List view private files in a directory"))) (define-key m [vc-clearcase-list-checkouts] (quote (menu-item "List Checkouts..." vc-clearcase-list-checkouts :help "List Clearcase checkouts in a directory"))) (define-key m [vc-clearcase-update-view] (quote (menu-item "Update snapshot view..." vc-clearcase-update-view :help "Update a snapshot view"))) (define-key m [vc-clearcase-edcs] (quote (menu-item "Edit Configspec..." vc-clearcase-edcs :help "Edit a view's configspec"))) (define-key m [vc-clearcase-start-view] (quote (menu-item "Start dynamic view..." vc-clearcase-start-view :help "Start a dynamic view"))) (fset (quote clearcase-global-menu) m)))

(define-key-after menu-bar-tools-menu [clearcase] (quote (menu-item "Clearcase" clearcase-global-menu)) (quote vc))

(when (executable-find cleartool-program) (if (boundp (quote vc-handled-backends)) (unless (memq (quote CLEARCASE) vc-handled-backends) (setq vc-handled-backends (nconc vc-handled-backends (quote (CLEARCASE))))) (setq vc-handled-backends (quote (RCS CVS CLEARCASE)))))

(let ((backup-regexp "\\.~[a-zA-Z0-9_-~]+\\'") (garbage-regexp "\\.\\(contrib\\|keep\\)\\(\\.[0-9]+\\)?\\'")) (unless (assoc backup-regexp auto-mode-alist) (push (list backup-regexp nil t) auto-mode-alist)) (unless (assoc garbage-regexp auto-mode-alist) (push (list garbage-regexp nil t) auto-mode-alist)))

(define-obsolete-variable-alias (quote ah-clearcase-cleartool-program) (quote cleartool-program))

(define-obsolete-variable-alias (quote ah-clearcase-vtree-program) (quote clearcase-vtree-program))

(define-obsolete-variable-alias (quote ah-cleartool-timeout) (quote cleartool-timeout))

(define-obsolete-variable-alias (quote ah-cleartool-idle-timeout) (quote cleartool-idle-timeout))

(define-obsolete-variable-alias (quote ah-cleartool-save-stop-data) (quote cleartool-save-stop-data))

(define-obsolete-variable-alias (quote ah-clearcase-checkout-comment-type) (quote clearcase-checkout-comment-type))

(define-obsolete-variable-alias (quote ah-clearcase-checkout-policy) (quote clearcase-checkout-policy))

(define-obsolete-variable-alias (quote ah-clearcase-rmbranch-on-revert-flag) (quote clearcase-rmbranch-on-revert-flag))

(define-obsolete-variable-alias (quote ah-clearcase-diff-cleanup-flag) (quote clearcase-diff-cleanup-flag))

(define-obsolete-variable-alias (quote ah-clearcase-use-external-diff) (quote clearcase-use-external-diff))

(define-obsolete-variable-alias (quote ah-clearcase-no-label-action) (quote clearcase-no-label-action))

(define-obsolete-variable-alias (quote ah-clearcase-confirm-label-move) (quote clearcase-confirm-label-move))

;;;***

;;;### (autoloads (ucm-checkin-activity ucm-browse-activity ucm-show-current-activity
;;;;;;  ucm-set-activity) "ucm" "ucm.el" (18265 55184))
;;; Generated autoloads from ucm.el

(autoload (quote ucm-set-activity) "ucm" "\
Set the UCM ACTIVITY in the current directory.
In interactive mode, the user is prompted for the available
activities in the stream associated with the UCM view in the
`default-directory', and the selected one is set.

Two special activity names are also accepted: *NONE* which will
cause the current activity to be unset and *NEW-ACTIVITY* which
will create and set a new activity (the user is prompted for the
activity headline).

\(fn &optional ACTIVITY)" t nil)

(autoload (quote ucm-show-current-activity) "ucm" "\
Show the current activity in the view.
With prefix argument (EXTRA-INFO), also shows the number of
files modified under this activity, number of versions and the
number of checked out files.

\(fn &optional EXTRA-INFO)" t nil)

(autoload (quote ucm-browse-activity) "ucm" "\
Pop-up an information buffer about ACTIVITY.
The buffer will contain a report about the file versions
checked-in under the activity plus any contributing activities.
The report contains buttons (hyperlinks) to files, versions and
other activities.

In interactive mode, the user is prompted for an activity name
and completion is available.  ACTIVITY must be in the current
stream (corresponding to the view in `default-directory').  With
prefix argument, obsolete activities can also be selected.  With
a negative prefix argument any activity can be selected, but no
completion is provided.

There are no restriction on ACTIVITY when called from another
program.

\(fn ACTIVITY)" t nil)

(autoload (quote ucm-checkin-activity) "ucm" "\
Check in all files checked-out under ACTIVITY.
This will pop-up a `log-edit' buffer to enter the check in
comment, than attempt to check in the files.

If the log buffer is empty, each file to be checked in using its
original check out comment, otherwise the same log message will
be used for all files.

An error will be signalled if no files are checked out under
ACTIVITY.

HINT: `log-edit-modes' allows to see what files will be
checked-in using \\[log-edit-show-files].

\(fn ACTIVITY)" t nil)

;;;***

(provide 'vc-clearcase-auto)
