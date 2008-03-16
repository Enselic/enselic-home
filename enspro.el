;; enspro.el - Enselic's Project management system for emacs
;;
;; Copyright © 2007 Martin Nordholts
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Prerequesites
;;

(eval-when-compile (require 'cl))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Customizable variales
;;

(defcustom enspro-projects-file "~/enspro-projects"
  "*Where to read and write assoscitions of a project name and
its project file - which is simply Elisp code that gets evaluated
when the project is loaded."
  :type 'file
  :group 'enspro)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Interactively callable functions
;;

(defun enspro-create-and-add-project (project-name project-file-path)
  "Intelligently wraps enspro-create-new-project-file and
enspro-add-project"

  (interactive "MProject name: \nFProject file: ")

  ;; Don't allow e.g. directories to be specified
  (if (and (file-exists-p project-file-path) (not (file-regular-p project-file-path)))
      (error "You must specify a normal file"))

  ;; If the project file does not exist, create it
  (if (not (file-exists-p project-file-path))
      (enspro-create-new-project-file project-file-path))

  ;; Assume the file was created, store the association
  (enspro-add-project project-name project-file-path))



(defun enspro-load-project (project-name)
  "Loads a given project."

  (interactive (list (enspro-read-project-name "Load project: " :require-match t)))

  (let ((project-file (gethash project-name enspro-projects)))

    (if (not (file-exists-p project-file))
        (message "Project %s didn't exist, add it first." project-name)
      ;; Load the actual project
      (enspro-load-project-file project-file))))

(defun enspro-remove-project (project-name)
  "Removes a project name -> project file association."

  (interactive (list (enspro-read-project-name "Name of project to remove association to: " :require-match t)))

  (setq enspro-project-completion-collection (remove project-name enspro-project-completion-collection))
  (remhash project-name enspro-projects))



(defun enspro-add-project (project-name project-file-path)
  "Makes a project name -> project file association."

  (interactive "MProject name: \nfProject file: ")

  (add-to-list 'enspro-project-completion-collection project-name)
  (puthash project-name project-file-path enspro-projects))





(defun enspro-create-new-project-file (project-file-path
                                       project-root
                                       versioned-files-root
                                       grep-command
                                       compile-command
                                       project-binary)
  "Creates a new project file that project-specific customization can
be added to."
  (interactive
   (let* ((project-file-path (expand-file-name (read-file-name "Project file: " default-directory "project.el")))
          (project-file-root (file-name-directory project-file-path)))
     (list
      ;; project-file-path
      project-file-path

      ;; project-root
      (expand-file-name "." (read-directory-name
                             "Project root (must be writable and persistent): "
                             project-file-root nil t))

      ;; versioned-files-root
      (and (yes-or-no-p "Any versioned files in project? ")
           (expand-file-name "." (read-directory-name
                                  "Versioned files root: "
                                  project-file-root nil t)))

      ;; grep-command
      (read-string "Default grep: " (format enspro-default-grep-command-format
                                            project-root))

      ;; compile-command
      (read-string "Default compile command: " (format enspro-default-compile-command-format
                                                       project-root))


      ;; project-binary
      (and (yes-or-no-p "Specify a project binary? ")
           (read-file-name "Project binary: " project-file-root)))))

  ;; Write the project file
  (with-temp-buffer

    ;; Setup project (`(if t' to get consistent indentation for the
    ;; `(insert':s)
    (if t                    (insert "(setq enspro-active-project\n"))
    (if t                    (insert "      (make-enspro-project\n"))
    (if t                    (insert "       :project-root         \"" project-root                  "\"\n"))
    (if versioned-files-root (insert "       :versioned-files-root \"" versioned-files-root          "\"\n"))
    (if grep-command         (insert "       :grep-command         \"" grep-command                  "\"\n"))
    (if compile-command      (insert "       :compile-command      \"" compile-command               "\"\n"))
    (if project-binary       (insert "       :project-binary       \"" (if project-binary "t" "nil") "\"\n"))
    (if t                    (insert "))"))

    (write-file project-file-path)))

(defun enspro-save-file-cache-to-project-root ()
  (interactive)
  (let ((project-root (enspro-project-project-root enspro-active-project)))
    (file-cache-save-cache-to-file (expand-file-name enspro-default-file-cache-file-name project-root))))

(defun enspro-edit-project-file ()
  (interactive)
  (find-file enspro-project-project-file))

(defun enspro-setup-gud-window (left-buffer gud-buffer-name)
  (interactive)
  (full-screen-buffer left-buffer)
  (split-window-vertically)
  (switch-to-buffer-other-window gud-buffer-name))

(defun get-buffer-regexp (regex)
  (find-if (lambda (x) (string-match regex (buffer-name x)))
           (buffer-list)))

(defun enspro-debug-binary ()
  (interactive)
  (let*
      ((gud-buffer        (get-buffer-regexp "\*gud-.*"))
       (gud-friend-buffer (current-buffer)))
    (unless (equal gud-friend-buffer gud-buffer)
      (if (and enspro-gud-buffer-name (buffer-live-p (get-buffer enspro-gud-buffer-name)))
          ;; If *gud-* exists
          (enspro-setup-gud-window (current-buffer) enspro-gud-buffer-name)

        (if (not (enspro-project-project-binary enspro-active-project))
            (error "No binary set."))

        (setq enspro-gud-buffer-name gud-buffer)

        (gdb (concat "gdb --annotate=3 --args " (enspro-project-project-binary enspro-active-project)))
;        (gdb (concat "libtool --mode=execute gdb --annotate=3 --args " (enspro-project-project-binary enspro-active-project)))
        (enspro-setup-gud-window gud-friend-buffer enspro-gud-buffer-name)))))

(defun enspro-run-debug-binary ()
  (interactive)
  (if (not (enspro-project-project-binary enspro-active-project))
      (error "No binary set.")
    (shell-command (concat (enspro-project-project-binary enspro-active-project) "&"))))

(defun enspro-stop-debug-binary ()
  (interactive)
  (if (buffer-live-p (get-buffer enspro-gud-buffer-name))
      (progn
        (let*
            ((non-gud-buffer (with-current-buffer enspro-gud-buffer-name (other-buffer (get-buffer enspro-gud-buffer-name) t))))
          (kill-buffer enspro-gud-buffer-name)
          (setq enspro-gud-buffer-name nil)
          (full-screen-buffer non-gud-buffer)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Constants
;;

(defconst enspro-default-grep-command-format "cd %s && gid "
  "Default grep-command format to put in new project files, %s becomes project root")

(defconst enspro-default-compile-command-format "make -k -j3 -C %s"
  "Default compile-command format %s becomes project root")

(defconst enspro-default-file-cache-file-name ".enspro-file-cache"
  "The name of the file-cache file that gets loaded for projects
  that have file cache loading enabled.")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Structures
;;

(defstruct enspro-project
  "Holds data for a project in the enspro system."
  (project-root          nil       :read-only t)
  (versioned-files-root  nil       :read-only t)
  (grep-command          nil       :read-only t)
  (compile-command       nil       :read-only t)
  (project-binary        nil       :read-only t))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Variables
;;

(defvar enspro-project-project-file nil)

(defvar enspro-active-project nil
  "The active project.")

(defvar enspro-project-completion-collection nil
  "A list of project names used for completing-read.")

(defvar enspro-projects (makehash 'equal)
  "A hash of 'project name' => 'project file' associations")

(defvar enspro-gud-buffer-name nil
  "Name of GUD buffer.")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Functions
;;

(defun enspro-create-default-project-file-in-current-dir ()
  (interactive)
  (if (file-exists-p "project.el")
      (message "File project.el alrady exists, refusing to overwrite")
    (enspro-create-new-project-file (expand-file-name "project.el" default-directory)
                                    default-directory
                                    default-directory
                                    (format enspro-default-grep-command-format default-directory)
                                    (format enspro-default-compile-command-format default-directory)
                                    t))
  ;; TODO make work
  ;; :project-root default-directory                                                   ;;
  ;; :versioned-files-root default-directory                                           ;;
  ;; :compile-command (format enspro-default-compile-command-format default-directory) ;;
  ;; :grep-command (format enspro-default-grep-command-format default-directory)       ;;
  ;; :project-binary t))                                                               ;;
)


(defun enspro-read-project-name (prompt &key require-match)
  (completing-read prompt enspro-project-completion-collection nil require-match))

(defun enspro-parse-projects-file ()
  "Parses `enspro-projects-file' and puts results in
`enspro-projects', which is a hash with 'project name' to
'project file path' associations. Also constructs the list of
completion candidate for `enspro-load-project'."
  (when (file-exists-p enspro-projects-file)

    ;; We are lazy (or is the Right way of doing it), so we prevent
    ;; end of file errors simply by catching them
    (condition-case nil
        (with-temp-buffer
          (insert-file-contents-literally enspro-projects-file)
          (let ((project-name      nil)
                (project-file-path nil))
            (beginning-of-buffer)
            (setq project-name      (read (current-buffer)))
            (setq project-file-path (read (current-buffer)))
            (while (and project-name project-file-path)
              (puthash project-name project-file-path enspro-projects)

              ;; Add the project name to the list of project names.
              (add-to-list 'enspro-project-completion-collection project-name)

              (setq project-name      (read (current-buffer)))
              (setq project-file-path (read (current-buffer))))))
      (error nil))))

(defun enspro-write-projects-file ()
  "Writes contents of `enspro-projects' to
`enspro-projects-file' but only if it contains projects"
  (if (> (hash-table-count enspro-projects) 0)
      (with-temp-buffer
        (maphash 'enspro-write-one-mapping enspro-projects)
        (write-file enspro-projects-file))))

(defun enspro-write-one-mapping (project-name project-file-path)
  (prin1 project-name (current-buffer))
  (insert " ")
  (prin1 project-file-path (current-buffer))
  (insert "\n"))

(defun enspro-load-project-file (project-file-path)
  "Performs the loading routine for the project with the
specified project file."

  (interactive "fProject file: ")

  ;; Reset some stuff
  (file-cache-clear-cache)
  (setq tags-file-name  nil)
  (setq tags-table-list nil)

  (setq enspro-project-project-file project-file-path)
  (load project-file-path)

  (if (not (enspro-project-p enspro-active-project))
      (error "The project file must set enspro-active-project."))

  (let* ((project-root          (enspro-project-project-root         enspro-active-project))
         (versioned-files-root  (enspro-project-versioned-files-root enspro-active-project))
         (compile-command-local (enspro-project-compile-command      enspro-active-project))
         (grep-command-local    (enspro-project-grep-command         enspro-active-project))
         (project-binary        (enspro-project-project-binary       enspro-active-project))
         (file-cache-file       (expand-file-name enspro-default-file-cache-file-name project-root))
         (tags-file             (expand-file-name "TAGS" project-root)))

    (desktop-change-dir project-root)

    (if (file-regular-p file-cache-file)
        (file-cache-read-cache-from-file file-cache-file))

    (if (and (file-regular-p tags-file) (not tags-table-list))
        (visit-tags-table tags-file))

    (setq compile-command compile-command-local)

    (setq grep-command grep-command-local)))

(defun create-files-for-new-project ()

  ;; mkid
  (message "==== Running mkid ====")
  (if (file-exists-p "ID")
      (message "The ID file alrady exists, refusing to create a new one.")
    (shell-command "mkid"))

  ;; ctags
  (message "==== Running exuberant ctags ====")
  (if (file-exists-p "TAGS")
      (message "The TAGS file alrady exists, refusing to create a new one.")
    (shell-command "ctags -e -R"))

  ;; file cache
  (message "==== Creating file cache ====")
  (create-file-cache-for-current-directory)

  ;; project file
  (message "==== Creating project file ====")
  (enspro-create-default-project-file-in-current-dir))




(provide 'enspro)
