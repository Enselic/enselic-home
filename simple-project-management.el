;; simple-project-management.el - A simple project management system for Emacs.
;;
;; Copyright © 2007-2008 Martin Nordholts
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
;;
;;
;; To install, put
;;
;;   (add-hook 'after-init-hook 'simple-project-management-initialize)
;;
;; in your ~/.emacs
;;

(eval-when-compile (require 'cl))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Customizable variables
;;

(defgroup simple-project-management nil
  "An Emacs extension for simple but powerful management of
projects."
  :group   'tools)

(defcustom simple-project-management-projects-directory       "~/projects"
  "Where to store project data."
  :type  'directory
  :group 'simple-project-management)

(defcustom simple-project-management-project-config-file-name "config.cfg"
  "The name of the file to hold various project data."
  :type  'file
  :group 'simple-project-management)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Structs
;;

(defstruct simple-project-management-project-type-entry
  "Holds data for a registered project type."
  (name                      nil  :read-only)
  (create-project-function   nil  :read-only)
  (load-project-function     nil  :read-only)
  (unload-project-function   nil  :read-only)
  (destroy-project-function  nil  :read-only))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Constants (internal)
;;

(defconst simple-project-management-project-type-config-key "project_type")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Variables (internal)

(defvar simple-project-management-registered-project-types
  (list (make-simple-project-management-project-type-entry
         :name                     "desktop"
         :create-project-function  'simple-project-management-desktop-project-create
         :load-project-function    'simple-project-management-desktop-project-load
         :unload-project-function  'simple-project-management-desktop-project-unload
         :destroy-project-function 'simple-project-management-desktop-project-destroy))
  "List of entires for registered project types.")

(defvar simple-project-management-current-project nil
  "The current project.")

(defvar simple-project-management-load-project-history nil
  "History of loaded project names.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Public functions
;;

(defun simple-project-management-create-project (project-name
                                                 project-type)
  "Creates a project with the given name of the given type. If
called interactively, prompt for project name and type."
  (interactive (list (progn
                       (setq project-name (read-from-minibuffer "Project name: "))
                       (if (simple-project-management-project-exists project-name)
                           (error "Project `%s' already exists" project-name)
                         project-name))
                     
                     (completing-read (format "Project type (default `%s'): " (simple-project-management-default-project-type))
                                      (simple-project-management-get-project-type-assoc-list)
                                      nil
                                      t
                                      nil
                                      'simple-project-management-load-project-history
                                      (simple-project-management-default-project-type))))

  ;; Create the directory that project specific data is put in
  (simple-project-management-create-project-directory project-name)

  ;; Create a project config file that for example keeps track of
  ;; the project type
  (simple-project-management-create-config-file project-name
                                                project-type)

  ;; Call the project creation function for the registred project type
  (funcall (simple-project-management-get-create-function project-type)
           project-name))


(defun simple-project-management-load-project (project-name)
  "Loads the project with the given name. If called
interactively, prompts for the name."
  (interactive (list (simple-project-management-read-exisisting-project-name)))

  ;; Unload the active project if any
  (simple-project-management-unload-active-project)

  ;; Load the project
  (setq simple-project-management-current-project project-name)
  (funcall (simple-project-management-get-load-function
            (simple-project-management-get-project-type project-name))
           project-name))


(defun simple-project-management-unload-active-project ()
  "Unloads active project. Not for interactive use."
  (if simple-project-management-current-project
      (funcall (simple-project-management-get-unload-function
                (simple-project-management-get-project-type simple-project-management-current-project))
               simple-project-management-current-project)))
      

(defun simple-project-management-destroy-project (project-name)
  "Destroys the project with the given name. If called
interactively, prompts for the name."
  (interactive (list (simple-project-management-read-exisisting-project-name)))

  (let (project-type
        destroy-project-function)

    (setq project-type (simple-project-management-get-project-type project-name))

    ;; Don't panic on corrupt projects, use `ignore' when things looks fishy
    (setq destroy-project-function (if project-type
                                       (simple-project-management-get-destroy-function project-type)
                                     'ignore))

    (funcall destroy-project-function project-name)

    (delete-file-if-exists      (simple-project-management-get-config-file       project-name))
    (delete-directory-if-exists (simple-project-management-get-project-directory project-name))))


(defun simple-project-management-register-project-type (project-type-entry)
  "External project types use this function to register
themselves. `project-type-entry' is an instance of
`simple-project-management-project-type-entry'"
  (add-to-list 'simple-project-management-registered-project-types
               project-type-entry))


(defun simple-project-management-initialize ()
  "Add this to `after-init-hook'"
  (add-hook 'kill-emacs-hook 'simple-project-management-unload-active-project))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; From here on functions are internal to simple-project-management.el
;;

(defun simple-project-management-default-project-type ()
  "Returns the default project type."
  (car (car simple-project-management-registered-project-types)))


(defun simple-project-management-read-exisisting-project-name ()
  "Do a completing reading of an existing project name."
  (let (existing-project-names)
    (setq existing-project-names (simple-project-management-get-existing-project-names))
    (if existing-project-names
        (completing-read "Project name: " existing-project-names nil t)
      (error "No existing projects"))))


(defun simple-project-management-get-project-type (project-name)
  "Get project type for project named `project-name'"
  (get-config-string (simple-project-management-get-config-file project-name)
                     simple-project-management-project-type-config-key))


(defun simple-project-management-get-project-type-entry (project-type)
  "Get the project type entry for the project type
`project-type'."
  (cdr (assoc project-type
              (simple-project-management-get-project-type-assoc-list))))


(defun simple-project-management-get-create-function (project-type)
  "Get create-function for project type `project-type'."
  (simple-project-management-project-type-entry-create-project-function
   (simple-project-management-get-project-type-entry project-type)))


(defun simple-project-management-get-load-function (project-type)
  "Get load-function for project type `project-type'."
  (simple-project-management-project-type-entry-load-project-function
   (simple-project-management-get-project-type-entry project-type)))


(defun simple-project-management-get-unload-function (project-type)
  "Get unload-function for project type `project-type'."
  (simple-project-management-project-type-entry-unload-project-function
   (simple-project-management-get-project-type-entry project-type)))


(defun simple-project-management-get-destroy-function (project-type)
  "Get destroy-function for project type `project-type'."
  (simple-project-management-project-type-entry-destroy-project-function
   (simple-project-management-get-project-type-entry project-type)))


(defun simple-project-management-get-current-project ()
  "Gets name of current project."
  simple-project-management-current-project)


(defun simple-project-management-get-existing-project-names ()
  "Returns a list of existing project names."
  (if (not (file-exists-p simple-project-management-projects-directory))
      nil
    (let (directory-content
          existing-project-names)
      
      (setq directory-content (directory-files-without-dots simple-project-management-projects-directory))

      (dolist (list-item directory-content)
        (if (file-directory-p (expand-file-name list-item simple-project-management-projects-directory))
            (add-to-list 'existing-project-names list-item)))

      existing-project-names)))


(defun simple-project-management-create-config-file (project-name project-type)
  "Creates config file for `project-name' and puts the
`project-type' of the project in it."
  (with-temp-buffer
    (insert simple-project-management-project-type-config-key " = " project-type "\n")
    (write-file (simple-project-management-get-config-file project-name))))


(defun simple-project-management-get-config-file (project-name)
  "Returns the config file for `project-name'"
  (expand-file-name simple-project-management-project-config-file-name
                    (simple-project-management-get-project-directory project-name)))

(defun simple-project-management-get-project-type-assoc-list ()
  "Returns an association list of `(project-type . project-type-entry)'."
  (mapcar (lambda (project-type-entry)
            (cons (simple-project-management-project-type-entry-name project-type-entry)
                  project-type-entry))
          simple-project-management-registered-project-types))


(defun simple-project-management-get-current-project-dir ()
  "Returns directory of current project."
  (simple-project-management-get-project-directory simple-project-management-current-project))


(defun simple-project-management-project-exists (project-name)
  "Returns `t' if project `project-name' exists, `nil' otherwise."
  (file-exists-p (simple-project-management-get-project-directory project-name)))


(defun simple-project-management-create-project-directory (project-name)
  "Creates project directory for `project-name'."
  (make-directory (simple-project-management-get-project-directory project-name) t))


(defun simple-project-management-get-project-directory (project-name)
  "Returns project directory for `project-name'."
  (expand-file-name project-name simple-project-management-projects-directory))
    

(defun simple-project-management-desktop-project-create (project-name)
  "Creates a desktop project."
  ())


(defun simple-project-management-desktop-project-load (project-name)
  "Loads a desktop project."
  (desktop-change-dir (simple-project-management-get-project-directory project-name)))


(defun simple-project-management-desktop-project-unload (project-name)
  "Unloads a desktop project."
  ;; Unloading the desktop is just annoying..
  ())


(defun simple-project-management-desktop-project-destroy (project-name)
  "Destroys a desktop project."
  ())



(provide 'simple-project-management)
