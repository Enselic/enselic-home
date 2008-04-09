;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; simple-project.el - A simple project management system for Emacs.
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Prerequesites
;;

(eval-when-compile (require 'cl))



(defgroup simple-project nil
  "An Emacs extension for simple but powerful management of
projects."
  :group 'tools)

(defcustom simple-project-projects-directory "~/simple-project-projects"
  "Where to read and write assoscitions of a project name and its
project file - which is simply Elisp code that gets evaluated
when the project is loaded."
  :type 'directory
  :group 'simple-project)


(defstruct simple-project-project-type-entry
  "Holds data for a registered project type."
  (name                      nil  :read-only)
  (create-project-function   nil  :read-only)
  (load-project-function     nil  :read-only)
  (unload-project-function   nil  :read-only)
  (destroy-project-function  nil  :read-only))


(defconst simple-project-config-file-name        "simple-project.cfg")

(defconst simple-project-project-type-config-key "project_type")


(defvar simple-project-current-project nil
  "The current project.")

(defvar simple-project-registered-project-types
  (list (cons "Basic Project"
              (make-simple-project-project-type-entry :name                     "Basic Project"
                                                      :create-project-function  'simple-project-create
                                                      :load-project-function    'simple-project-load
                                                      :unload-project-function  'simple-project-unload
                                                      :destroy-project-function 'simple-project-destroy)))
  "List of entires for registered project types.")



(defun simple-project-create-project (project-name
                                      project-type)
  "Creates a project with the given name of the given type. If
called interactively, prompt for project name and type."
  (interactive (list (progn
                       (setq project-name (read-from-minibuffer "Project name: "))
                       (if (simple-project-project-exists project-name)
                           (error "Project `%s' already exists" project-name)
                         project-name))
                     
                     (completing-read "Project type: "
                                      simple-project-registered-project-types nil t)))

  (let (create-project-function)
    (setq create-project-function (simple-project-get-create-function project-type))

    ;; Create the directory that project specific data is put in
    (simple-project-create-project-directory project-name)

    ;; Create a project config file that for example keeps track of
    ;; the project type
    (simple-project-create-config-file project-name
                                       project-type)

    ;; Call the project creation function for the registred project type
    (funcall create-project-function project-name)))


(defun simple-project-unload-active-project ()
  (let (active-project)
    (setq active-project (simple-project-get-current-project))

    (if active-project
        (let (project-type
              unload-project-function)

          (setq project-type            (simple-project-get-project-type active-project)
                unload-project-function (simple-project-get-unload-function project-type))

          (funcall unload-project-function active-project)))))
      

(defun simple-project-load-project (project-name)
  (interactive (list (simple-project-read-exisisting-project-name)))

  ;; Unload the active project if any
  (simple-project-unload-active-project)

  ;; Load the project
  (let (project-type
        load-project-function)

    (setq project-type          (simple-project-get-project-type project-name)
          load-project-function (simple-project-get-load-function project-type))

    (setq simple-project-current-project project-name)

    (funcall load-project-function project-name)))


(defun simple-project-destroy-project (project-name)
  (interactive (list (simple-project-read-exisisting-project-name)))

  (let (project-type
        destroy-project-function)

    (setq project-type (simple-project-get-project-type project-name))

    (if project-type
        (setq destroy-project-function (simple-project-get-destroy-function project-type))
      (setq destroy-project-function 'ignore))

    (funcall destroy-project-function project-name)

    (delete-file-if-exists      (simple-project-get-config-file       project-name))
    (delete-directory-if-exists (simple-project-get-project-directory project-name))))


(defun simple-project-register-project-type (project-type)
  (add-to-list 'simple-project-registered-project-types
               (cons (simple-project-project-type-entry-name project-type) project-type)))


(defun simple-project-read-exisisting-project-name ()
  (let (existing-project-names)
    (setq existing-project-names (simple-project-get-existing-project-names))
    (if existing-project-names
        (completing-read "Project name: " existing-project-names nil t)
      (error "No existing projects"))))


(defun simple-project-get-project-type (project-name)
  (get-config-string (simple-project-get-config-file project-name)
                     simple-project-project-type-config-key))

(defun simple-project-get-project-type-entry (project-type)
  (cdr (assoc project-type simple-project-registered-project-types)))


(defun simple-project-get-create-function (project-type)
  (simple-project-project-type-entry-create-project-function (simple-project-get-project-type-entry project-type)))


(defun simple-project-get-load-function (project-type)
  (simple-project-project-type-entry-load-project-function (simple-project-get-project-type-entry project-type)))


(defun simple-project-get-unload-function (project-type)
  (simple-project-project-type-entry-unload-project-function (simple-project-get-project-type-entry project-type)))


(defun simple-project-get-destroy-function (project-type)
  (simple-project-project-type-entry-destroy-project-function (simple-project-get-project-type-entry project-type)))


(defun simple-project-get-current-project ()
  simple-project-current-project)


(defun simple-project-get-existing-project-names ()
  (if (not (file-exists-p simple-project-projects-directory))
      nil
    (let (directory-content
          existing-project-names)
      
      (setq directory-content (directory-files-without-dots simple-project-projects-directory))

      (dolist (list-item directory-content)
        (if (file-directory-p (expand-file-name list-item simple-project-projects-directory))
            (add-to-list 'existing-project-names list-item)))

      existing-project-names)))


(defun simple-project-create-config-file (project-name project-type)
  (with-temp-buffer
    (insert simple-project-project-type-config-key " = " project-type "\n")
    (write-file (simple-project-get-config-file project-name))))


(defun simple-project-get-config-file (project-name)
  (expand-file-name simple-project-config-file-name
                    (simple-project-get-project-directory project-name)))


(defun simple-project-get-current-project-dir ()
  (simple-project-get-project-directory simple-project-current-project))


(defun simple-project-project-exists (project-name)
  (file-exists-p (simple-project-get-project-directory project-name)))


(defun simple-project-create-project-directory (project-name)
  (make-directory (simple-project-get-project-directory project-name) t))


(defun simple-project-get-project-directory (project-name)
  (expand-file-name project-name simple-project-projects-directory))
    

(defun simple-project-create (project-name)
  ())


(defun simple-project-load (project-name)
  (desktop-change-dir (simple-project-get-project-directory project-name)))


(defun simple-project-unload (project-name)
  ;; Unloading the desktop is just annoying..
  ())


(defun simple-project-destroy (project-name)
  ())



(provide 'simple-project)


