;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Prerequesites
;;

(eval-when-compile (require 'cl))



(defgroup simple-project-management nil
  "An Emacs extension for simple but powerful management of
projects."
  :group 'tools)

(defcustom simple-project-management-projects-directory "~/.simple-project-management-projects"
  "Where to read and write assoscitions of a project name and its
project file - which is simply Elisp code that gets evaluated
when the project is loaded."
  :type 'directory
  :group 'simple-project-management)


(defstruct simple-project-management-project-type-entry
  "Holds data for a registered project type."
  (name                      nil  :read-only)
  (create-project-function   nil  :read-only)
  (load-project-function     nil  :read-only)
  (unload-project-function   nil  :read-only)
  (destroy-project-function  nil  :read-only))


(defconst simple-project-management-config-file-name        "simple-project-management.cfg")
(defconst simple-project-management-project-type-config-key "project_type")
(defconst simple-project-management-basis-project-type      "basic")


(defvar simple-project-management-current-project nil
  "The current project.")

(defvar simple-project-management-registered-project-types
  (list (cons simple-project-management-basis-project-type
              (make-simple-project-management-project-type-entry :name           simple-project-management-basis-project-type
                                                      :create-project-function  'simple-project-management-create
                                                      :load-project-function    'simple-project-management-load
                                                      :unload-project-function  'simple-project-management-unload
                                                      :destroy-project-function 'simple-project-management-destroy)))
  "List of entires for registered project types.")



(defun simple-project-management-create-project (project-name
                                      project-type)
  "Creates a project with the given name of the given type. If
called interactively, prompt for project name and type."
  (interactive (list (progn
                       (setq project-name (read-from-minibuffer "Project name: "))
                       (if (simple-project-management-project-exists project-name)
                           (error "Project `%s' already exists" project-name)
                         project-name))
                     
                     (completing-read (format "Project type (default `%s') : " (simple-project-management-default-project-type))
                                      simple-project-management-registered-project-types
                                      nil
                                      t
                                      nil
                                      nil
                                      (simple-project-management-default-project-type))))

  (let (create-project-function)
    (setq create-project-function (simple-project-management-get-create-function project-type))

    ;; Create the directory that project specific data is put in
    (simple-project-management-create-project-directory project-name)

    ;; Create a project config file that for example keeps track of
    ;; the project type
    (simple-project-management-create-config-file project-name
                                       project-type)

    ;; Call the project creation function for the registred project type
    (funcall create-project-function project-name)))


(defun simple-project-management-unload-active-project ()
  (let (active-project)
    (setq active-project (simple-project-management-get-current-project))

    (if active-project
        (let (project-type
              unload-project-function)

          (setq project-type            (simple-project-management-get-project-type active-project)
                unload-project-function (simple-project-management-get-unload-function project-type))

          (funcall unload-project-function active-project)))))
      

(defun simple-project-management-load-project (project-name)
  (interactive (list (simple-project-management-read-exisisting-project-name)))

  ;; Unload the active project if any
  (simple-project-management-unload-active-project)

  ;; Load the project
  (let (project-type
        load-project-function)

    (setq project-type          (simple-project-management-get-project-type project-name)
          load-project-function (simple-project-management-get-load-function project-type))

    (setq simple-project-management-current-project project-name)

    (funcall load-project-function project-name)))


(defun simple-project-management-destroy-project (project-name)
  (interactive (list (simple-project-management-read-exisisting-project-name)))

  (let (project-type
        destroy-project-function)

    (setq project-type (simple-project-management-get-project-type project-name))

    (if project-type
        (setq destroy-project-function (simple-project-management-get-destroy-function project-type))
      (setq destroy-project-function 'ignore))

    (funcall destroy-project-function project-name)

    (delete-file-if-exists      (simple-project-management-get-config-file       project-name))
    (delete-directory-if-exists (simple-project-management-get-project-directory project-name))))


(defun simple-project-management-register-project-type (project-type)
  (add-to-list 'simple-project-management-registered-project-types
               (cons (simple-project-management-project-type-entry-name project-type) project-type)))


(defun simple-project-management-default-project-type ()
  (car (car simple-project-management-registered-project-types)))


(defun simple-project-management-on-kill-emacs ()
  (simple-project-management-unload-active-project))


(defun simple-project-management-read-exisisting-project-name ()
  (let (existing-project-names)
    (setq existing-project-names (simple-project-management-get-existing-project-names))
    (if existing-project-names
        (completing-read "Project name: " existing-project-names nil t)
      (error "No existing projects"))))


(defun simple-project-management-get-project-type (project-name)
  (get-config-string (simple-project-management-get-config-file project-name)
                     simple-project-management-project-type-config-key))

(defun simple-project-management-get-project-type-entry (project-type)
  (cdr (assoc project-type simple-project-management-registered-project-types)))


(defun simple-project-management-get-create-function (project-type)
  (simple-project-management-project-type-entry-create-project-function (simple-project-management-get-project-type-entry project-type)))


(defun simple-project-management-get-load-function (project-type)
  (simple-project-management-project-type-entry-load-project-function (simple-project-management-get-project-type-entry project-type)))


(defun simple-project-management-get-unload-function (project-type)
  (simple-project-management-project-type-entry-unload-project-function (simple-project-management-get-project-type-entry project-type)))


(defun simple-project-management-get-destroy-function (project-type)
  (simple-project-management-project-type-entry-destroy-project-function (simple-project-management-get-project-type-entry project-type)))


(defun simple-project-management-get-current-project ()
  simple-project-management-current-project)


(defun simple-project-management-get-existing-project-names ()
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
  (with-temp-buffer
    (insert simple-project-management-project-type-config-key " = " project-type "\n")
    (write-file (simple-project-management-get-config-file project-name))))


(defun simple-project-management-get-config-file (project-name)
  (expand-file-name simple-project-management-config-file-name
                    (simple-project-management-get-project-directory project-name)))


(defun simple-project-management-get-current-project-dir ()
  (simple-project-management-get-project-directory simple-project-management-current-project))


(defun simple-project-management-project-exists (project-name)
  (file-exists-p (simple-project-management-get-project-directory project-name)))


(defun simple-project-management-create-project-directory (project-name)
  (make-directory (simple-project-management-get-project-directory project-name) t))


(defun simple-project-management-get-project-directory (project-name)
  (expand-file-name project-name simple-project-management-projects-directory))
    

(defun simple-project-management-create (project-name)
  ())


(defun simple-project-management-load (project-name)
  (desktop-change-dir (simple-project-management-get-project-directory project-name)))


(defun simple-project-management-unload (project-name)
  ;; Unloading the desktop is just annoying..
  ())


(defun simple-project-management-destroy (project-name)
  ())



(provide 'simple-project-management)


