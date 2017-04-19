;;; pyvenv.el --- Python virtual environment interface -*- lexical-binding: t -*-

;; Copyright (C) 2013-2015  Jorgen Schaefer <contact@jorgenschaefer.de>

;; Author: Jorgen Schaefer <contact@jorgenschaefer.de>
;; URL: http://github.com/jorgenschaefer/pyvenv
;; Version: 1.9
;; Package-Requires: ((emacs "25.1"))
;; Keywords: Python, Virtualenv, Tools

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a simple global minor mode which will replicate the changes
;; done by virtualenv activation inside Emacs.

;; The main entry points are `pyvenv-activate', which queries the user
;; for a virtual environment directory to activate, and
;; `pyvenv-workon', which queries for a virtual environment in
;; $WORKON_HOME (from virtualenvwrapper.sh).

;; If you want your inferior Python processes to be restarted
;; automatically when you switch your virtual environment, add
;; `pyvenv-restart-python' to `pyvenv-post-activate-hooks'.

;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))

(require 'json)
(require 'python)

(eval-and-compile
  (unless (fboundp 'file-local-name)
    ;; Available since 25.2
    (defun file-local-name (file)
      "Return the local name component of FILE.
It returns a file name which can be used directly as argument of
`process-file', `start-file-process', or `shell-command'."
      (or (file-remote-p file 'localname) file))))

;; User customization

(defgroup pyvenv nil
  "Python Virtual Environment Interface."
  :prefix "pyvenv-"
  :group 'languages)

(defcustom pyvenv-workon-home (or (getenv "WORKON_HOME")
                                  (expand-file-name "~/.virtualenvs"))
  "Directory containing virtualenvs.

This is the value of $WORKON_HOME or ~/.virtualenvs."
  :type '(directory :must-match t)
  :safe #'file-directory-p
  :group 'pyvenv)

(defcustom pyvenv-workon nil
  "The intended virtualenv in the virtualenvwrapper directory.

This is rarely useful to set globally. Rather, set this in file-
or directory-local variables using \\[add-file-local-variable] or
\\[add-dir-local-variable].

When `pyvenv-mode' is enabled, pyvenv will switch to this
virtualenv. If a virtualenv is already enabled, it will ask first."
  :type 'pyvenv-workon
  :safe #'stringp
  :group 'pyvenv)

(defcustom pyvenv-activate nil
  "The intended virtualenv directory.

This is rarely useful to set globally. Rather, set this in file-
or directory-local variables using \\[add-file-local-variable] or
\\[add-dir-local-variable].

When `pyvenv-mode' is enabled, pyvenv will switch to this
virtualenv. If a virtualenv is already enabled, it will ask first."
  :type 'directory
  :safe #'stringp
  :group 'pyvenv)

(defcustom pyvenv-tracking-ask-before-change nil
  "Non-nil means pyvenv will ask before automatically changing a virtualenv.

This can happen when a new file is opened with a buffer-local
value (from file-local or directory-local variables) for
`pyvenv-workon' or `pyvenv-workon', or if `pyvenv-tracking-mode'
is active, after every command."
  :type 'boolean
  :group 'pyvenv)

(defcustom pyvenv-use-variable-watcher (fboundp #'add-variable-watcher)
  "Whether to use variable watchers to setup pyvenv variables."
  :type 'boolean
  :group 'pyvenv)

(defcustom pyvenv-virtualenvwrapper-python
  (or (getenv "VIRTUALENVWRAPPER_PYTHON")
      (executable-find "python")
      "python")
  "The python process which has access to the virtualenvwrapper module.

This should be $VIRTUALENVWRAPPER_PYTHON outside of Emacs, but
virtualenvwrapper.sh does not export that variable. We make an
educated guess, but that can be off."
  :type '(file :must-match t)
  :safe #'file-directory-p
  :group 'pyvenv)

;; API for other libraries

(defvar pyvenv-virtual-env nil
  "The current virtual environment.

Do not set this variable directly; use `pyvenv-activate' or
`pyvenv-workon'.")

(defvar pyvenv-virtual-env-name nil
  "The name of the current virtual environment.

This is usually the base name of `pyvenv-virtual-env'.")

(defvar pyvenv-pre-activate-hooks nil
  "Hooks run before a virtual environment is activated.

`pyvenv-virtual-env' is already set.")

(defvar pyvenv-post-activate-hooks nil
  "Hooks run after a virtual environment is activated.

`pyvenv-virtual-env' is set.")

(defvar pyvenv-pre-deactivate-hooks nil
  "Hooks run before a virtual environment is deactivated.

`pyvenv-virtual-env' is set.")

(defvar pyvenv-post-deactivate-hooks nil
  "Hooks run after a virtual environment is deactivated.

`pyvenv-virtual-env' is still set.")

(defvar pyvenv-mode-line-indicator '(pyvenv-virtual-env-name
                                     ("[" pyvenv-virtual-env-name "] "))
  "How `pyvenv-mode' will indicate the current environment in the mode line.")

;; Internal code.

(defvar pyvenv-old-process-environment nil
  "The old process environment before the last activate.")

(defvar pyvenv-old-exec-path nil
  "The old exec path before the last activate.")

(defvar pyvenv-old-tramp-remote-path nil
  "The old process environment before the last activate.")

(defvaralias 'pyvenv-shell-virtualenv (if (boundp 'python-shell-virtualenv-root)
                                          'python-shell-virtualenv-root
                                        'python-shell-virtualenv-path)
  "Alias to `python.el' virtualenv variable.")

(defvar pyvenv-virtualenv-bin (if (eq system-type 'windows-nt)
                                  "Scripts"
                                "bin"))
(defvar pyvenv-virtualenv-activate-script (if (eq system-type 'windows-nt)
                                              "activate.bat"
                                            "activate"))

(defun pyvenv-normalize-directory (file-name &optional directory)
  "Normalize FILE-NAME relative to DIRECTORY."
  (file-name-as-directory (expand-file-name file-name directory)))

(defun pyvenv-bin-directory (directory)
  "Return the bin path from a virtualenv DIRECTORY."
  (pyvenv-normalize-directory pyvenv-virtualenv-bin directory))

(defun pyvenv-virtualenv-p (directory)
  "Check if DIRECTORY is a virtualenv."
  (file-exists-p (expand-file-name pyvenv-virtualenv-activate-script (pyvenv-bin-directory directory))))

(defun pyvenv-module-installed-p (modname)
  "Check if python module MODNAME is installed."
  (zerop (process-file python-shell-interpreter nil nil nil "-c" (format "import %s" modname))))

;;;###autoload
(defun pyvenv-activate (directory)
  "Activate the virtual environment in DIRECTORY."
  (interactive "DActivate venv: ")
  (pyvenv-deactivate)
  (setq pyvenv-virtual-env (pyvenv-normalize-directory directory)
        pyvenv-virtual-env-name (file-name-nondirectory (directory-file-name pyvenv-virtual-env))
        pyvenv-shell-virtualenv pyvenv-virtual-env)
  ;; Preserve variables from being overwritten.
  (let ((old-exec-path exec-path)
        (old-process-environment process-environment))
    (unwind-protect
        (pyvenv-run-virtualenvwrapper-hook "pre_activate" pyvenv-virtual-env)
      (setq exec-path old-exec-path
            process-environment old-process-environment)))
  (run-hooks 'pyvenv-pre-activate-hooks)
  (setq pyvenv-old-exec-path exec-path
        pyvenv-old-process-environment process-environment
        ;; For some reason, Emacs adds some directories to `exec-path'
        ;; but not to `process-environment'?
        exec-path (cons (pyvenv-bin-directory pyvenv-virtual-env) exec-path)
        process-environment (append
                             (list
                              (format "VIRTUAL_ENV=%s" pyvenv-virtual-env)
                              (format "PATH=%s" (mapconcat 'identity
                                                           (cons (pyvenv-bin-directory pyvenv-virtual-env)
                                                                 (split-string (getenv "PATH")
                                                                               path-separator))
                                                           path-separator))
                              ;; No "=" means to unset
                              "PYTHONHOME")
                             process-environment)
        )
  (when (file-remote-p pyvenv-virtual-env)
    (setq pyvenv-old-tramp-remote-path tramp-remote-path
          tramp-remote-path (cons (pyvenv-bin-directory (file-local-name pyvenv-virtual-env)) tramp-remote-path)))
  (pyvenv-run-virtualenvwrapper-hook "post_activate")
  (run-hooks 'pyvenv-post-activate-hooks))

;;;###autoload
(defun pyvenv-deactivate ()
  "Deactivate any current virtual environment."
  (interactive)
  (when pyvenv-virtual-env
    (pyvenv-run-virtualenvwrapper-hook "pre_deactivate")
    (run-hooks 'pyvenv-pre-deactivate-hooks))
  (when pyvenv-old-process-environment
    (setq process-environment pyvenv-old-process-environment
          pyvenv-old-process-environment nil))
  (when pyvenv-old-exec-path
    (setq exec-path pyvenv-old-exec-path
          pyvenv-old-exec-path nil))
  (when pyvenv-old-tramp-remote-path
    (setq tramp-remote-path pyvenv-old-tramp-remote-path
          pyvenv-old-tramp-remote-path nil))
  (when pyvenv-virtual-env
    ;; Make sure this does not change `exec-path', as $PATH is
    ;; different
    (let ((old-exec-path exec-path)
          (old-process-environment process-environment))
      (unwind-protect
          (pyvenv-run-virtualenvwrapper-hook "post_deactivate"
                                             pyvenv-virtual-env)
        (setq exec-path old-exec-path
              process-environment old-process-environment))))
  (setq pyvenv-virtual-env nil
        pyvenv-virtual-env-name nil
        pyvenv-shell-virtualenv nil)
  (run-hooks 'pyvenv-post-deactivate-hooks))

(defvar pyvenv-workon-history nil
  "Prompt history for `pyvenv-workon'.")

;;;###autoload
(defun pyvenv-workon (name)
  "Activate a virtual environment from $WORKON_HOME."
  (interactive
   (list (completing-read "Work on: " (pyvenv-virtualenv-list) nil t)))
  (pyvenv-activate (expand-file-name name pyvenv-workon-home)))

(defun pyvenv-virtualenv-list (&optional full)
  "Prompt the user for a name in `venv-workon-home'.

If FULL is non nil will return the full path to virtualenvs."
  (let* ((directories (directory-files pyvenv-workon-home 'full directory-files-no-dot-files-regexp))
         (virtualenvs (seq-filter #'pyvenv-virtualenv-p directories)))
    (if full virtualenvs (mapcar #'file-name-nondirectory virtualenvs))))

(declare-function widget-copy "wid-edit")
(declare-function widget-types-convert-widget "wid-edit")

(define-widget 'pyvenv-workon 'choice
  "Select an available virtualenv from virtualenvwrapper."
  :convert-widget
  (lambda (widget)
    (setq widget (widget-copy widget))
    (widget-put widget
                :args (cons '(const :tag "None" nil)
                            (mapcar (lambda (env)
                                      (list 'const env))
                                    (pyvenv-virtualenv-list))))
    (widget-types-convert-widget widget))

  :prompt-value (lambda (_widget prompt _value _unbound)
                  (let ((name (completing-read
                               prompt
                               (cons "None"
                                     (pyvenv-virtualenv-list))
                               nil t)))
                    (if (equal name "None")
                        nil
                      name))))

(defvar pyvenv-mode-map (make-sparse-keymap)
  "The mode keymap for `pyvenv-mode'.")

(easy-menu-define pyvenv-menu pyvenv-mode-map
  "Pyvenv Menu"
  '("Virtual Envs"
    :visible pyvenv-mode
    ("Workon"
     :help "Activate a virtualenvwrapper environment"
     :filter (lambda (&optional ignored)
               (mapcar (lambda (venv)
                         (vector venv `(pyvenv-workon ,venv)
                                 :style 'radio
                                 :selected `(equal pyvenv-virtual-env-name
                                                   ,venv)))
                       (pyvenv-virtualenv-list))))
    ["Activate" pyvenv-activate
     :help "Activate a virtual environment by directory"]
    ["Deactivate" pyvenv-deactivate
     :help "Deactivate the current virtual environment"
     :active pyvenv-virtual-env
     :suffix pyvenv-virtual-env-name]
    ["Restart Python Processes" pyvenv-restart-python
     :help "Restart all Python processes to use the current environment"]))

(defun pyvenv-variable-watcher (symbol newval operation _where)
  "Variable watcher for pyvenv.

SYMBOL, NEWVAL, OPERATION, and WHERE"
  (cl-case operation
    (set (cl-case symbol
           (pyvenv-activate (and (not (string= (pyvenv-normalize-directory newval) pyvenv-virtual-env))
                                 (or (not pyvenv-tracking-ask-before-change)
                                     (y-or-n-p (format "Switch to virtualenv %s (currently %s)? " newval pyvenv-virtual-env)))
                                 (pyvenv-activate newval)))
           (pyvenv-workon (and (not (string= newval pyvenv-virtual-env-name))
                               (or (not pyvenv-tracking-ask-before-change)
                                   (y-or-n-p (format "Switch to virtualenv %s (currently %s)? " newval pyvenv-virtual-env-name)))
                               (pyvenv-workon newval)))))))

;;;###autoload
(define-minor-mode pyvenv-mode
  "Global minor mode for pyvenv.

Will show the current virtualenv in the mode line, and respect a
`pyvenv-workon' setting in files."
  :global t
  (cond
   (pyvenv-mode
    (add-to-list 'mode-line-misc-info '(pyvenv-mode pyvenv-mode-line-indicator))
    (if (and pyvenv-use-variable-watcher (fboundp #'add-variable-watcher))
        (dolist (symbol '(pyvenv-workon pyvenv-activate))
          (add-variable-watcher symbol #'pyvenv-variable-watcher))
      (add-hook 'hack-local-variables-hook #'pyvenv-track-virtualenv)))
   ((not pyvenv-mode)
    (setq mode-line-misc-info (delete '(pyvenv-mode pyvenv-mode-line-indicator)
                                      mode-line-misc-info))
    (if (and pyvenv-use-variable-watcher (fboundp #'remove-variable-watcher))
        (dolist (symbol '(pyvenv-workon pyvenv-activate))
          (remove-variable-watcher symbol #'pyvenv-variable-watcher))
      (remove-hook 'hack-local-variables-hook #'pyvenv-track-virtualenv)))))

;;;###autoload
(define-minor-mode pyvenv-tracking-mode
  "Global minor mode to track the current virtualenv.

When this mode is active, pyvenv will activate a buffer-specific
virtualenv whenever the user switches to a buffer with a
buffer-local `pyvenv-workon' or `pyvenv-activate' variable."
  :global t
  (if pyvenv-tracking-mode
      (add-hook 'post-command-hook 'pyvenv-track-virtualenv)
    (remove-hook 'post-command-hook 'pyvenv-track-virtualenv)))

(defun pyvenv-track-virtualenv ()
  "Set a virtualenv as specified for the current buffer.

If either `pyvenv-activate' or `pyvenv-workon' are specified, and
they specify a virtualenv different from the current one, switch
to that virtualenv."
  (cond
   (pyvenv-activate
    (when (and (not (equal (pyvenv-normalize-directory pyvenv-activate)
                           pyvenv-virtual-env))
               (or (not pyvenv-tracking-ask-before-change)
                   (y-or-n-p (format "Switch to virtualenv %s (currently %s)"
                                     pyvenv-activate pyvenv-virtual-env))))
      (pyvenv-activate pyvenv-activate)))
   (pyvenv-workon
    (when (and (not (equal pyvenv-workon pyvenv-virtual-env-name))
               (or (not pyvenv-tracking-ask-before-change)
                   (y-or-n-p (format "Switch to virtualenv %s (currently %s)"
                                     pyvenv-workon pyvenv-virtual-env-name))))
      (pyvenv-workon pyvenv-workon)))))

(defun pyvenv-run-virtualenvwrapper-hook (hook &rest args)
  "Run a virtualenvwrapper hook, and update the environment.

This will run a virtualenvwrapper hook and update the local
environment accordingly.

CAREFUL! This will modify your `process-environment' and
`exec-path'."
  (when (pyvenv-module-installed-p "virtualenvwrapper")
    (with-temp-buffer
      (let ((tmpfile (make-temp-file "pyvenv-virtualenvwrapper-")))
        (unwind-protect
            (progn
              (apply #'call-process
                     pyvenv-virtualenvwrapper-python
                     nil t nil
                     "-c"
                     "from virtualenvwrapper.hook_loader import main; main()"
                     "--script" tmpfile
                     (if (getenv "HOOK_VERBOSE_OPTION")
                         (cons (getenv "HOOK_VERBOSE_OPTION")
                               (cons hook args))
                       (cons hook args)))
              (call-process-shell-command
               (format ". '%s' ; python -c 'import os, json; print(\"\\n=-=-=\"); print(json.dumps(dict(os.environ)))'"
                       tmpfile)
               nil t nil))
          (delete-file tmpfile)))
      (goto-char (point-min))
      (when (and (not (re-search-forward "ImportError: No module named '?virtualenvwrapper'?" nil t))
                 (re-search-forward "\n=-=-=\n" nil t))
        (let ((output (buffer-substring (point-min)
                                        (match-beginning 0))))
          (when (> (length output) 0)
            (with-help-window "*Virtualenvwrapper Hook Output*"
              (with-current-buffer "*Virtualenvwrapper Hook Output*"
                (let ((inhibit-read-only t))
                  (erase-buffer)
                  (insert
                   (format
                    "Output from the virtualenvwrapper hook %s:\n\n"
                    hook)
                   output))))))
        (dolist (binding (json-read))
          (let ((env (format "%s=%s" (car binding) (cdr binding))))
            (when (not (member env process-environment))
              (setq process-environment (cons env process-environment))))
          (when (eq (car binding) 'PATH)
            (setq exec-path (split-string (cdr binding)
                                          path-separator))))))))

;;;###autoload
(cl-defun pyvenv-restart-python-buffer (&optional (buffer (current-buffer)))
  "Restart Python inferior processes in BUFFER."
  (interactive)
  (if-let (process (get-buffer-process buffer))
      (with-current-buffer buffer
        (cl-assert (not (file-remote-p default-directory)) nil "`%s' does not support tramp shells yet" this-command)
        (let ((cmd (combine-and-quote-strings (process-command process)))
              (dedicated (string-match-p "\\[.*\\]\\*" (buffer-name))))
          (delete-process process)
          (goto-char (point-max))
          (insert "\n\n"
                  "###\n"
                  (format "### Restarting in virtualenv %s (%s)\n"
                          pyvenv-virtual-env-name pyvenv-virtual-env)
                  "###\n"
                  "\n\n")
          (cl-letf (((symbol-function #'python-shell-get-process-name)
                     (lambda (_dedicated) (string-remove-prefix "*" (string-remove-suffix "*" (buffer-name))))))
            (run-python cmd dedicated))
          (goto-char (point-max))))
    (user-error "Not process associated to buffer %S" buffer)))

;;;###autoload
(defun pyvenv-restart-python ()
  "Restart all the Python inferior processes."
  (interactive)
  (dolist (buffer (buffer-list))
    (and (eq (buffer-local-value 'major-mode buffer) 'inferior-python-mode)
         (pyvenv-restart-python-buffer buffer))))

(provide 'pyvenv)
;;; pyvenv.el ends here
