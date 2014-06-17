;;; popup-keys-examples.el --- example popups

;; Copyright © 2014 Joseph Rabinoff.

;; Author: Joseph Rabinoff <rabinoff@post.harvard.edu>
;; Maintainer: Joseph Rabinoff <rabinoff@post.harvard.edu>
;; Keywords: convenience, tools
;; URL: http://github.com/QBobWatson/popup-keys

;; This program is free software: you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later
;; version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Example configurations for popup-keys popups.  This file is by no means meant
;; to contain a comprehensive list of applications of popup-keys.  Presumably
;; you will want to configure your own.  Contributions of popups of a generally
;; useful or illustrative nature are welcome.

;; To use one or more of the popups in this file, put it and popup-keys.el
;; somewhere Emacs can find it and require it using
;;
;;     (require 'popup-keys-examples)
;;
;; Explicitly requiring the file may not be necessary if your package system
;; extracted the autoloads correctly.  You will probably want to bind the popups
;; you want to use to keys.  If the popup is named `vc', for example, then the
;; command to bind will be called `popup-keys:run-vc'.  See below for Lisp code
;; to make the suggested keybindings.

;; This file should take negligible time to load as all initialization is done
;; lazily; loading this file simply stores some lists in a hash table and
;; defines some auxiliary functions and variables.  Requiring this file does
;; *not* have any other side-effects.  In particular, it doesn't install any
;; keybindings -- see the comments with the examples for example keybindings.
;; Popups in this file are optimized for 100-character wide windows.

;; Please see the README.org file at http://github.com/QBobWatson/popup-keys
;; for detailed usage information.

;;; Code:

(require 'popup-keys)

(eval-when-compile
  (require 'ibuffer)
  (require 'ibuf-ext))

(defvar popups:header-max-len 70
  "Maximum length of strings in header lines.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * Find

;; Popup to dispatch to various advanced file finding tools.  This is just an
;; interactive keymap, the simplest kind of popup.

;; You can bind this popup to a key using:

;; (global-set-key (kbd "C-x M-/") 'popup-keys:run-findtool)

;;;###autoload (autoload 'popup-keys:run-findtool "popup-keys-examples" "Popup named findtool" t)
(popup-keys:new
 'popup-keys:run-findtool
 :buf-name "*find tools*"
 :actions '(("d" "find-name-dired" find-name-dired)
            ("D" "find-dired" find-dired)
            ("h" "helm-find (C-u: prompt)" helm-find)
            ("l" "helm-locate" helm-locate)
            ("a" "ack-find-file" ack-find-file)
            ("A" "ack-find-file-same" ack-find-file-same)
            ("F" "helm-for-files" helm-for-files)
            ("f" "helm-find-files" helm-find-files)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * Debug commands

;; Popup for Emacs debug commands and context.  This popup shows the current
;; state of `debug-on-error', etc. and has some debug actions.

;; Suggested keybinding:

;; (global-set-key (kbd "C-x D") 'popup-keys:run-debug-commands)

;;;###autoload (autoload 'popup-keys:run-debug-commands "popup-keys-examples" "Popup named debug-commands" t)
(popup-keys:new
 'popup-keys:run-debug-commands
 :header #'(lambda (popup)
             (require 'debug)
             (defvar debug-on-error)
             (defvar debug-on-quit)
             (defvar debug-function-list)
             (insert
              (propertize "Debug on error:        " 'face 'font-lock-keyword-face)
              (propertize (if debug-on-error "Yes" "No")
                          'face 'font-lock-constant-face)
              "\n"
              (propertize "Debug on quit:         " 'face 'font-lock-keyword-face)
              (propertize (if debug-on-quit "Yes" "No")
                          'face 'font-lock-constant-face)
              "\n"
              (propertize "Debug entry functions: " 'face 'font-lock-keyword-face)
              (propertize (prin1-to-string debug-function-list)
                          'face 'font-lock-constant-face)
              "\n\n"
              ))
 :actions '(("d" "toggle debug on error" toggle-debug-on-error)
            ("x" "toggle debug on quit" toggle-debug-on-quit)
            ("e" "debug on entry" debug-on-entry)
            ("E" "cancel debug on entry" cancel-debug-on-entry)
            ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * VC

;; Popup for Emacs version control commands and context.  This popup shows some
;; VC information about the current buffer's file and displays an (almost stock)
;; interactive VC keymap (C-x v).

;; Suggested keybinding:

;; (eval-after-load "vc-hooks"
;;   '(progn
;;      ;; move original keymap
;;      (define-key ctl-x-map "V" 'vc-prefix-map)
;;      ;; run popup on original key
;;      (define-key ctl-x-map "v" 'popup-keys:run-vc)
;;      ))
;; (global-set-key (kbd "C-x v") 'popup-keys:run-vc)

;;;###autoload (autoload 'popup-keys:run-vc "popup-keys-examples" "Popup named vc" t)
(popup-keys:new
 'popup-keys:run-vc
 :buf-name "*VC keys*"
 ;; header function to display context
 :header #'(lambda (popup)
             (let ((file (buffer-file-name popup-keys:orig-buffer)))
               (if file
                   (progn
                     (insert (propertize "Buffer file: "
                                         'face 'font-lock-keyword-face)
                             (propertize file 'face 'font-lock-constant-face)
                             "\n")
                     (let ((backend (vc-backend file)))
                       (if backend
                           (insert (propertize "Backend:     "
                                               'face 'font-lock-keyword-face)
                                   (propertize (symbol-name backend)
                                               'face 'font-lock-constant-face)
                                   "\n"
                                   (propertize "State:       "
                                               'face 'font-lock-keyword-face)
                                   (propertize
                                    (let ((state (vc-state file backend)))
                                      (cond ((symbolp state)
                                             (symbol-name state))
                                            ((stringp state)
                                             state)
                                            (t
                                             "Locally modified")))
                                    'face 'font-lock-constant-face)
                                   "\n"
                                   (propertize "Current rev: "
                                               'face 'font-lock-keyword-face)
                                   (propertize (vc-working-revision file backend)
                                               'face 'font-lock-constant-face)
                                   "\n")
                         (insert (propertize "File is not known to be under version control\n"
                                             'face 'warning)))))
                 (insert (propertize "Buffer does not have an associated file\n"
                                     'face 'warning))))
             (insert "\n"))

 :more-help (popup-keys:info-node "(emacs) Version Control")
 :actions '(("v" "dwim next action"    vc-next-action)
            ("d" "vc directory"        vc-dir)

            ("i" "register file"       vc-register :keepbuf t)
            ("+" "pull from remote"    vc-update :keepbuf t)
            ("m" "merge branch"        vc-merge)
            ("~" "visit revision"      vc-revision-other-window)
            ("u" "revert"              vc-revert)
            ("s" "create tag"          vc-create-tag)
            ("r" "revert to tag"       vc-retrieve-tag)

            ("g" "annotate w/revs"     vc-annotate)
            ("l" "log fileset"         vc-print-log)
            ("L" "log repository"      vc-print-root-log)
            ("I" "log incoming"        vc-log-incoming)
            ("O" "log outgoing"        vc-log-outgoing)

            ("=" "diff fileset (C-u)"  vc-diff)
            ("e" "ediff files (C-u)"   vc-ediff)
            ("D" "diff workdir (C-u)"  vc-root-diff)

            ("a" "update changelog"    vc-update-change-log)
            ("b" "switch backend"      vc-switch-backend)
            ("c" "rollback change"     vc-rollback)
            ("h" "insert headers"      vc-insert-headers)
            ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * Projectile

;; Popup for Projectile commands.  You can get projectile from
;;    http://github.com/bbatsov/projectile
;; or from el-get or melpa.

;; This popup mainly serves as an interactive version of the Projectile keymap
;; (C-c p).  It also displays the current Projectile project directory, and
;; allows you to change the directory before running any of the commands.

;; Suggested keybindings:

;; ;; Set this before requiring 'projectile.
;; (setq projectile-keymap-prefix (kbd "C-c P"))
;; (global-set-key (kbd "C-c p") 'popup-keys:run-projectile)

;;;###autoload (autoload 'popup-keys:run-projectile "popup-keys-examples" "Popup named projectile" t)
(popup-keys:new
 'popup-keys:run-projectile
 :buf-name "*Projectile dispatcher*"
 :actions '(("h" "helm-projectile"     helm-projectile)
            ("f" "find file"           projectile-find-file)
            ("F" "find file in projs"  projectile-find-file-in-known-projects)
            ("4f" "... in other win"   projectile-find-file-other-window)
            ("e" "recent files"        projectile-recentf)
            ;; alternate keybinding (invisible)
            ("M-h" nil                 projectile-recentf)
            ("d" "find dir"            projectile-find-dir)
            ("4d" "... in other win"   projectile-find-dir-other-window)
            ("D" "dired proj root"     projectile-dired)
            ("v" "vc-dir on project"   projectile-vc)

            ("b" "switch buffer"       projectile-switch-to-buffer)
            ("4b" "... in other win"   projectile-switch-to-buffer-other-window)
            ("I" "ibuffer"             projectile-ibuffer)
            ("B" "most recent buffer"  projectile-project-buffers-other-buffer)
            ("4 C-o" "open other win"  projectile-display-buffer)
            ("k" "kill proj bufs"      projectile-kill-buffers)
            ("S" "save proj bufs"      projectile-save-project-buffers)

            ("a" "ack"                 projectile-ack)
            ("A" "ag"                  projectile-ag)
            ("g" "grep"                projectile-grep)
            ;; alternate keybinding (invisible)
            ("M-g" nil                 projectile-grep)
            ("o" "moccur"              projectile-multi-occur)
            ("j" "etags"               projectile-find-tag)
            ;; alternate keybinding (invisible)
            ("M-." nil                 projectile-find-tag)
            ("R" "regenerate etags"    projectile-regenerate-tags)
            ("r" "tags query replace"  projectile-replace)

            ("c" "compile project"     projectile-compile-project)
            ("p" "test project"        projectile-test-project)
            ("t" "toggle testing"      projectile-toggle-between-implementation-and-test)
            ("4t" "find test o/win"    projectile-find-implementation-or-test-other-window)
            ("T" "find test file"      projectile-find-test-file)

            ("!" "shell cmd in root"   projectile-run-shell-command-in-root)
            ("&" "async cmd in root"   projectile-run-async-shell-command-in-root)

            ("s" "switch project"      projectile-switch-project)
            ("l" "find in other dir"   projectile-find-file-in-directory)

            ("i" "invalidate cache"    projectile-invalidate-cache)
            ("z" "cache current file"  projectile-cache-current-file))

 ;; Argument to set the project directory before running a command
 :arguments '(("-d" "project directory" :projectdir
               :read (read-directory-name "New project directory: " curval)
               :help "Most Projectile commands will not work without a valid root directory"))

 :setup '(progn (require 'projectile) (popups:projectile-get-proot))
 :pre-action 'popups:projectile-set-defaultdir
 :post-arg 'popups:projectile-arg)

;; ** functions
(defun popups:projectile-get-proot (&optional popup)
  "Set popup argument :projectdir to the project root of `default-directory'.
POPUP is unused."
  (ignore popup)
  (declare-function projectile-project-p "projectile")
  (let ((proot (projectile-project-p)))
    (setq popup-keys:current-args
          (plist-put popup-keys:current-args
                     :projectdir (or proot default-directory)))
    (unless proot
      (message "Warning: %s is not in a project" default-directory))))

(defun popups:projectile-set-defaultdir (key)
  "Set `default-directory' from `popup-keys:action-args' when running an action.

This function is run as a hook when an action is executed.  The return value of
this function is let-bound before the action is actually run.  We use this to
set `default-directory' to the current value of the :projectdir argument.  KEY
is unused."
  (ignore key)
  (let ((arg (plist-get popup-keys:action-args :projectdir)))
    (when (and arg (not (string= arg "")))
      ;; return arguments get let-bound
      `((default-directory ,arg)))))

(defun popups:projectile-arg (arg val)
  "Hook function called when an argument is changed.

When ARG is :projectdir, translate VAL into a project root."
  (when (eq arg :projectdir)
    (let ((default-directory val))
      (popups:projectile-get-proot))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * kmacro

;; This is a much more advanced (and more useful) example of a popup.  It
;; displays context and an interactive keymap for defining keyboard macros
;; (normally C-x C-k).  This is extremely convenient because the one time you
;; can't stop and look up a key which, for example, inserts the value of the
;; current keyboard macro counter, is when you're defining a keyboard macro.
;; This popup has the following additional features:
;;
;; + It takes advantage of the :keepbuf argument to popup actions, which allows
;;   the action to be executed without closing the popup window.  So to execute
;;   the second keyboard macro in the macro ring repeatedly, open the popup and
;;   just keep pressing "C-l".  This is an elegant way of implementing the
;;   repeat-key functionality of the standard kmacro key bindings.
;;
;; + It dynamically adds new actions to the popup when you bind a keyboard macro
;;   to a number or a capital letter.  Running the action keeps the buffer open,
;;   as above.  The help text for that action is the expanded macro definition.
;;
;; + It mangles the keyboard input so that you can open the popup buffer while
;;   defining a keyboard macro.  For example, to insert the current value of the
;;   keyboard macro counter, open the popup and hit "c"; the counter is
;;   inserted, and the key sequence "C-x C-S-K TAB" is recorded in the current
;;   keyboard macro definition.  This also works well with prefix arguments, for
;;   use e.g. with `kmacro-set-counter'.

;; In order for this to work, you should use the following keybindings.  If you
;; change the second, you need to change the keybinding mangling options in the
;; popup definitions and in `popups:kmacro-bind-to-key'.

;; (global-set-key (kbd "C-x C-k")   'popup-keys:run-kmacro)
;; (global-set-key (kbd "C-x C-S-k") 'kmacro-keymap)

;;;###autoload (autoload 'popup-keys:run-kmacro "popup-keys-examples" "Popup named kmacro" t)
(popup-keys:new
 'popup-keys:run-kmacro
 :buf-name "*kmacro keys*"
 :more-help (popup-keys:info-node "(emacs) Keyboard Macros")
 :header #'(lambda (popup)
             (insert
              (propertize (format "Macros (%d total):\n"
                                  (+ (length kmacro-ring)
                                     (if last-kbd-macro 1 0)))
                          'face 'font-lock-keyword-face)
              (propertize "  last " 'face 'font-lock-function-name-face)
              (propertize (if last-kbd-macro
                              (popups:truncate-string
                               (format-kbd-macro last-kbd-macro 1)
                               popups:header-max-len
                               "[...]")
                            "<none>")
                          'face 'font-lock-constant-face)
              "\n"
              (propertize "   2nd " 'face 'font-lock-function-name-face)
              (propertize (if kmacro-ring
                              (popups:truncate-string
                               (format-kbd-macro (car (car kmacro-ring)) 1)
                               popups:header-max-len
                               "[...]")
                            "<none>")
                          'face 'font-lock-constant-face)
              "\n\n"
              ))
 ;; Tell the keyboard macro facility to forget about the keys used to open the
 ;; popup.
 :setup '(progn (require 'kmacro) (cancel-kbd-macro-events))
 ;; Disable kmacro-repeat functionality since it's not needed.  This list gets
 ;; let-bound.
 :pre-action '(quote ((kmacro-repeat-no-prefix nil)))
 ;; Don't use the `-repeat' variants; instead use :keepbuf t.
 :actions `(("C-s" "start macro"       kmacro-start-macro)
            ;; alternate keybinding (invisible)
            ("s" nil                   kmacro-start-macro)
            ("C-k" "end or run macro"  kmacro-end-or-call-macro :keepbuf t)
            ("r" "apply to region"     apply-macro-to-region-lines)
            ("C-l" "run second macro"  kmacro-call-ring-2nd :keepbuf t)

            ("C-e" "edit macro"        kmacro-edit-macro)
            ("SPC" "step-edit macro"   kmacro-step-edit-macro)
            ("b" "bind macro"          popups:kmacro-bind-to-key :keepbuf t)
            ("n" "name macro"          kmacro-name-last-macro :keepbuf args)
            ("e" "edit bound macro"    edit-kbd-macro)
            ("l" "macro from lossage"  kmacro-edit-lossage)
            ("C-v" "view prev macro"   kmacro-view-macro :keepbuf t)

            ("C-n" "next ring macro"   kmacro-cycle-ring-next :keepbuf t)
            ("C-p" "prev ring macro"   kmacro-cycle-ring-previous :keepbuf t)
            ("C-d" "delete from ring"  kmacro-delete-ring-head :keepbuf t)
            ("C-t" "swap top two ring" kmacro-swap-ring :keepbuf t)

            ;; Dealing with counters, etc. in a macro are not handled specially
            ;; -- they're stored as key sequences.  Hence the macro mangling
            ;; here.  Don't try to do anything other than this from this popup
            ;; while defining a macro, other than ending the macro!  (Using
            ;; other popups in macros should work fine.)
            ;; don't rebind TAB here
            ("c" "insert counter"      kmacro-insert-counter
             :macro-keys "C-x C-S-K TAB")
            ("C-a" "add arg to ctr"    kmacro-add-counter
             :macro-keys "C-x C-S-K C-a")
            ("C-c" "set ctr to arg"    kmacro-set-counter
             :macro-keys "C-x C-S-K C-c")
            ("C-f" "set ctr format"    kmacro-set-format
             :macro-keys "C-x C-S-K C-f")
            ;; don't rebind "q"
            ("M-q" "query user"        kbd-macro-query
             :macro-keys "C-x C-S-K q")
            ))

;; ** functions
(defun popups:truncate-string (str len &optional ellipsis)
  "Truncate STR to LEN chars, inserting ELLIPSIS if necessary."
  (setq ellipsis (or ellipsis ""))
  (let* ((str-len  (length str))
         (too-long (> str-len len)))
    (if too-long
        (setq len (- len (length ellipsis)))
      (setq len str-len))
    (format "%s%s"
            (substring-no-properties
             str 0 len)
            (if too-long ellipsis ""))))

(defun popups:kmacro-bind-to-key ()
  "Modified version of `kmacro-bind-to-key'.

Works the same, but entering a binding of 0 through 9 or A through Z places the
binding in the `popup-keys:run-kmacro' menu."
  (interactive)
  (require 'kmacro)
  (declare-function kmacro-ring-head "kmacro")
  (declare-function kmacro-lambda-form "kmacro")
  (if (or defining-kbd-macro executing-kbd-macro)
      (if defining-kbd-macro
	  (message "Cannot save macro while defining it."))
    (unless last-kbd-macro
      (error "No keyboard macro defined"))
    (let* ((key-seq (read-key-sequence "Bind last macro to key: "))
           (ch (and (= (length key-seq) 1) (aref key-seq 0)))
           (to-bind (kmacro-lambda-form (kmacro-ring-head)))
           ok-to-rebind cmd)
      (when (and (integerp ch)
                 (or (and (>= ch ?0) (<= ch ?9))
                     (and (>= ch ?A) (<= ch ?Z))))
        ;; Insert into the popup-keys menu.  Also bind it under C-x C-S-K for use
        ;; in keyboard macros.
        (let* ((formatted (format-kbd-macro last-kbd-macro 1))
               (descr (popups:truncate-string formatted 17 ".."))
               (chstr (char-to-string ch))
               (newseq (concat "C-x C-S-K " chstr)))
          (popup-keys:add-thing
           'kmacro 'action
           chstr (concat "[" descr "]") to-bind
           :pre-action '(progn (unless defining-kbd-macro
                                 (setq popup-keys:keep-buffer t))
                               nil)
           :macro-keys newseq
           :help (concat "User-defined keyboard macro:\n"
                         "----------------------------\n"
                         (format-kbd-macro last-kbd-macro t)))
          (setq key-seq (kbd newseq))
          (setq ok-to-rebind t)))
      (when (and (not (equal key-seq ""))
                 (or ok-to-rebind
                     (not (setq cmd (key-binding key-seq)))
                     (stringp cmd)
                     (vectorp cmd)
                     (yes-or-no-p (format "%s runs command %S.  Bind anyway? "
                                          (format-kbd-macro key-seq) cmd))))
        (define-key global-map key-seq to-bind)
        (message "Keyboard macro bound to %s" (format-kbd-macro key-seq))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * registers

;; Interactive version of the multipurpose register/rectangle/bookmark keymap
;; (C-x r).  This is quite useful as it displays the contents of several
;; registers.  It also keeps the popup window open while interactively prompting
;; in several commands so you can see what you're doing.

;; Suggested keybindings:

;; (global-set-key (kbd "C-x r") 'popup-keys:run-registers)
;; (global-set-key (kbd "C-x R") ctl-x-r-map)
;; ;; undo-tree annoyingly binds to the C-x r prefix and overrides the above.
;; (eval-after-load "undo-tree"
;;   '(define-key undo-tree-map (kbd "C-x r") nil))

;;;###autoload (autoload 'popup-keys:run-registers "popup-keys-examples" "Popup named registers" t)
(popup-keys:new
 'popup-keys:run-registers
 :header 'popups:register-header
 :more-help (popup-keys:info-node "(emacs) Registers")
 :actions `(("DEL" "delete register"   popups:delete-register :keepbuf args)
            ("SPC" "point to reg"      point-to-register :keepbuf args)
            ;; alternate keybindings (invisible)
            ("C-@" nil                 point-to-register :keepbuf args)
            ("C-SPC" nil               point-to-register :keepbuf args)
            ("F" "file pos to reg"     popups:point-to-file-register :keepbuf args)
            ("M-f" "file name to reg"  popups:fname-to-file-register :keepbuf args)
            ("j" "jump to reg"         jump-to-register :keepbuf args)

            ("s" "region to reg"       copy-to-register :keepbuf args)
            ;; alternate keybinding (invisible)
            ("x" nil                   copy-to-register :keepbuf args)
            ("a" "append to reg"       append-to-register :keepbuf args)
            ("p" "prepend to reg"      prepend-to-register :keepbuf args)
            ("i" "insert reg"          insert-register :keepbuf args)
            ;; alternate keybinding (invisible)
            ("g" nil                   insert-register :keepbuf args)

            ("n" "number to reg"       number-to-register :keepbuf args)
            ("+" "increment reg"       increment-register :keepbuf t)

            ("f" "frame conf to reg"   frame-configuration-to-register :keepbuf args)
            ("w" "window conf to reg"  window-configuration-to-register :keepbuf args)

            ("k" "kill rectangle"      kill-rectangle)
            ("M-w" "copy rect as kill" copy-rectangle-as-kill)
            ("y" "yank rectangle"      yank-rectangle)
            ("r" "rectangle to reg"    copy-rectangle-to-register :keepbuf args)
            ("c" "clear rectangle"     clear-rectangle)
            ("o" "open rectangle"      open-rectangle)
            ("d" "delete rectangle"    delete-rectangle)
            ("t" "fill rect with str"  string-rectangle)
            ("N" "rect number lines"   rectangle-number-lines)

            ("m" "set bookmark"        bookmark-set)
            ("b" "jump to bookmark"    bookmark-jump)
            ("D" "delete bookmark"     bookmark-delete)
            ("l" "bookmark list"       bookmark-bmenu-list)

            ("u" "undo state to reg"   undo-tree-save-state-to-register :keepbuf args)
            ("U" "restore undo state"  undo-tree-restore-state-from-register :keepbuf args)
            ("M-h" nil                 helm-register)
            ))

;; ** header
(defvar popups:register-max-regs 5
  "Display this many registers in the registers popup header.")

;; Is there really no builtin function for this?
(defun popups:quote-escape-string (str)
  "Replace some control characters with their escaped equivalents in STR."
  (setq str (replace-regexp-in-string "[\t]" "\\t" str t t))
  (setq str (replace-regexp-in-string "[\n]" "\\n" str t t))
  (setq str (replace-regexp-in-string "[\r]" "\\r" str t t))
  (setq str (replace-regexp-in-string "[\f]" "\\f" str t t)))

(defun popups:register-header (popup)
  "Display registers.  POPUP is unused."
  (insert (propertize (format "Registers (%d total):\n"
                              (length register-alist))
                      'face 'font-lock-keyword-face))
  (let ((regs (sort (copy-sequence register-alist)
                    (lambda (x y) (< (car x) (car y))))))
    ;; copied from `helm-register-candidate'
    (loop
     for (char . val) in regs
       for i = 1 then (1+ i)
       for key = (single-key-description char)
       if (<= i popups:register-max-regs) do
         (insert
          (propertize (format "%5s " key)
                      'face 'font-lock-function-name-face)
          (propertize
           (cond
            ((numberp val)
             (format "number: %d" val))
            ((markerp val)
             (let ((buf (marker-buffer val)))
               (if (null buf)
                   "a marker in no buffer"
                 (format "marker: %s, position %d"
                         (buffer-name buf) (marker-position val)))))
            ((and (consp val) (window-configuration-p (car val)))
             "<window configuration>")
            ((and (consp val) (frame-configuration-p (car val)))
             "<frame configuration>")
            ((and (consp val) (eq (car val) 'file))
             (format "file: %s" (file-name-nondirectory (cdr val))))
            ((and (consp val) (eq (car val) 'file-query))
             (format "file (query): %s, position %d"
                     (file-name-nondirectory (cadr val)) (caddr val)))
            ((consp val) ; rectangle
             (let ((str (popups:quote-escape-string (car val))))
               (format "rectangle (%d lines): \"%s\"%s"
                       (length val)
                       (popups:truncate-string str popups:header-max-len
                                            "[...]")
                       (if (> (length val) 1) " etc." ""))))
            ((stringp val)
             (let ((str (popups:quote-escape-string val)))
               (concat "\""
                       (popups:truncate-string str popups:header-max-len
                                               "[...]")
                       "\"")))
            ((vectorp val)
             "<undo-tree entry>")
            (t
             "???"))
           'face 'font-lock-constant-face)
          "\n")
       else return nil
     ))
  (insert "\n"))

;; ** functions
(defun popups:delete-register (register)
  "Delete REGISTER from `register-alist'."
  (interactive "cDelete register: \n")
  (setq register-alist
        (delete (assoc register register-alist) register-alist)))

(defun popups:point-to-file-register (register)
  "Add a file-query reference to REGISTER."
  (interactive "cFile query to register: \n")
  (if buffer-file-name
      (set-register register
                    (list 'file-query buffer-file-name (point)))
    (message "Buffer is not associated with a file.")))

(defun popups:fname-to-file-register (register)
  "Add a filename to REGISTER."
  (interactive "cFile name to register: \n")
  (if buffer-file-name
      (set-register register
                    (cons 'file buffer-file-name))
    (message "Buffer is not associated with a file.")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * Dired

;; This is an example of a popup that essentially just displays the active
;; keymap for a buffer in a special mode, i.e. a mode where most keys are mapped
;; to commands (and not `self-insert-command').  This takes advantage of the
;; fact that actions can be executed in the original buffer without closing the
;; popup buffer.

;; Have to change bindings in the mode hook.  It doesn't work to do this
;; after-load "dired" because dired-x defines commands in the * and % prefixes.
;; Even after-load "dired-x" doesn't work, maybe because dired-x loads lazily.

;; (add-hook 'dired-mode-hook
;;           (lambda ()
;;             (require 'dired-x)
;;             (define-key dired-mode-map (kbd "?") 'popup-keys:run-dired)
;;             (define-key dired-mode-map (kbd "%") 'popup-keys:run-dired-regexp)
;;             (define-key dired-mode-map (kbd "*") 'popup-keys:run-dired-mark)))

;; ** general
;;;###autoload (autoload 'popup-keys:run-dired "popup-keys-examples" "Popup named dired" t)
(popup-keys:new
 'popup-keys:run-dired
 :more-help (popup-keys:info-node "(emacs) Dired")
 :actions '(;; navigation
            ("n" "next line"           dired-next-line :keepbuf t)
            ;; alternate keybindings (invisible)
            ("SPC" nil                 dired-next-line :keepbuf t)
            ("C-n" nil                 dired-next-line :keepbuf t)
            ("<down>" nil              dired-next-line :keepbuf t)
            ("p" "previous line"       dired-previous-line :keepbuf t)
            ("C-p" nil                 dired-previous-line :keepbuf t)
            ("<up>" nil                dired-previous-line :keepbuf t)
            ("C-." "up directory"      dired-up-directory)
            ("^" nil                   dired-up-directory)
            ("M-}" "next marked"       dired-next-marked-file :keepbuf t)
            ("M-{" "previous marked"   dired-prev-marked-file :keepbuf t)
            (">" "next dir"            dired-next-dirline :keepbuf t)
            ("<" "previous dir"        dired-prev-dirline :keepbuf t)
            ("j" "jump to file"        dired-goto-file :keepbuf t)
            ("M-G" "goto subdir"       dired-goto-subdir :keepbuf t)
            ;; isearch
            ("M-f" "file isearch"      dired-isearch-filenames)
            ("C-M-f" "... regexp"      dired-isearch-filenames-regexp)
            ;; find files
            ("f" "find file"           dired-find-file)
            ("e" nil                   dired-find-file)
            ("F" "find marked"         dired-do-find-marked-files)
            ("a" "find replace buffer" dired-find-alternate-file)
            ("o" "find other window"   dired-find-file-other-window)
            ("v" "view file"           dired-view-file)
            ("C-o" "disp other frm"    dired-display-file)
            ;; subdir
            ("i" "insert subdir below" dired-maybe-insert-subdir :keepbuf t)
            ("C-M-n" "next subdir"     dired-next-subdir :keepbuf t)
            ("C-M-p" "prev subdir"     dired-prev-subdir :keepbuf t)
            ("C-M-d" "child subdir"    dired-tree-down :keepbuf t)
            ("C-M-u" "parent subdir"   dired-tree-up :keepbuf t)
            ("$" "hide subdir"         dired-hide-subdir :keepbuf t)
            ("M-$" "hide all"          dired-hide-all :keepbuf t)
            ;; misc
            ("k" "visually delete"     dired-do-kill-lines :keepbuf t)
            ("M-o" "omit boring"       dired-omit-mode :keepbuf t)
            ("g" "reload buffer"       revert-buffer :keepbuf t)
            ("l" "redisplay"           dired-do-redisplay :keepbuf t)
            ("C-/" "undo"              dired-undo :keepbuf t)
            ("C-_" nil                 dired-undo :keepbuf t)
            ("C-x u" nil               dired-undo :keepbuf t)
            ("s" "toggle sort (C-u)"   dired-sort-toggle-or-edit :keepbuf t)
            ("w" "copy filename"       dired-copy-filename-as-kill :keepbuf t)
            ("y" "show file type"      dired-show-file-type :keepbuf t)
            ("=" "diff"                dired-diff)
            ("C-x C-q" "toggle wdired" dired-toggle-read-only)
            ("V" "run mail"            dired-do-run-mail)
            ;; marking
            ("m" "mark one"            dired-mark :keepbuf t)
            ("u" "unmark one"          dired-unmark :keepbuf t)
            ("DEL" "unmark backward"   dired-unmark-backward :keepbuf t)
            ("U" "unmark all"          dired-unmark-all-marks :keepbuf t)
            ("C-M-?" "unmark all"      dired-unmark-all-files :keepbuf t)
            ("M-(" "mark sexp"         dired-mark-sexp :keepbuf t)
            ("t" "toggle marked"       dired-toggle-marks :keepbuf t)
            ("A" "search marked"       dired-do-search :keepbuf t)
            ("Q" "query replace mkd"   dired-do-query-replace-regexp :keepbuf t)
            ("*" "marking popup"       popup-keys:run-dired-mark)
            ("%" "transform filenames" popup-keys:run-dired-regexp)
            ;; flagging
            ("x" "delete flagged"      dired-do-flagged-delete :keepbuf t)
            ("d" "flag for deletion"   dired-flag-file-deletion :keepbuf t)
            ("#" "flag auto save"      dired-flag-auto-save-files :keepbuf t)
            ("." "flag numerical"      dired-clean-directory :keepbuf t)
            ("~" "flag backups"        dired-flag-backup-files :keepbuf t)
            ;; file operations on marked files
            ("+" "create directory"    dired-create-directory :keepbuf t)
            ("C" "copy marked"         dired-do-copy :keepbuf t)
            ("R" "rename marked"       dired-do-rename :keepbuf t)
            ("D" "delete marked"       dired-do-delete :keepbuf t)
            ("S" "symlink marked"      dired-do-symlink :keepbuf t)
            ("H" "hardlink marked"     dired-do-hardlink :keepbuf t)
            ("G" "chgrp marked"        dired-do-chgrp :keepbuf t)
            ("M" "chmod marked"        dired-do-chmod :keepbuf t)
            ("O" "chown marked"        dired-do-chown :keepbuf t)
            ("T" "touch marked"        dired-do-touch :keepbuf t)
            ("Z" "compress marked"     dired-do-compress :keepbuf t)
            ("L" "load marked"         dired-do-load :keepbuf t)
            ("B" "byte compile"        dired-do-byte-compile :keepbuf t)
            ("P" "print marked"        dired-do-print :keepbuf t)
            ;; shell commands
            ("!" "shell command"       dired-do-shell-command)
            ("X" nil                   dired-do-shell-command)
            ("M-!" "smart shell cmd"   dired-smart-shell-command)
            ("&" "async shell cmd"     dired-do-async-shell-command)
            ;; crypt
            (":e" "encrypt"            epa-dired-do-encrypt :keepbuf t)
            (":d" "decrypt"            epa-dired-do-decrypt :keepbuf t)
            (":v" "verify"             epa-dired-do-verify :keepbuf t)
            (":s" "sign"               epa-dired-do-sign :keepbuf t)
            ))

;; ** regexp operations
;;;###autoload (autoload 'popup-keys:run-dired-regexp "popup-keys-examples" "Popup named dired-regexp" t)
(popup-keys:new
 'popup-keys:run-dired-regexp
 :more-help (popup-keys:info-node "(emacs) Transforming File Names")
 :actions '(("m" "mark regexp exact"   dired-mark-files-regexp)
            ("g" "mark regexp in"      dired-mark-files-containing-regexp)

            ("l" "downcase"            dired-downcase)
            ("u" "upcase"              dired-upcase)

            ("C" "copy by regexp"      dired-do-copy-regexp)
            ("H" "hardlink by regexp"  dired-do-hardlink-regexp)
            ("R" "rename by regexp"    dired-do-rename-regexp)
            ("r" nil                   dired-do-rename-regexp)
            ("S" "symlink by regexp"   dired-do-symlink-regexp)
            ("Y" "rel-symlink regexp"  dired-do-relsymlink-regexp)

            ("d" "flag files regexp"   dired-flag-files-regexp)
            ("&" "flag garbage"        dired-flag-garbage-files)
            ))

;; ** mark operations
;;;###autoload (autoload 'popup-keys:run-dired-mark "popup-keys-examples" "Popup named dired-mark" t)
(popup-keys:new
 'popup-keys:run-dired-mark
 :header #'(lambda (popup)
             (let (marks flags)
               (with-current-buffer popup-keys:orig-buffer
                 (setq marks (how-many dired-re-mark (point-min) (point-max)))
                 (setq flags (how-many "^D" (point-min) (point-max))))
               (insert
                (propertize "Total marks: " 'face 'font-lock-keyword-face)
                (propertize (int-to-string marks) 'face 'font-lock-constant-face)
                (propertize " -- Total flags: " 'face 'font-lock-keyword-face)
                (propertize (int-to-string flags) 'face 'font-lock-constant-face)
                "\n\n"
                )))
 :more-help (popup-keys:info-node "(emacs) Marks vs Flags")
 :actions '(("n" "next line"           dired-next-line :keepbuf t)
            ;; alternate keybindings (invisible)
            ("SPC" nil                 dired-next-line :keepbuf t)
            ("<down>" nil              dired-next-line :keepbuf t)
            ("p" "previous line"       dired-previous-line :keepbuf t)
            ("<up>" nil                dired-previous-line :keepbuf t)
            ("C-n" "next marked"       dired-next-marked-file :keepbuf t)
            ("C-p" "prev marked"       dired-prev-marked-file :keepbuf t)

            ("m" "mark one"            dired-mark :keepbuf t)
            ("%" "mark regexp"         dired-mark-files-regexp :keepbuf t)
            ("(" "mark sexp"           dired-mark-sexp :keepbuf t)
            ("s" "mark in subdir"      dired-mark-subdir-files :keepbuf t)
            ("*" "mark executables"    dired-mark-executables :keepbuf t)
            ("/" "mark directories"    dired-mark-directories :keepbuf t)
            ("@" "mark symlinks"       dired-mark-symlinks :keepbuf t)
            ("." "mark extension"      dired-mark-extension :keepbuf t)
            ("O" "mark boring files"   dired-mark-omitted :keepbuf t)

            ("u" "unmark one"          dired-unmark :keepbuf t)
            ("DEL" "unmark backward"   dired-unmark-backward :keepbuf t)
            ("!" "unmark all marks"    dired-unmark-all-marks :keepbuf t)
            ("U" nil                   dired-unmark-all-marks :keepbuf t)

            ("d" "flag for deletion"   dired-flag-file-deletion :keepbuf t)
            ("~" "flag backup files"   dired-flag-backup-files :keepbuf t )
            ("#" "flag auto-save"      dired-flag-auto-save-files :keepbuf t)
            ("&" "flag garbage"        dired-flag-garbage-files :keepbuf t)
            ("x" "delete flagged"      dired-do-flagged-delete :keepbuf t)

            ("t" "toggle marks"        dired-toggle-marks :keepbuf t)
            ("c" "change mark type"    dired-change-marks :keepbuf t)

            ("A" "search marked"       dired-do-search)
            ("Q" "query-replace markd" dired-do-query-replace-regexp)
            ))

;; ** functions

(defun popups:dired-count-marks ()
  "Count the number of marked files."
  (interactive)
  (require 'dired)
  (message (format "%d files marked"
                   (how-many dired-re-mark (point-min) (point-max)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * IBuffer

;; This is another example of a popup that essentially just displays the active
;; keymap for a buffer in a special mode, like dired.

;; Suggested keybindings:

;; (eval-after-load "ibuffer"
;;   '(progn
;;      (define-key ibuffer-mode-map (kbd "?") 'popup-keys:run-ibuffer)
;;      (define-key ibuffer-mode-map (kbd "*") 'popup-keys:run-ibuffer-mark)
;;      (define-key ibuffer-mode-map (kbd "/") 'popup-keys:run-ibuffer-filter)
;;      ))

(declare-function "ibuffer" ibuffer-map-lines-nomodify)
(declare-function "ibuffer" ibuffer-count-deletion-lines)
(declare-function "ibuffer" ibuffer-unmark-all)

;; ** general
;;;###autoload (autoload 'popup-keys:run-ibuffer "popup-keys-examples" "Popup named ibuffer" t)
(popup-keys:new
 'popup-keys:run-ibuffer
 :more-help 'describe-mode
 :actions `(;; digit arguments
            ("0" nil digit-argument :keepbuf t)
            ("1" nil digit-argument :keepbuf t)
            ("2" nil digit-argument :keepbuf t)
            ("3" nil digit-argument :keepbuf t)
            ("4" nil digit-argument :keepbuf t)
            ("5" nil digit-argument :keepbuf t)
            ("6" nil digit-argument :keepbuf t)
            ("7" nil digit-argument :keepbuf t)
            ("8" nil digit-argument :keepbuf t)
            ("9" nil digit-argument :keepbuf t)
            ;; navigation
            ("n" "next line"           ibuffer-forward-line :keepbuf t)
            ;; alternate keybindings (invisible)
            ("SPC" nil                 ibuffer-forward-line :keepbuf t)
            ("C-n" nil                 ibuffer-forward-line :keepbuf t)
            ("<down>" nil              ibuffer-forward-line :keepbuf t)
            ("p" "previous line"       ibuffer-backward-line :keepbuf t)
            ("C-p" nil                 ibuffer-backward-line :keepbuf t)
            ("<up>" nil                ibuffer-backward-line :keepbuf t)
            ("M-}" "next marked"       ibuffer-forward-next-marked :keepbuf t)
            ("M-{" "previous marked"   ibuffer-backwards-next-marked :keepbuf t)
            ("j" "jump to buffer"      ibuffer-jump-to-buffer :keepbuf t)
            ("M-g" nil                 ibuffer-jump-to-buffer :keepbuf t)
            ;; open buffers
            ("f" "visit buffer"        ibuffer-visit-buffer)
            ("e" nil                   ibuffer-visit-buffer)
            ("o" "visit other window"  ibuffer-visit-buffer-other-window)
            ("M-o" "visit one window"  ibuffer-visit-buffer-1-window)
            ("v" "view"                ibuffer-do-view)
            ("A" nil                   ibuffer-do-view)
            ("C-x v" "view horizontal" ibuffer-do-view-horizontally)
            ("H" "view marked frame"   ibuffer-do-view-other-frame)
            ("C-o" "display other win" ibuffer-visit-buffer-other-window-noselect)
            ("C-x C-f" "file in dir"   ibuffer-find-file)
            ;; groups
            ("M-n" "next group"        ibuffer-forward-filter-group :keepbuf t)
            ("M-p" "previous group"    ibuffer-backward-filter-group :keepbuf t)
            ("M-j" "jump to group"     ibuffer-jump-to-filter-group :keepbuf t)
            ("C-k" "kill group"        ibuffer-kill-line :keepbuf t)
            ("C-y" "yank group"        ibuffer-yank :keepbuf t)
            ("/" "filter popup"        popup-keys:run-ibuffer-filter)
            ;; misc
            ("g" "reload buffer"       ibuffer-update :keepbuf t)
            ("l" "redisplay"           ibuffer-redisplay :keepbuf t)
            ("`" "switch display"      ibuffer-switch-format :keepbuf t)
            ("=" "diff with file"      ibuffer-diff-with-file)
            ("-" "hide by regexp"      ibuffer-add-to-tmp-hide :keepbuf t)
            ("+" "show by regexp"      ibuffer-add-to-tmp-show :keepbuf t)
            ("b" "bury buffer"         ibuffer-bury-buffer :keepbuf t)
            ("C-t" "visit this TAGS"   ibuffer-visit-tags-table :keepbuf t)
            ("w" "copy filename"       ibuffer-copy-filename-as-kill :keepbuf t)
            ("C-c C-a" "auto update"   ibuffer-auto-mode :keepbuf t)
            ;; sorting
            ("," "sort switch"         ibuffer-toggle-sorting-mode :keepbuf t)
            ("s i" "sort reverse"      ibuffer-invert-sorting :keepbuf t)
            ("s m" "sort mode"         ibuffer-do-sort-by-major-mode :keepbuf t)
            ("s v" "sort recent"       ibuffer-do-sort-by-recency :keepbuf t)
            ("s a" "sort alphabetic"   ibuffer-do-sort-by-alphabetic :keepbuf t)
            ("s s" "sort size"         ibuffer-do-sort-by-size :keepbuf t)
            ("s f" "sort filename"     ibuffer-do-sort-by-filename/process :keepbuf t)
            ;; marking
            ("m" "mark one"            ibuffer-mark-forward :keepbuf t)
            ("u" "unmark one"          ibuffer-unmark-forward :keepbuf t)
            ("DEL" "unmark backward"   ibuffer-unmark-backward :keepbuf t)
            ("M-DEL" nil               ibuffer-unmark-all :keepbuf t)
            ("t" "toggle marked"       ibuffer-toggle-marks :keepbuf t)
            ("." "mark old"            ibuffer-mark-old-buffers :keepbuf t)
            ("*" "marking popup"       popup-keys:run-ibuffer-mark)
            ("% n" "mark name regexp"  ibuffer-mark-by-name-regexp :keepbuf t)
            ("% m" "mark mode regexp"  ibuffer-mark-by-mode-regexp :keepbuf t)
            ("% f" "mark file regxp"   ibuffer-mark-by-file-name-regexp :keepbuf t)
            ;; flagging
            ("x" "kill flagged"        ibuffer-do-kill-on-deletion-marks :keepbuf t)
            ("d" "flag for deletion"   ibuffer-mark-for-delete :keepbuf t)
            ("k" nil                   ibuffer-mark-for-delete :keepbuf t)
            ("C-d" "... backwards"     ibuffer-mark-for-delete-backwards :keepbuf t)
            ;; buffer operations on marked
            ("~" "toggle modified"     ibuffer-do-toggle-modified :keepbuf t)
            ("M" nil                   ibuffer-do-toggle-modified :keepbuf t)
            ("D" "kill marked"         ibuffer-do-delete :keepbuf t)
            ("E" "eval in marked"      ibuffer-do-eval)
            ("W" "view-eval in mrkd"   ibuffer-do-view-and-eval)
            ("O" "occur in marked"     ibuffer-do-occur)
            ("P" "print marked"        ibuffer-do-print)
            ("Q" "query-replace mrkd"  ibuffer-do-query-replace)
            ("I" "... regexp"          ibuffer-do-query-replace-regexp)
            ("M-f" "multi isearch"     ibuffer-do-isearch)
            ("C-M-f" "... regexp"      ibuffer-do-isearch-regexp)
            ("U" "replace regexp"      ibuffer-do-replace-regexp)
            ("R" "rename uniquely"     ibuffer-do-rename-uniquely :keepbuf t)
            ("S" "save marked"         ibuffer-do-save :keepbuf t)
            ("T" "toggle read-only"    ibuffer-do-toggle-read-only :keepbuf t)
            ("V" "revert marked"       ibuffer-do-revert :keepbuf t)
            ;; shell commands
            ("|" "shell cmd pipe"      ibuffer-do-shell-command-pipe)
            ("X" nil                   ibuffer-do-shell-command-pipe)
            ("!" "shell cmd filename"  ibuffer-do-shell-command-file)
            ("F" nil                   ibuffer-do-shell-command-file)
            ("N" "shell pipe replace"  ibuffer-do-shell-command-pipe-replace :keepbuf t)
            ))

;; ** mark operations
;;;###autoload (autoload 'popup-keys:run-ibuffer-mark "popup-keys-examples" "Popup named ibuffer-mark" t)
(popup-keys:new
 'popup-keys:run-ibuffer-mark
 :header #'(lambda (popup)
             (let (marks flags)
               (with-current-buffer popup-keys:orig-buffer
                 (setq marks (ibuffer-map-lines-nomodify
                              (lambda (_buf mark)
                                (or (char-equal mark ibuffer-marked-char)
                                    (char-equal mark ibuffer-deletion-char)))))
                 (setq flags (ibuffer-count-deletion-lines)))
               (insert
                (propertize "Total marks: " 'face 'font-lock-keyword-face)
                (propertize (int-to-string marks) 'face 'font-lock-constant-face)
                (propertize " -- Total flags: " 'face 'font-lock-keyword-face)
                (propertize (int-to-string flags) 'face 'font-lock-constant-face)
                "\n\n"
                )))
 :more-help 'describe-mode
 :actions '(("n" "next line"           ibuffer-forward-line :keepbuf t)
            ;; alternate keybindings (invisible)
            ("SPC" nil                 ibuffer-forward-line :keepbuf t)
            ("C-n" nil                 ibuffer-forward-line :keepbuf t)
            ("<down>" nil              ibuffer-forward-line :keepbuf t)
            ("p" "previous line"       ibuffer-backward-line :keepbuf t)
            ("C-p" nil                 ibuffer-backward-line :keepbuf t)
            ("<up>" nil                ibuffer-backward-line :keepbuf t)
            ("M-}" "next marked"       ibuffer-forward-next-marked :keepbuf t)
            ("M-{" "previous marked"   ibuffer-backwards-next-marked :keepbuf t)

            ("M" "mark by mode"        ibuffer-mark-by-mode :keepbuf t)
            ("m" "mark modified"       ibuffer-mark-modified-buffers :keepbuf t)
            ("u" "mark unsaved"        ibuffer-mark-unsaved-buffers :keepbuf t)
            ("s" "mark *special*"      ibuffer-mark-special-buffers :keepbuf t)
            ("r" "mark read-only"      ibuffer-mark-read-only-buffers :keepbuf t)
            ("/" "mark dired"          ibuffer-mark-dired-buffers :keepbuf t)
            ("e" "mark no file"        ibuffer-mark-dissociated-buffers :keepbuf t)
            ("h" "mark help buffers"   ibuffer-mark-help-buffers :keepbuf t)
            ("z" "mark compressed"     ibuffer-mark-compressed-file-buffers :keepbuf t)
            ("." "mark old"            ibuffer-mark-old-buffers :keepbuf t)
            ("% n" "mark name regexp"  ibuffer-mark-by-name-regexp :keepbuf t)
            ("% m" "mark mode regexp"  ibuffer-mark-by-mode-regexp :keepbuf t)
            ("% f" "mark file regxp"   ibuffer-mark-by-file-name-regexp :keepbuf t)

            ("DEL" "unmark backward"   ibuffer-unmark-backward :keepbuf t)
            ("*" "unmark all"          ibuffer-unmark-all :keepbuf t)
            ("!" nil                   ibuffer-unmark-all :keepbuf t)

            ("d" "flag for deletion"   ibuffer-mark-for-delete :keepbuf t)
            ("C-d" "... backwards"     ibuffer-mark-for-delete-backwards :keepbuf t)
            ("x" "kill flagged"        ibuffer-do-kill-on-deletion-marks :keepbuf t)

            ("t" "toggle marked"       ibuffer-toggle-marks :keepbuf t)
            ))

;; ** filter operations
(declare-function ibuffer-format-qualifier "ibuf-ext")

;;;###autoload (autoload 'popup-keys:run-ibuffer-filter "popup-keys-examples" "Popup named ibuffer-filter" t)
(popup-keys:new
 'popup-keys:run-ibuffer-filter
 :header #'(lambda (popup)
             (require 'ibuf-ext)
             (let ((qualifiers
                    (with-current-buffer popup-keys:orig-buffer
                      ibuffer-filtering-qualifiers)))
               (insert
                (propertize "Filter stack:\n" 'face 'font-lock-keyword-face))
               (dolist (q qualifiers)
                 (insert
                  " "
                  (propertize (ibuffer-format-qualifier q)
                              'face 'font-lock-constant-face)
                  "\n"))
               (insert "\n")
               ))
 :more-help 'describe-mode
 :actions `(("/" "disable all filters" ibuffer-filter-disable :keepbuf t)

            ("m" "filter by mode"      ibuffer-filter-by-used-mode :keepbuf t)
            ("M" "filt derived mode"   ibuffer-filter-by-derived-mode :keepbuf t)
            ("n" "filter by name"      ibuffer-filter-by-name :keepbuf t)
            ("c" "filter by content"   ibuffer-filter-by-content :keepbuf t)
            ("e" "filter by predicate" ibuffer-filter-by-predicate :keepbuf t)
            ("f" "filter by filename"  ibuffer-filter-by-filename :keepbuf t)
            (">" "filter by size >="   ibuffer-filter-by-size-gt :keepbuf t)
            ("<" "filter by size <="   ibuffer-filter-by-size-lt :keepbuf t)

            ("p" "pop filter"          ibuffer-pop-filter :keepbuf t)
            ("t" "swap top two filts"  ibuffer-exchange-filters :keepbuf t)
            ("o" "logical 'or' filts"  ibuffer-or-filter :keepbuf t)
            ("!" "logical 'not' filt"  ibuffer-negate-filter :keepbuf t)
            ("d" "undo logical op"     ibuffer-decompose-filter :keepbuf t)

            ("s" "save filters"        ibuffer-save-filters :keepbuf t)
            ("r" "switch to saved"     ibuffer-switch-to-saved-filters :keepbuf t)
            ("a" "add saved filters"   ibuffer-add-saved-filters :keepbuf t)
            ("x" "delete saved filts"  ibuffer-delete-saved-filters :keepbuf t)

            ("g" "name filter group"   ibuffer-filters-to-filter-group :keepbuf t)
            ("P" "pop filter group"    ibuffer-pop-filter-group :keepbuf t)
            ("D" "decomp filter group" ibuffer-decompose-filter-group :keepbuf t)
            ("S" "save filter groups"  ibuffer-save-filter-groups :keepbuf t)
            ("R" "switch to saved grp" ibuffer-switch-to-saved-filter-groups :keepbuf t)
            ("X" "delete saved groups" ibuffer-delete-saved-filter-groups :keepbuf t)
            ("\\" "clear filter groups" ibuffer-clear-filter-groups :keepbuf t)

            ("*" "marking popup"       popup-keys:run-ibuffer-mark)
            ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * undo-tree

;; This is another example of a popup that essentially just displays the active
;; keymap for a buffer in a special mode, like dired.

;; Suggested keybinding:

;; (eval-after-load "undo-tree"
;;   '(define-key undo-tree-visualizer-mode-map (kbd "?") 'popup-keys:run-undo-tree))

;;;###autoload (autoload 'popup-keys:run-undo-tree "popup-keys-examples" "Popup named undo-tree" t)
(popup-keys:new
 'popup-keys:run-undo-tree
 :actions '(("C-q" "abort"             undo-tree-visualizer-abort)
            ("p" "undo"                undo-tree-visualize-undo :keepbuf t)
            ("C-p" nil                 undo-tree-visualize-undo :keepbuf t)
            ("<up>" nil                undo-tree-visualize-undo :keepbuf t)
            ("n" "redo"                undo-tree-visualize-redo :keepbuf t)
            ("C-n" nil                 undo-tree-visualize-redo :keepbuf t)
            ("<down>" nil              undo-tree-visualize-redo :keepbuf t)
            ("f" "branch right"        undo-tree-visualize-switch-branch-right :keepbuf t)
            ("C-f" nil                 undo-tree-visualize-switch-branch-right :keepbuf t)
            ("<right>" nil             undo-tree-visualize-switch-branch-right :keepbuf t)
            ("b" "branch left"         undo-tree-visualize-switch-branch-left :keepbuf t)
            ("C-b" nil                 undo-tree-visualize-switch-branch-left :keepbuf t)
            ("<left>" nil              undo-tree-visualize-switch-branch-left :keepbuf t)
            ("M-{" "undo to X"         undo-tree-visualize-undo-to-x :keepbuf t)
            ("C-<up>" nil              undo-tree-visualize-undo-to-x :keepbuf t)
            ("M-}" "redo to X"         undo-tree-visualize-redo-to-x :keepbuf t)
            ("C-<down>" nil            undo-tree-visualize-redo-to-x :keepbuf t)
            ("," "scroll left"         undo-tree-visualizer-scroll-left :keepbuf t)
            ("<" nil                   undo-tree-visualizer-scroll-left :keepbuf t)
            ("." "scroll right"        undo-tree-visualizer-scroll-right :keepbuf t)
            (">" nil                   undo-tree-visualizer-scroll-right :keepbuf t)

            ("t" "toggle timestamps"   undo-tree-visualizer-toggle-timestamps :keepbuf t)
            ("d" "toggle diff"         undo-tree-visualizer-toggle-diff)
            ("s" "toggle progressive"  undo-tree-visualizer-selection-mode :keepbuf t)
            ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * Org speed

;; This popup is a reimplementation of org-mode's speed keybindings.  Using
;; popup keys has the advantage that the keys can be run from anywhere (not just
;; on a heading line), and we don't have to rebind `self-insert-command' to make
;; it happen.  The speed comes from the fact that most actions do not close the
;; popup buffer window.

;; Suggested keybinding:

;; (add-hook 'org-load-hook
;;           (lambda ()
;;             (defvar org-mode-map)
;;             (define-key org-mode-map (kbd "M-S") 'popup-keys:run-org-speed)))

;;;###autoload (autoload 'popup-keys:run-org-speed "popup-keys-examples" "Popup named org-speed" t)
(popup-keys:new
 'popup-keys:run-org-speed
 :buf-name "*org speed*"
 :actions '(;; navigation
            ("n" "next heading"   (org-speed-move-safe 'outline-next-visible-heading)
             :keepbuf t :help (describe-function 'outline-next-visible-heading))
            ("p" "prev heading"   (org-speed-move-safe 'outline-previous-visible-heading)
             :keepbuf t :help (describe-function 'outline-previous-visible-heading))
            ("f" "forward hdg"    (org-speed-move-safe 'org-forward-heading-same-level)
             :keepbuf t :help (describe-function 'org-forward-heading-same-level))
            ("b" "back heading"   (org-speed-move-safe 'org-backward-heading-same-level)
             :keepbuf t :help (describe-function 'org-backward-heading-same-level))
            ("u" "up heading"     (org-speed-move-safe 'outline-up-heading)
             :keepbuf t :help (describe-function 'outline-up-heading))
            ("F" "next block"     org-next-block :keepbuf t)
            ("B" "prev block"     org-previous-block :keepbuf t)
            ("M-}" "next element" org-forward-element :keepbuf t)
            ("M-{" "prev element" org-backward-element :keepbuf t)
            ("j" "goto"           org-goto)
            ("g" "wide goto"      (org-refile t)
              :help (describe-function 'org-refile))
            ;; visibility
            ("c" "cycle"          org-cycle :keepbuf t)
            ("C" "cycle global"   org-shifttab :keepbuf t)
            ("SPC" "echo path"    org-display-outline-path :keepbuf t)
            ("s" "narrow"         org-narrow-to-subtree :keepbuf t)
            ("=" "columns"        org-columns)
            ;; structure edit
            ("C-c" "update"       org-ctrl-c-ctrl-c :keepbuf t)
            ("C-/" nil            undo-tree-undo :keepbuf t)
            ("U" "move up"        org-shiftmetaup :keepbuf t)
            ("D" "move down"      org-shiftmetadown :keepbuf t)
            ("M-t" "transpose"    org-transpose-element :keepbuf t)
            ("r" "demote"         org-metaright :keepbuf t)
            ("l" "promote"        org-metaleft :keepbuf t)
            ("R" "demote tree"    org-shiftmetaright :keepbuf t)
            ("L" "promote tree"   org-shiftmetaleft :keepbuf t)
            ("-" "make list"      org-ctrl-c-minus :keepbuf t)
            ("*" "make header"    org-ctrl-c-minus :keepbuf t)
            ("i" "new heading"
             (progn (forward-char 1) (call-interactively
                                      'org-insert-heading-respect-content))
             :help (describe-function 'org-insert-heading-respect-content))
            ("C-w" "kill subtree" org-cut-special :keepbuf t)
            ("M-w" "copy subtree" org-copy-special :keepbuf t)
            ("C-y" "yank subtree" org-paste-special :keepbuf t)
            ("y" "clone subtree"  org-clone-subtree-with-time-shift :keepbuf t)
            ("^" "sort"           org-sort :keepbuf t)
            ("w" "refile"         org-refile)
            ("a" "archive tag"    org-toggle-archive-tag :keepbuf t)
            ("$" "archive"        org-archive-subtree-default-with-confirmation :keepbuf t)
            ("@" "mark"           org-mark-subtree)
            (";" "comment"        org-toggle-comment :keepbuf t)
            ("#" nil              org-toggle-comment :keepbuf t)
            ;; clocking
            ("I" "clock in"       org-clock-in :keepbuf t)
            ("O" "clock out"      org-clock-out :keepbuf t)
            ("W" "warning"
             (lambda(m) (interactive "sMinutes before warning: ")
               (org-entry-put (point) "APPT_WARNTIME" m))
             :help "Set APPT_WARNTIME property"
             :keepbuf t)
            ;; timestamps
            ("C-d" "deadline"     org-deadline :keepbuf t)
            ("C-s" "scheduled"    org-schedule :keepbuf t)
            ;; metadata edit
            ("t" "todo"           org-todo :keepbuf t)
            ("," "set priority"   org-priority :keepbuf t)
            ("0" "clear priority" (org-priority ?\ ) :keepbuf t
             :help (describe-function 'org-priority))
            ("1" "priority A"     (org-priority ?A) :keepbuf t
             :help (describe-function 'org-priority))
            ("2" "priority B"     (org-priority ?B) :keepbuf t
             :help (describe-function 'org-priority))
            ("3" "priority C"     (org-priority ?C) :keepbuf t
             :help (describe-function 'org-priority))
            (":" "set tags"       org-set-tags-command :keepbuf t)
            ("e" "set effort"     org-set-effort :keepbuf t)
            ("E" "inc effort"     org-inc-effort :keepbuf t)
            ;; misc
            ("v" "agenda"         org-agenda)
            ("/" "sparse tree"    org-sparse-tree)
            ("o" "open at point"  org-open-at-point)
            ("<" "restr lock"     (org-agenda-set-restriction-lock 'subtree)
             :help (describe-function 'org-agenda-set-restriction-lock))
            (">" "clear lock"     (org-agenda-remove-restriction-lock)
             :help (describe-function 'org-agenda-remove-restriction-lock))
            ))

(provide 'popup-keys-examples)
;;; popup-keys-examples.el ends here
