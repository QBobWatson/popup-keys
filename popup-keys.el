;;; popup-keys.el --- interactive keymaps with context and arguments

;; Copyright © 2014 Joseph Rabinoff.

;; Author: Joseph Rabinoff <rabinoff@post.harvard.edu>
;;      Phil Jackson <phil@shellarchive.co.uk>
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

;; Please see the README.org file at http://github.com/QBobWatson/popup-keys
;; for usage information.

;; This library is based on magit-key-mode.el from the Magit package at
;; https://github.com/magit/magit

;;; Code:

(eval-when-compile
  (require 'cl))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * Customization options and faces

(defgroup popup-keys nil
  "Popup keys settings."
  :group 'tools)

(defcustom popup-keys:show-usage t
  "Whether to show usage information when entering a popup."
  :group 'popup-keys
  :type 'boolean)

(defcustom popup-keys:universal-key ?\C-u
  "The key to use to run `universal-argument' when mangling keyboard macros."
  :group 'popup-keys
  :type 'character)

(defcustom popup-keys:buf-name "*popup-keys: %s*"
  "Format string to create the name of the popup-keys buffer.

The %s is replaced by the group name."
  :group 'popup-keys
  :type 'string)

(defcustom popup-keys:mode-hook nil
  "Hook run when entering any popup-keys mode buffer."
  :group 'popup-keys
  :type 'hook)

(defface popup-keys:section-face
  '((t :inherit font-lock-keyword-face))
  "Face for popup-keys mode section lines."
  :group 'popup-keys)

(defface popup-keys:button-face
  '((t :inherit font-lock-builtin-face))
  "Face for popup-keys mode buttons."
  :group 'popup-keys)

(defface popup-keys:switch-face
  '((t :inherit font-lock-warning-face))
  "Face for popup-keys mode switches."
  :group 'popup-keys)

(defface popup-keys:args-face
  '((t :inherit font-lock-constant-face))
  "Face for popup-keys mode switch arguments."
  :group 'popup-keys)

(defvar popup-keys:action-args nil
  "Property list of keyword args for an action to use.

Since keyword args can't be passed using `call-interactively', they're stored
here instead.  (Actually they're let-bound.)  This is the value of
`popup-keys:current-args' at the time the action is executed.")

(defvar popup-keys:base-keymap
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map 'nodigits)
    ;; ret dwim
    (define-key map (kbd "RET") 'popup-keys:exec-at-point)
    ;; tab/backtab jumps to the next/prev "button"
    (define-key map (kbd "TAB") 'popup-keys:jump-to-next-exec)
    (define-key map (kbd "<backtab>") 'popup-keys:jump-to-prev-exec)
    ;; reset window configuration
    (define-key map (kbd "s-l") 'popup-keys:reset-windows)
    ;; isearch
    (define-key map (kbd "s-s") 'isearch-forward)
    (define-key map (kbd "s-r") 'isearch-backward)
    (define-key map (kbd "M-s-s") 'isearch-forward-regexp)
    (define-key map (kbd "M-s-r") 'isearch-backward-regexp)

    ;; all maps should `quit' with `C-g' or `q'
    (define-key map (kbd "C-g") 'popup-keys:do-action)
    (define-key map (kbd "q")   'popup-keys:do-action)

    ;; run help
    (define-key map (kbd "?") 'popup-keys:dispatch-help)
    map)
  "Base key bindings in `popup-keys:mode' buffers.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * Internal variables

(defvar popup-keys:popups (make-hash-table)
  "Hash table associating popup names to popup structures.

The value of an uninitialized popup is simply its definition, i.e. the arguments
passed to `popup-keys:add-popup'.  The popup is initialized the first time it is
accessed.")

(defvar popup-keys:prefix-arg nil
  "Prefix argument to the command that brought up the `popup-keys:mode' window.

For internal use.  Used by the command that's eventually invoked.")

(defvar popup-keys:current-args nil
  "Property list of the current argument set.

Switches are storted with value t or nil.  These are stored in
`popup-keys:action-args' before the action is executed.")

(defvar popup-keys:current-popup nil
  "The popup-keys popup active in this buffer.")

(defvar popup-keys:current-popup-name nil
  "The name symbol of `popup-keys:current-popup'.")

(defvar popup-keys:keep-buffer nil
  "When non-nil, keep the popup-keys:mode buffer after executing the action.

This is meant to be set from action hooks.")

(defvar popup-keys:orig-buffer nil
  "Stores the buffer where popup-keys:mode was initiated.")

(defvar popup-keys:pre-window-conf nil
  "Holds the pre-menu window configuration.")

(defvar popup-keys:point-orig-buffer nil
  "Holds the current value of point in the original buffer.")

(defvar popup-keys:window-conf nil
  "Holds the newly-created menu window configuration.")

(defvar popup-keys:exec-point-alist nil
  "Stores the locations of the \"buttons\" in the popup mode buffer.

It is an association list of cons pairs of the form (POINT . KEY), where POINT
is a point position in the buffer and KEY is the key at that point.  Sorted by
POINT.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * Structures

;; ** structure definitions
(defstruct (popup-keys:popup (:conc-name popup-keys:popup:)
                             (:constructor popup-keys:make-popup)
                             (:copier popup-keys:copy-popup))
  "Stores the data necessary to show a specific popup.

Most of the slots are copied from the arguments passed to `popup-keys:new'.  The
keymaps are computed at initialization time in `popup-keys:build-keymaps'."
  actions switches arguments
  ;; functions or forms
  header more-help setup pre-action post-arg
  ;; other options
  buf-name args-in-cols starting-args
  ;; computed
  keymap help-keymap)

(defstruct (popup-keys:item (:conc-name popup-keys:item:)
                            (:constructor popup-keys:make-item)
                            (:copier popup-keys:copy-item))
  "Data common to action, switch, and argument items."
  key desc help)

(defstruct
    (popup-keys:action
     (:conc-name popup-keys:action:)
     (:constructor popup-keys:make-action
                   (key desc action &key keepbuf macro-keys
                        pre-action pass-kwargs
                        (help `(describe-function ',action))))
     (:copier popup-keys:copy-action)
     (:include popup-keys:item))
  "Data specific to action items."
  action keepbuf macro-keys pre-action pass-kwargs)

(defstruct (popup-keys:switch (:conc-name popup-keys:switch:)
                              (:constructor popup-keys:make-switch
                                            (key desc argname &key help))
                              (:copier popup-keys:copy-switch)
                              (:include popup-keys:item))
  "Data specific to switch items."
  argname)

(defstruct (popup-keys:argument (:conc-name popup-keys:argument:)
                                (:constructor popup-keys:make-argument
                                              (key desc argname
                                                   &key read help))
                                (:copier popup-keys:copy-argument)
                                (:include popup-keys:item))
  "Data specific to argument items."
  argname read)

;; ** popup structure accessors
(defun popup-keys:delete-popup (popup-name)
  "Delete POPUP-NAME from `popup-keys:popups'.

Also do cleanup."
  (let ((val (gethash popup-name popup-keys:popups)))
    (when val
      (remhash popup-name popup-keys:popups)
      ;; unbind the defun
      (popup-keys:de-generate popup-name))
    val))

(defun popup-keys:add-popup (popup-name params)
  "Adds POPUP-NAME to `popup-keys:popups'.

Does not initialize the popup, which means the popup value is simply the list
PARAMS.  Does generate the associated command function."
  (popup-keys:delete-popup popup-name)
  (puthash popup-name params popup-keys:popups)
  ;; bind the defun
  (popup-keys:generate popup-name))

(defun popup-keys:get-popup (popup-name)
  "Get POPUP-NAME from `popup-keys:popups', initializing if necessary."
  (let ((popup (gethash popup-name popup-keys:popups)))
    (cond
     ((popup-keys:popup-p popup)        ; initialized popup
      popup)
     ((and popup (listp popup)) ; uninitialized popup
      (puthash popup-name (popup-keys:init-popup popup) popup-keys:popups))
     (t
      (error "Unknown or invalid popup %S" popup-name)))))

(defun popup-keys:init-popup (params)
  "Instantiate a popup structure from PARAMS."
  ;; the definition list for actions, switches, and arguments is the argument
  ;; list for the corresponding constructor
  (let ((actions    (mapcar (apply-partially 'apply 'popup-keys:make-action)
                            (plist-get params :actions)))
        (switches   (mapcar (apply-partially 'apply 'popup-keys:make-switch)
                            (plist-get params :switches)))
        (arguments  (mapcar (apply-partially 'apply 'popup-keys:make-argument)
                            (plist-get params :arguments)))
        popup)
    (setq params (plist-put params :actions   actions))
    (setq params (plist-put params :switches  switches))
    (setq params (plist-put params :arguments arguments))
    (setq popup (apply 'popup-keys:make-popup params))
    (popup-keys:build-keymaps popup)
    popup))

(defun popup-keys:de-generate (popup-name)
  "Unbind the function for POPUP-NAME."
  (fmakunbound (popup-keys:popup-name-from-group-name popup-name)))

(defun popup-keys:generate (popup-name)
  "Generate `popup-keys:run-POPUP-NAME'."
  (eval
   `(defun ,(popup-keys:popup-name-from-group-name popup-name) nil
      ,(concat "Popup key menu for " (symbol-name popup-name) ".\n"
               "Generated by `popup-keys:generate'.")
      (interactive)
      (popup-keys:mode (quote ,popup-name)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * Keymaps

(defun popup-keys:build-keymaps (popup)
  "Construct the main and help keymaps for POPUP."
  (let* ((actions   (popup-keys:popup:actions   popup))
         (switches  (popup-keys:popup:switches  popup))
         (arguments (popup-keys:popup:arguments popup))
         (map       (make-sparse-keymap))
         (help-map  (make-sparse-keymap)))
    (suppress-keymap map 'nodigits)
    (set-keymap-parent map popup-keys:base-keymap)

    ;; run help
    (define-key help-map (kbd "?") 'popup-keys:more-help)
    (define-key help-map (kbd "RET") 'popup-keys:help-at-point)

    (dolist (action actions)
      (popup-keys:def-key map help-map action))
    (dolist (switch switches)
      (popup-keys:def-key map help-map switch))
    (dolist (argument arguments)
      (popup-keys:def-key map help-map argument))

    (setf (popup-keys:popup:keymap popup) map)
    (setf (popup-keys:popup:help-keymap popup) help-map)))

(defun popup-keys:def-key (map help-map item &optional overwrite-ok)
  "Add the key definition for ITEM to MAP and HELP-MAP.

ITEM is an action, switch, or argument structure.  If OVERWRITE-OK is non-nil,
don't complain if the binding exists."
  (let* ((key (kbd (popup-keys:item:key item)))
         (curbind (lookup-key map key))
         (typestr (cond ((popup-keys:action-p   item) "action")
                        ((popup-keys:switch-p   item) "switch")
                        ((popup-keys:argument-p item) "argument"))))
    (when (and curbind (not (or overwrite-ok
                                (vectorp curbind)
                                (stringp curbind)
                                (numberp curbind))))
      (message "Warning: overriding binding for `%s'"
               (popup-keys:item:key item))
      (ding)
      (sit-for 2))
    (define-key map key
      `(lambda () (interactive)
         (,(intern (concat "popup-keys:do-" typestr)) ',item)))
    (define-key help-map key
      `(lambda () (interactive) (popup-keys:help-item ',item)))))

(defun popup-keys:get-current-keymap ()
  "Get the keymap for `popup-keys:current-popup'."
  (popup-keys:popup:keymap popup-keys:current-popup))

(defun popup-keys:get-current-help-keymap ()
  "Get the help keymap for `popup-keys:current-popup'."
  (popup-keys:popup:help-keymap popup-keys:current-popup))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; * Commands specific to `popup-keys:mode'

;; ** help commands
(defun popup-keys:dispatch-help ()
  "Read a key and run its definition in the help keymap."
  (interactive)
  (let* ((popup     popup-keys:current-popup)
         (map       (popup-keys:popup:keymap      popup))
         (help-map  (popup-keys:popup:help-keymap popup))
         (more-help (popup-keys:popup:more-help   popup))
         (buf       (current-buffer)))
    (use-local-map help-map)            ; for `read-key-sequence'
    (unwind-protect
        (let* ((key (read-key-sequence
                     (format "Enter key%s: "
                             (if more-help
                                 ", `?' for more help"
                               ""))))
               (cmd (lookup-key help-map key)))
          (if (commandp cmd)
              (call-interactively cmd)
            (error "No help associated with `%s'" (format-kbd-macro key))))
      (set-buffer buf)
      (use-local-map map))))

(defun popup-keys:simple-help (helpstr)
  "Display HELPSTR in a help buffer."
  ;; function to redisplay if the user presses back, then forward
  (help-setup-xref (list 'popup-keys:simple-help helpstr) t)
  (with-help-window (help-buffer) (princ helpstr)))

(defun popup-keys:help-item (item)
  "Run the help slot from ITEM."
  ;; Switch to orig buffer so `describe-function' can show key bindings
  (with-current-buffer popup-keys:orig-buffer
    (let ((help (popup-keys:item:help item)))
      (if (stringp help)
          (popup-keys:simple-help help)
        (if help
            (popup-keys:run-generic-func help (list 'item item))
          (error "No help associated with `%s'"
                 (popup-keys:item:key item)))))))

(defun popup-keys:help-at-point ()
  "Find the thing at point, and run its help."
  (interactive)
  (let ((key (or (get-text-property (point) 'popup-keys:key-here)
                 (error "No help at point."))))
    (call-interactively
     (lookup-key (popup-keys:get-current-help-keymap) (kbd key)))))

(defun popup-keys:more-help ()
  "Run the current popup's more-help if available."
  (interactive)
  (let ((more-help (popup-keys:popup:more-help popup-keys:current-popup)))
    (with-current-buffer popup-keys:orig-buffer
      (if (stringp more-help)
          (popup-keys:simple-help more-help)
        (if more-help
            (popup-keys:run-generic-func more-help)
          (error "More help not available"))))))

;; ** exec at point (RET)
(defun popup-keys:exec-at-point ()
  "Do action/arg/switch at point."
  (interactive)
  (let ((key (or (get-text-property (point) 'popup-keys:key-here)
                 (error "Nothing at point to do."))))
    (call-interactively (lookup-key (current-local-map) (kbd key)))))

;; ** jump next and prev (TAB)
(defun popup-keys:jump-to-next-exec ()
  "Jump to the next action/args/switch from point."
  (interactive)
  (goto-char (car
              (popup-keys:next-after
               (point)
               popup-keys:exec-point-alist))))

(defun popup-keys:jump-to-prev-exec ()
  "Jump to the prev action/args/switch from point."
  (interactive)
  (goto-char (car
              (popup-keys:prev-before
               (point)
               popup-keys:exec-point-alist))))

;; ** reset windows
(defun popup-keys:reset-windows ()
  "Reset window configuration."
  (interactive)
  (set-window-configuration popup-keys:window-conf))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * Toggling and running

;; ** action
;; What's the correct way to do this?
(defmacro popup-keys:unwind-protect-err (bodyform &rest unwindforms)
  "If BODYFORM terminates with a nonlocal exit, execute UNWINDFORMS.

Do not execute UNWINDFORMS otherwise."
  `(let ((err t))
     (unwind-protect
         (progn
           ,bodyform
           (setq err nil))
       (when err
         ,@unwindforms))))

(defun popup-keys:get-args (cmd)
  "Run the interactive spec of CMD and return the argument list."
  (call-interactively
   (eval `(lambda (&rest args) ,(interactive-form cmd) args))))

(defun popup-keys:do-action (&optional item)
  "Execute the action associated to the item ITEM.

Quit `popup-keys:mode' unless one of the action hooks sets
`popup-keys:keep-buffer' or the action option keepbuf is set to t.  Reset
`popup-keys:keep-buffer' to nil afterward.

Just quit if ITEM is nil.

This sets `popup-keys:action-args' for use by the action."
  (interactive)
  (setq item (or item (popup-keys:make-action nil nil nil)))
  (let* ((popup popup-keys:current-popup)
         (key (popup-keys:item:key item))
         (action (popup-keys:action:action item))
         (keep (popup-keys:action:keepbuf item))
         (pass-kwargs (popup-keys:action:pass-kwargs item))
         (macro-keys (popup-keys:action:macro-keys item))
         (popup-hook (popup-keys:popup:pre-action popup))
         (extra-hook (popup-keys:action:pre-action item))
         (current-prefix-arg (or current-prefix-arg popup-keys:prefix-arg))
         (popup-keys:action-args popup-keys:current-args)
         (current-buf (current-buffer))
         (orig-buf popup-keys:orig-buffer)
         (pre-window-conf popup-keys:pre-window-conf)
         (window-conf popup-keys:window-conf)
         (orig-point popup-keys:point-orig-buffer)

         (to-bind
          (loop for hook in (list popup-hook extra-hook)
                append (popup-keys:run-generic-func
                        hook (list 'item item))))

         (keepbuf (and action (or (eq keep t) popup-keys:keep-buffer)))
         interactive-args)
    ;; Reset for next command
    (setq popup-keys:keep-buffer nil)
    ;; Get the interactive args before killing the buffer?
    (when (and (commandp action) (eq keep 'args))
      (set-window-configuration window-conf)
      (select-window (get-buffer-window orig-buf))
      (goto-char orig-point)
      ;; What's the right way to catch any error?
      (popup-keys:unwind-protect-err
         (setq interactive-args (popup-keys:get-args action))
       (set-window-configuration pre-window-conf)
       (kill-buffer current-buf)))
    ;; Kill the buffer or switch to the original window
    (if keepbuf
        (progn
          (set-window-configuration window-conf)
          ;; need to use `select-window' for running macros
          (select-window (get-buffer-window orig-buf)))
      (set-window-configuration pre-window-conf)
      (kill-buffer current-buf))
    (set-buffer orig-buf)
    (goto-char orig-point)
    ;; Run the command
    (when macro-keys (popup-keys:save-macro-keys macro-keys))
    (when action
      (unwind-protect
          (eval
           `(let ,to-bind
              (cond
               ((eq keep 'args)
                (setq this-command action)
                (apply action interactive-args))
               ((and pass-kwargs (functionp action))
                (apply action popup-keys:action-args))
               ((commandp action)
                (setq this-command action)
                (call-interactively action))
               ((functionp action)
                (funcall action))
               ((listp action)
                (eval action)))))
        ;; If we're keeping the buffer, save the value of point in the original
        ;; buffer.
        (when keepbuf
          (setq orig-point (point))
          (set-window-configuration window-conf)
          ;; the previous function moves the point
          (with-current-buffer orig-buf
            (goto-char orig-point))
          ;; move the cursor in the other window
          (let ((win (get-buffer-window orig-buf)))
            (when win (set-window-point win orig-point)))
          (set-buffer current-buf)
          (setq popup-keys:point-orig-buffer orig-point)
          (popup-keys:draw)
          ;; save window config in case window size has changed
          (setq popup-keys:window-conf (current-window-configuration)))))))

;; ** argument
(defun popup-keys:do-argument (item)
  "Input value for argument ITEM."
  (let* ((argname (popup-keys:argument:argname item))
         (read (popup-keys:argument:read item))
         (curval (plist-get popup-keys:current-args argname))
         (input (cond
                 ((null read)
                  (read-string (concat (symbol-name argname) ": ")))
                 ((stringp read)
                  (read-string read))
                 (t
                  (popup-keys:run-generic-func
                   read
                   (list 'arg `(quote ,argname))
                   (list 'curval curval))))))
    (setq popup-keys:current-args
          (plist-put popup-keys:current-args argname input))
    (let ((hook (popup-keys:popup:post-arg popup-keys:current-popup)))
      (popup-keys:run-generic-func hook
                                   (list 'arg `(quote ,argname))
                                   (list 'newval input)))
    (popup-keys:draw)))

;; ** switch
(defun popup-keys:do-switch (item)
  "Toggle the switch ITEM."
  (let* ((switch-name (popup-keys:switch:argname item))
         (newval (not (plist-get popup-keys:current-args switch-name))))
    (setq popup-keys:current-args
          (plist-put popup-keys:current-args switch-name newval))
    (let ((hook (popup-keys:popup:post-arg popup-keys:current-popup)))
      (popup-keys:run-generic-func hook
                                   (list 'arg `(quote ,switch-name))
                                   (list 'newval newval))))
  (popup-keys:draw))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * Mode

(defun popup-keys:mode (popup-name)
  "Mode for popup-keys selection in popup named POPUP-NAME."
  (interactive)
  ;; setup the mode, draw the buffer
  (let* ((popup (popup-keys:get-popup popup-name))
         (buf (get-buffer-create (or (popup-keys:popup:buf-name popup)
                                     (format popup-keys:buf-name
                                             (symbol-name popup-name)))))
         (oldbuf (current-buffer))
         (starting-args (popup-keys:popup:starting-args popup))
         (orig-point (point)))
    (set-buffer buf)
    (kill-all-local-variables)
    (set (make-local-variable 'popup-keys:current-popup-name) popup-name)
    (set (make-local-variable 'popup-keys:current-popup) popup)
    (set (make-local-variable 'popup-keys:orig-buffer) oldbuf)
    (set (make-local-variable 'scroll-margin) 0)
    (set (make-local-variable 'popup-keys:current-args) (copy-sequence starting-args))
    (set (make-local-variable 'popup-keys:prefix-arg) current-prefix-arg)
    (set (make-local-variable 'popup-keys:keep-buffer) nil)
    (set (make-local-variable 'popup-keys:pre-window-conf)
         (current-window-configuration))
    (set (make-local-variable 'popup-keys:point-orig-buffer) orig-point)
    (set (make-local-variable 'popup-keys:exec-point-alist) nil)
    (make-local-variable 'font-lock-defaults)
    (use-local-map (popup-keys:popup:keymap popup))
    (setq mode-name "popup-keys:mode"
          major-mode 'popup-keys:mode)
    (setq buffer-read-only t)

    ;; run init hooks
    (run-hooks 'popup-keys:mode-hook)
    (let ((hook (popup-keys:popup:setup popup)))
      (popup-keys:run-generic-func hook (list 'popup popup)))

    ;; First draw the popup-keys buffer, then count how many lines it has, then
    ;; create a window of that size for it.  This way, if creating the
    ;; popup-keys window below the top window in a split frame, the lower window
    ;; does not get resized.  As far as I can tell, if the popup-keys window is
    ;; resized in any other way, then the bottom window gets resized.
    (popup-keys:draw)
    ;; Negative argument means the "below" window gets that many lines.
    ;; Add 2 for mode line and final newline (?) -- this agrees with what
    ;; `fit-window-to-buffer' does, but I'm not sure why.
    (split-window-below (- (+ 2 (count-screen-lines))))
    (other-window 1)
    (switch-to-buffer buf)
    (set (make-local-variable 'popup-keys:window-conf)
         (current-window-configuration)))

  (when popup-keys:show-usage
    (message (concat "Type a prefix key to toggle it. "
                     "Run actions with their keys. "
                     "'?' for more help."))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; * Draw

;; ** draw elements
(defun popup-keys:draw-section (section)
  "Draw section name SECTION with the correct face."
  (insert (propertize section 'face 'popup-keys:section-face) "\n"))

(defun popup-keys:draw-arguments (args args-in-cols)
  "Draw the args part of the menu.

ARGS is a list of `popup-keys:argument' structures.  Draw in columns if
ARGS-IN-COLS is true."
  (popup-keys:draw-buttons
   "Arguments"
   args
   (lambda (arg)
     (let* ((argname (popup-keys:argument:argname arg))
            (val (plist-get popup-keys:current-args argname)))
       (format "(%S) %s"
               argname
               (propertize (prin1-to-string val)
                           'face 'popup-keys:args-face))))
   (not args-in-cols)))

(defun popup-keys:draw-switches (switches)
  "Draw the switches part of the menu.

SWITCHES is a list of `popup-keys:switch' structures."
  (popup-keys:draw-buttons
   "Switches"
   switches
   (lambda (switch)
     (let* ((argname (popup-keys:switch:argname switch))
            (str (symbol-name argname)))
       (format "(%s)"
               (if (plist-get popup-keys:current-args argname)
                   (propertize str 'face 'popup-keys:switch-face)
                 str))))))

(defun popup-keys:draw-actions (actions)
  "Draw the actions part of the menu.

ACTIONS is a list of `popup-keys:action' structures."
  (popup-keys:draw-buttons "Actions" actions nil))

(defun popup-keys:draw-buttons (section items maker
                                        &optional one-col-each)
  "Draw buttons under section name SECTION.

ITEMS is a list of structures inheriting from `popup-keys:item'.  MAKER, if
non-nil, is a function to make text to place after the button.  MAKER is a
function of one argument, the `popup-keys:item' structure.  If ONE-COL-EACH is
true, put each entry in its own column."
  (when items
    (popup-keys:draw-section section)
    (popup-keys:draw-in-cols
     (mapcar (lambda (item)
               (let* ((key  (popup-keys:item:key item))
                      (desc (popup-keys:item:desc item))
                      (head (propertize key 'face 'popup-keys:button-face))
                      (more (and maker (funcall maker item)))
                      (text (format "%s: %s%s%s"
                                    head desc (if more " " "") (or more ""))))
                 (propertize text 'popup-keys:key-here key)))
             items)
     one-col-each)))

(defun popup-keys:draw-in-cols (strings one-col-each)
  "Given a list of STRINGS, print in columns (using `insert').

Also add the information to `popup-keys:exec-point-alist'.  If ONE-COL-EACH is
true then don't columify, but rather, draw each item on one line."
  (let* ((longest-act (apply 'max (mapcar 'length strings))))
    (insert " ")
    (while strings
      (let* ((str (car strings))
             (padding (make-string (- (+ longest-act 3) (length str)) ? )))
        (push (cons (point-marker)
                    (get-text-property 0 'popup-keys:key-here str))
              popup-keys:exec-point-alist)
        (insert str)
        (if (or one-col-each
                (and (> (+ (length padding) (current-column) longest-act)
                        (window-width))
                     (cdr strings)))
            (insert "\n ")
          (insert padding)))
      (setq strings (cdr strings))))
  (insert "\n"))

;; ** draw overall
(defun popup-keys:draw-1 ()
  "Draw actions, switches and arguments.

Return the point before the actions part, if any, nil otherwise."
  (require 'cl)
  (declare-function cl-remove-if-not "cl")
  (let* ((popup        popup-keys:current-popup)
         (args-in-cols (popup-keys:popup:args-in-cols popup))
         (header       (popup-keys:popup:header popup))
         (switches     (popup-keys:popup:switches popup))
         (arguments    (popup-keys:popup:arguments popup))
         (actions      (popup-keys:popup:actions popup))
         point-before-actions)
    (setq popup-keys:exec-point-alist nil)
    (popup-keys:run-generic-func header (list 'popup popup))
    (popup-keys:draw-switches switches)
    (popup-keys:draw-arguments arguments args-in-cols)
    ;; handle invisible actions
    (setq actions (cl-remove-if-not 'popup-keys:action:desc actions))
    (when actions (setq point-before-actions (point-marker)))
    (popup-keys:draw-actions actions)
    (insert "\n")
    (delete-trailing-whitespace)
    (setq popup-keys:exec-point-alist
          (reverse popup-keys:exec-point-alist))
    point-before-actions))

(defun popup-keys:draw ()
  "(Re)draw the popup buffer.  (Re)position the point."
  (let ((buffer-read-only nil)
        (current-exec (get-text-property (point) 'popup-keys:key-here))
        (new-exec-pos)
        (old-point (point))
        (is-first (zerop (buffer-size)))
        (point-before-actions nil))
    ;; draw the buffer
    (erase-buffer)
    (setq point-before-actions (popup-keys:draw-1))
    ;; reposition the point
    (when current-exec
      (setq new-exec-pos
            (car (rassoc current-exec popup-keys:exec-point-alist))))
    (cond ((and is-first point-before-actions)
           (goto-char point-before-actions)
           (popup-keys:jump-to-next-exec))
          (new-exec-pos
           (goto-char new-exec-pos)
           (skip-chars-forward " "))
          (t
           (goto-char old-point))))
  ;; The popup-keys buffer will not be displayed yet if we're called from
  ;; `popup-keys:mode'.
  (let ((win (get-buffer-window (current-buffer))))
    (when win
      ; Poor-man's (fit-window-to-buffer win) -- but this works more
      ; consistently with the way the window is originally sized.
      (window-resize win (- (+ 2 (count-screen-lines))
                            (window-total-size win))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * Misc functions

(defun popup-keys:run-generic-func (thing &rest args)
  "Run THING with ARGS.

THING may be a function or a form, or nil.  Each optional ARGS is a two-element
list (NAME VAL).

If THING is a function, apply it to the cadr's of ARGS.
If THING is a form, let-bind ARGS and then eval THING.
If THING is nil, do nothing.  (This is a special case of the above.)

Return the result of running or evaluating THING."
  (if (functionp thing)
      (apply thing (mapcar 'cadr args))
    (eval (list 'let args thing))))

;; ** list functions
(defun popup-keys:next-after (num ring)
  "Return the next element of RING after NUM.

Wrap around if necessary.  RING is an alist of cons cells like
`popup-keys:exec-point-alist'."
  (or (loop for elt in ring
               if (> (car elt) num)
                 return elt)
      (car ring)))

(defun popup-keys:prev-before (num ring)
  "Return the previous element of RING before NUM.

Wrap around if necessary.  RING is an alist of cons cells like
`popup-keys:exec-point-alist'."
  (if (<= num (caar ring))
      (car (last ring))
    (let ((prev))
      (loop for elt in ring
               if (>= (car elt) num)
                 return prev
               do (setq prev elt)))))

;; ** macro support
(defun popup-keys:save-macro-keys (macro-keys)
  "When defining a keyboard macro, replace current command with MACRO-KEYS."
  (let* ((keylist (listify-key-sequence (kbd macro-keys))))
    (when defining-kbd-macro
      (cancel-kbd-macro-events)
      (dolist (key (nconc (popup-keys:prefix-arg-to-key-seq
                           current-prefix-arg)
                          keylist))
        (store-kbd-macro-event key)))))

(defun popup-keys:prefix-arg-to-key-seq (arg)
  "Take raw prefix arg ARG and make a key sequence to invoke it.

Uses `popup-keys:universal-key' to run `universal-argument'."
  (cond
   ((null arg)
    nil)
   ((integerp arg)
    (cons popup-keys:universal-key
          (listify-key-sequence (kbd (int-to-string arg)))))
   ((eq arg '-)
    (list popup-keys:universal-key ?-))
   ((listp arg)
    (let ((num (car arg)) seq)
      (while (> num 1)
        (setq num (/ num 4))
        (setq seq (cons popup-keys:universal-key seq)))
      seq))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; * Functions to export

;; ** popup-keys:new
;;;###autoload
(defun popup-keys:new (popup-name &rest kwargs)
"Define a new popup command `popup-keys:run-POPUP-NAME'.

The popup is initialized lazily, so this function takes negligible time to
execute.  If there already is a popup of that name then this will remove it
first.

This function accepts the following keyword arguments KWARGS:

  :actions: the value is a list of actions, each of the form

        (KEY DESC ACTION [OPTIONS])

      where:

        KEY is the key sequence to type in kmacro format, e.g. \"C-x g\".  It is
          passed to the function `kbd' to generate the key sequence.

        DESC is the display name of the action, e.g. \"Grep\".
          If this is nil, the key is bound but the entry is invisible.

        ACTION is the action to execute.  It can be a command, a function, or a
          form.  If a command, it is executed interactively; if a function, it
          is run with no arguments, and if a form, it is evaluated.  In all
          cases the dynamic variable `popup-keys:action-args' is bound to a
          property list containing the current state of the arguments and
          switches defined in :arguments and :switches below.  Other dynamic
          bindings can be set using the two kinds of :pre-action hooks.

      The remaining part of the list, OPTIONS, is a property list accepting the
      following keys:

        :keepbuf: if the value is t, causes the popup buffer not to be killed
            after the command is run.  This is useful, for example, if the
            action just moves the point in the original buffer.  If the value is
            'args, read the interactive arguments of the command before killing
            the buffer.  This is useful so the user can see the contents of the
            popup buffer while being queried, e.g. if there is interesting
            context in the header.

        :pass-kwargs: if this boolean flag is set, ACTION must be a function;
            it will be called with the contents of `popup-keys:action-args'
            applied as arguments.  This works well with `cl-defun' and &key
            arguments, although you might want to include &allow-other-keys to
            prevent errors.  This flag has no effect if :keepbuf is 'args.

        :macro-keys: the value is a key sequence in kmacro format.  If set,
            replace the key(s) used to run the action with this key sequence
            when defining a keyboard macro.  To be useful you probably want to
            run `cancel-kbd-macro-events' in the :setup hook (see below) to
            prevent the key sequence which initiated the popup from being part
            of the macro.

        :pre-action: the value is an additional hook to run after the popup's
            :pre-action hook is run, but only when running to this action.  See
            :pre-action below.

        :help: how to display help for this action.  The value is a string, a
            function, or a form.  If a string, that string will be displayed in
            a help buffer.  If a function, that function is called with a single
            argument, the `popup-keys:action' structure initialized from this
            action.  If a form, that form is evaluated, with the symbol 'item
            bound to the `popup-keys:action' structure.  If not specified, run
            `describe-command' on ACTION.

  :switches: the value is a list of switches, each of the form
        (KEY DESC ARGNAME [OPTIONS])
      where:
        KEY and DESC are as in :actions.
        ARGNAME is the true-false keyword argument symbol, e.g. :tfarg.

      The remaining part of the list, OPTIONS, is a property list accepting the
      following key:

        :help: as in :actions, except there is no default.

  :arguments: the value is a list of arguments, each of the form.
        (KEY DESC ARGNAME [OPTIONS])
      where:
        KEY and DESC are as in :actions.
        ARGNAME is the keyword argument symbol, e.g. :arg.

      The remaining part of the list, OPTIONS, is a property list accepting the
      following keys:

        :read: Specifies how to query the user for the new argument value.  The
            value is a string, a function, or a form.  If a string, that string
            is used as the prompt argument of `read-string'.  If a function,
            that function is called with two arguments, the  argument name and
            the current value of the argument.  If a form, that form is
            evaluated, with the symbol 'arg bound to the argument name and the
            symbol 'curval bound to the current value of the argument.  If not
            specified, defaults to
                   (read-string (concat (symbol-name ARGNAME) \": \"))

        :help: as in :actions, except there is no default.

  :more-help: how to display extra help for this popup.  The value is a string,
      a function, or a form.  If a string, that string will be displayed in a
      help buffer.  If a function, that function is called with no arguments.
      If a form, that form is evaluated.  See `popup-keys:info-node'.

  :setup: a function or a form to execute before showing the popup.  If a
      function, it is run with one argument, the `popup-keys:popup' structure.
      If a form, it is evaluated with the symbol 'popup bound to the
      `popup-keys:popup' structure.

  :pre-action: a function or a form to execute before running an action.  If a
      function, it is run with one argument, the `popup-keys:action' structure
      for the action being run.  If a form, it is evaluated with the symbol
      'item bound to the `popup-keys:action' structure.  If there is no action
      being run, i.e. the user is quitting, the structure is initialized to nil
      values.  The hook may return / evaluate to a list of two-element lists
      (VAR VALUE) which are let-bound when the action runs.  Otherwise it must
      return nil.

  :post-arg: a function or a form to execute after the user updates an argument
      or a switch.  If a function, it is executed with two arguments (ARG VAL),
      where ARG is name of the switch or argument that was changed, and VAL is
      its new value.  If a form, it is evaluated with the symbol 'arg bound to
      ARG and the symbol 'newval bound to VAL, as above.

  :header: this may insert arbitrary text at the top of the popup buffer (using
      `insert').  It may be a function or a form.  If a function, it is executed
      with a single argument, this `popup-keys:popup' structure.  If a form, it
      is evaluated with the symbol 'popup bound to this `popup-keys:popup'
      structure.  Note that it has to propertize the text itself.

  :buf-name: use this as the popup keys buffer name.  The default is to use
      `popup-keys:buf-name'.

  :args-in-cols: a boolean value which, if set, causes arguments to be drawn in
      columns, like the switches and actions.

  :starting-args: the value is a property list whose keys are the switch and
      argument names and whose values are their initial values."
  (popup-keys:add-popup popup-name kwargs))

;; ** popup-keys:add-thing
;;;###autoload
(defun popup-keys:add-thing (popup-name type &rest def)
  "Add an action, switch, or argument to POPUP-NAME.

TYPE is the symbol 'action, 'switch, or 'argument.  DEF is a list that would
appear as an element in the :actions, :switches, or :arguments keyword argument,
respectively, in `popup-keys:new'."
  (require 'cl)
  (declare-function cl-delete "cl")
  (let* ((popup (popup-keys:get-popup popup-name))
         (getter (intern (concat "popup-keys:popup:"
                                 (symbol-name type)
                                 (if (eq type 'switch) "es" "s"))))
         (things (funcall getter popup))
         (maker (intern (concat "popup-keys:make-" (symbol-name type))))
         (item (apply maker def))
         (key (popup-keys:item:key item))
         (newthings (nconc (cl-delete key things
                                      :key 'popup-keys:item:key
                                      :test 'equal)
                           (list item))))
    (eval `(setf (,getter popup) newthings))
    ;; Don't define the new key if the keymaps haven't been generated yet.
    (popup-keys:def-key
       (popup-keys:popup:keymap popup)
       (popup-keys:popup:help-keymap popup)
       item 'overwrite-ok)))

;; ** popup-keys:info-node
;;;###autoload
(defun popup-keys:info-node (node)
  "Return a form that that takes the user to info node NODE.

NODE could be, for example, the string \"(emacs) Dired\".  Run info in other
window.  Suitable for the :more-help arg."
  `(progn
     (require 'info)
     (info-setup ,node
                 (switch-to-buffer-other-window "*info*"))))

;; ** other
(defun popup-keys:popup-name-from-group-name (name)
  "Return the name of the popup function generated from popup name NAME."
  (intern (concat "popup-keys:run-" (symbol-name name))))

;;;###autoload
(defun popup-keys:write-autoload (name)
  "Insert an autoload cookie for a popup named NAME at point."
  (interactive "SPopup name: ")
  (let ((fname (buffer-file-name)))
    (unless fname
      (user-error "This command only works from a buffer with a file."))
    (setq fname (file-name-base fname))
    (insert
     (format
      ";;;###autoload (autoload '%S \"%s\" \"Popup named %S\" t)\n"
      (popup-keys:popup-name-from-group-name name) fname name))))

;; Local Variables:
;;   checkdoc-arguments-in-order-flag: nil
;;   flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:

(provide 'popup-keys)
;;; popup-keys.el ends here
