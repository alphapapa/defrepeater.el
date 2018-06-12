;;; defrepeater.el --- Easily make repeatable commands  -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Adam Porter

;; Author: Adam Porter <adam@alphapapa.net>
;; URL: http://github.com/alphapapa/defrepeater.el
;; Version: 0.1-pre
;; Package-Requires: ((emacs "25.2"))
;; Keywords: convenience

;;; Commentary:

;; This package lets you easily define "repeating commands," which are commands that can be repeated
;; by repeatedly pressing the last key of the sequence bound to it.

;; For example, let's say that you use "winner-mode", and you have "C-c C-w p" bound to
;; `winner-undo'.  Obviously, pressing that whole sequence repeatedly is tiresome when you want to
;; go several steps back.  But using this macro, you can press "C-c C-w p" once, and then just press
;; "p" to keep repeating `winner-undo', until you press a different key (you could press "C-g" if
;; you needed to stop the repetition so you could press "p" normally).

;; You use it like this:

;;     (defrepeater winner-undo-repeat #'winner-undo)

;; Then you can bind your keys like this (example using `general'):

;;     (general-def
;;       [remap winner-undo] #'winner-undo-repeat)

;; For example, I had "M-SPC w p" bound to `winner-undo', so now I can press "M-SPC w p p p" to call
;; `winner-undo' 3 times.

;;;; Installation

;;;;; MELPA

;; If you installed from MELPA, you're done.

;;;;; Manual

;; Put this file in your load-path, and put this in your init file:

;; (require 'defrepeater)

;;;; Usage

;; Use the `defrepeater' macro as described above.

;;;; Credits

;; This was inspired by this answer by Drew Adams: <https://emacs.stackexchange.com/a/13102>

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

;;;; Requirements

(require 'repeat)

;;;; Functions

;;;###autoload
(defmacro defrepeater (name command)
  "Define NAME as an interactive command which calls COMMAND repeatedly.
    COMMAND is called every time the last key of the sequence bound
    to NAME is pressed, until another key is pressed."
  (let* ((docstring (format "Repeatedly call `%s'. You may repeatedly press the last key of
    the sequence bound to this command to repeatedly call `%s'."
                            (cadr command)
                            (cadr command))))
    `(defun ,name () ,docstring
            (interactive)
            (let ((repeat-message-function #'ignore))
              (setq last-repeatable-command ,command)
              (repeat nil)))))

;;;; Footer

(provide 'defrepeater)

;;; defrepeater.el ends here
