;;; advent-of-code-helper.el --- Emacs Interface for Advent of Code               -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Brandon C. Irizarry

;; Author: Brandon C. Irizarry <brandon.irizarry@gmail.com>
;; Keywords: advent-of-code, game, games, holidays

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; To install, put this file somewhere in your path and require this
;; file.
;;
;; Advent of Code is an Advent calendar of small programming puzzles
;; for a variety of skill sets and skill levels that can be solved in
;; any programming language you like. People use them as interview
;; prep, company training, university coursework, practice problems, a
;; speed contest, or to challenge each other.
;;
;; See 'https://adventofcode.com/2023/about'.
;;
;; In order to play Advent of Code, you need to set up an account with
;; it and log in. You then solve the available puzzles for a given
;; year. In order to do so, you first need to download that puzzle's
;; input, which is what you'll work with to solve the puzzle.
;;
;; Normally, you would download this input manually, specifying where
;; it should be downloaded onto disk. However, it's possible to
;; programmatically pull it down to some ideal location, using the hash
;; associated with your login cookie, which you find in your browser's
;; Developer Tools panel somewhere.
;;
;; This library provides a few utilities for facilitating these
;; concerns: one for setting the cookie's hash, a process we call
;; "bootstrapping"; one for then downloading the puzzle input into a
;; particular directory, which is created automatically if missing;
;; and one for visiting a particular puzzle directory, obviating the
;; need to remember where you have your Advent of Code work stored.
;;
;; We also define a minor mode, which uses a keymap that in turn lets
;; us define a simple menu enabling these tasks. It's of course
;; possible to add bindings to the keymap directly; we avoid doing
;; this out of the box to stay as much out of the user's way as
;; possible.

;;; Customization and defvars.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup advent-of-code-helper nil
  "A library for organizing Advent of Code solutions.

In particular, this library manages a user's cookie hash for
automating both downloading puzzle input, and uploading
solutions."
  :version 1.0
  :group 'local)

(defcustom aoch-top-level-directory "~/adventofcode/"
  "The top level directory containing your Advent of Code solutions."
  :type 'directory
  :tag "Advent of Code Helper Top Level Directory"
  :group 'advent-of-code-helper)

(defcustom aoch-cookie-name "cookie.el"
  "The name of the file containing your Advent of Code cookie hash.

This file is stored directly under your top-level directory."
  :type 'string
  :set-after '(aoch-top-level-directory)
  :tag "Advent of Code Helper Cookie Hash Filename"
  :group 'advent-of-code-helper)

(define-inline aoch-get-cookie-fullpath ()
  "Get the full path to the cookie, based on the current values of
`aoch-top-level-directory' and `aoch-cookie-name'."
  (concat aoch-top-level-directory
          aoch-cookie-name))

;;; EIEIO definitions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'eieio-custom)
(require 'eieio-base)

(defclass aoch-cookie (eieio-persistent eieio-singleton eieio-named)
  ((hash :initarg :hash
         :initform ""
         :reader get-hash
         :custom string
         :label "Cookie hash"
         :documentation
         "Used to validate Advent of Code HTTP requests.")))

(cl-defmethod initialize-instance :after ((cookie aoch-cookie) &rest _)
  "Initialize fields inherited from EIEIO-PERSISTENT to their proper
values."
  (let ((fullpath (aoch-get-cookie-fullpath)))
    (unless (file-exists-p fullpath)
      (make-empty-file fullpath))
    (oset cookie file fullpath)
    (oset cookie file-header-line ";; -*- mode: lisp-data -*-")
    (oset cookie do-backups nil)
    (oset cookie object-name "Advent of Code Session Cookie")))

;; This function is a pared-down version of `eieio-persistent-save'.
(defun aoch-save-cookie (cookie)
  "Save a cookie to disk, using the :file field of COOKIE.

The comment header is a file-local-variable property line
specifying the major mode as `lisp-data-mode'."
  (with-temp-buffer
    (let ((standard-output (current-buffer))
          (eieio-print-object-name nil))
      (object-write cookie)
      (let ((backup-inhibited (not (oref cookie do-backups)))
            (coding-system-for-write 'utf-8-emacs))
        (write-region (point-min) (point-max) (oref cookie file))))))

(cl-defmethod eieio-done-customizing ((cookie aoch-cookie))
  "Override this hook to ensure that the cookie is saved to disk
after customization."
  (aoch-save-cookie cookie))

(defun aoch-load-cookie ()
  "Load a cookie from `aoch-cookie-fullpath'."
  (eieio-persistent-read (aoch-get-cookie-fullpath) aoch-cookie))

(defun aoch-bootstrap ()
  "Bootstrap cookie by defining it via an EIEIO customization
buffer.

`eieio-done-customizing' is overriden to ensure that the new cookie
is saved to disk after customization."
  (interactive)
  (eieio-customize-object (aoch-cookie)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun aoch--get-year-and-day-from-user ()
  "Helper function to use in `interactive' forms that require YEAR
and DAY as user-supplied parameters."
  (let ((current-year (string-to-number (format-time-string "%Y"))))
    (mapcar #'string-to-number
            (list
             (completing-read "Year: " (mapcar #'number-to-string (number-sequence 2015 current-year)))
             (completing-read "Day: " (mapcar #'number-to-string (number-sequence 1 25)))))))

(define-inline aoch--get-puzzle-directory (year day)
  "Return the string denoting the puzzle directory for a given YEAR
and DAY."
  (inline-quote (format "%s%d/day/%d" aoch-top-level-directory ,year ,day)))

(defun aoch-prepare-puzzle (year day)
  "Download puzzle input for YEAR and DAY, possibly creating the
relevant directory."
  (interactive (aoch--get-year-and-day-from-user))
  ;; In case we're not running interactively.
  (when (< year 2015)
    (user-error "Advent of Code didn't exist before this time"))
  (let ((cookie (condition-case nil
                    (aoch-load-cookie)
                  (file-missing
                   (user-error "Define a cookie first with 'aoch-bootstrap'.")))))
    ;; It can't hurt to always re-store the cookie, even though one
    ;; might already be in place.
    (url-cookie-store "session" (get-hash cookie) nil ".adventofcode.com" "/")
    (url-retrieve (format "https://adventofcode.com/%d/day/%d/input" year day)
                  (lambda (status)
                    (pcase (cl-third (plist-get status :error))
                      ((pred null)
                       ;; Delete the HTTP response header
                       (re-search-forward "^$" nil t)
                       (delete-region (point-min) (point))
                       ;; Prepare the puzzle directory before writing
                       ;; the input file
                       (let ((puzzle-directory (aoch--get-puzzle-directory year day)))
                         (make-directory puzzle-directory 'create-missing-parent-dirs)
                         (write-file (concat puzzle-directory "/input.txt"))))
                      (404
                       (error "Puzzle hasn't been published yet"))
                      (500
                       (error "Bad cookie hash: run 'aoch-bootstrap'")))))))

(defun aoch-visit-puzzle (year day)
  "Visit directory associated with a given YEAR and DAY (e.g.,
Day 12 of Year 2016)."
  (interactive (aoch--get-year-and-day-from-user))
  (let ((directory (aoch--get-puzzle-directory year day)))
    (unless (file-exists-p directory)
      (user-error "Prepare puzzle first with `aoch-prepare-puzzle'"))
    (dired directory)))

(defvar aoch-map (make-sparse-keymap))

(define-minor-mode aoch-mode
    "Activate certain niceties for working with Advent of Code.

The user is free to provide their own bindings for
`aoch-bootstrap', `aoch-prepare-puzzle', and
`aoch-visit-puzzle'. A GUI menu is also provided."
  :lighter " AOCH"
  :global t
  :keymap aoch-map)

(easy-menu-define nil aoch-map
  "Menu for Advent of Code Helper."
  '("Advent-of-Code"
    ["Bootstrap Cookie" aoch-bootstrap]
    ["Prepare Puzzle" aoch-prepare-puzzle]
    ["Visit Puzzle" aoch-visit-puzzle]))

(provide 'advent-of-code-helper)

;; Local Variables:
;; read-symbol-shorthands: (("aoch-" . "advent-of-code-helper-"))
;; End:
