;; -*- lexical-binding: t -*-

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
         "The hash used to validate HTTP requests made to the Advent of
Code website.")))

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
(defun aoch-main (year day)
  (interactive "nYear: \nnDay: ")
  (let ((cookie (condition-case nil
                    (aoch-load-cookie)
                  (file-missing
                   (user-error "Define a cookie first with 'advent-of-code-helper-bootstrap'.")))))
    (url-cookie-store "session" (get-hash cookie) nil ".adventofcode.com" "/")
    (url-retrieve (format "https://adventofcode.com/%d/day/%d/input" year day)
                  (lambda (status)
                    (pcase (cl-third (plist-get status :error))
                      ((pred null)
                       (re-search-forward "^$" nil t)
                       (delete-region (point-min) (point))
                       (print (buffer-string)))
                      (404
                       (error "Puzzle hasn't been published yet"))
                      (500
                       (error "Bad cookie hash: run 'advent-of-code-helper-bootstrap'")))))))

(completing-read "Year: " (mapcar #'number-to-string (number-sequence 2015 (string-to-number (format-time-string "%Y")))))
(completing-read "Day: " (mapcar #'number-to-string (number-sequence 1 25)))

(aoch-main 2023 5)

;; FIXME
(defun aoc-setup-year-and-day (year day)
  "Setup directory for YEAR and DAY.

Assuming top level directory named TOP, this function creates
TOP/YEAR/day/DAY, creating whatever missing parent directories
are needed per MAKE-DIRECTORY.

The directory structure is meant to echo the one used by Advent
of Code itself."
  (interactive "nYear: \nnDay: ")
  (make-directory (format "~/tmp/scratch/adventofcode/%d/day/%d" year day)))

;; Local Variables:
;; read-symbol-shorthands: (("aoch-" . "advent-of-code-helper-"))
;; End:
