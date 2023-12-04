;; -*- lexical-binding: t -*-

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

(require 'eieio-custom)
(require 'eieio-base)

(defclass aoch-cookie (eieio-named eieio-persistent)
  ((hash :initarg :hash
         :initform ""
         :custom string
         :label "Cookie hash"
         :documentation
         "The hash used to validate HTTP requests made to the Advent of
Code website.")))

(cl-defmethod eieio-done-customizing ((cookie aoch-cookie))
  "Override this hook to ensure that the cookie is saved to disk
after customization."
  (eieio-persistent-save cookie))

(setq foo (aoch-cookie :object-name "Advent of Code Helper Cookie"
                       :file (let ((name (concat aoch-top-level-directory aoch-cookie-name)))
                               (unless (file-exists-p name)
                                 (make-empty-file name))
                               name)))

(eieio-customize-object foo)

;; This seems to be the key for using the default implementation.
(cl-defmethod aoch-load-cookie ((cookie aoch-cookie))
  (let ((default-directory (file-name-directory (oref cookie file))))
    (eieio-persistent-read (eieio-persistent-path-relative foo aoch-cookie-name)
                           aoch-cookie)))

(aoch-load-cookie foo)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(unless (url-cookie-retrieve ".adventofcode.com" "/")
  (user-error "Run advent-of-code-session-bootstrap"))

(aoc-session-load)


(cl-defun aoc-retrieve-input (&key year day)
  "Retrieve and return puzzle input from URL named INPUT-SOURCE.

If unsuccessful, return NIL."
  (with-current-buffer (url-retrieve-synchronously (format "https://adventofcode.com/%d/day/%d/input" year day))
    (goto-char (point-min))
    (re-search-forward "HTTP/1.1 \\([[:digit:]]+\\)" (line-end-position))
    (when (string= (match-string 1) "200")
      (re-search-forward "^$" nil t)
      (delete-region (point-min) (point))
      (buffer-string))))

;; Make a cookie
;; Customize it
;; Save it
(defun aoc-session-bootstrap ()
  "Bootstrap the AOC cookie."
  (interactive)
  (let ((cookie (aoc-session-cookie :object-name "Advent of Code Session Cookie"
                                    :file aoc-session-cookie-file)))
    (eieio-customize-object cookie)
))

(defun aoc-session-load ()
  "Load an existing cookie.

If the cookie is missing, return NIL."
  (let ((cookie-file (concat aoc-top-level-directory
                             aoc-session-cookie-file)))
    (when (file-exists-p cookie-file)
      (let ((cookie (eieio-persistent-read
                     aoc-session-cookie)))
        cookie))))

(aoc-session-load)

(aoc-retrieve-input :year 2023 :day 1)




;;; Commentary
;;;
;;; This is meant to use a session from a logged-in Advent of Code
;;; session. However, the file being loaded currently uses an old
;;; cookie, and I'm still able to download input!
;;;
;;; Apparently, this lets me use my old cookies to log in, which is
;;; interesting. If I corrupt the old hash I'm using by one digit -
;;; thus using a hash never assigned to me - it breaks.
(cl-defgeneric aoc-retrieve-hash (source)
  "Retrieve a cookie-hash value from a generic SOURCE."
  (error "Invalid source: %s" source))

(cl-defmethod aoc-retrieve-hash ((source string))
  "Retrieve a cookie-hash from filename SOURCE."
  (when (file-exists-p source)
    (with-temp-buffer
      (insert-file-contents source)
      (buffer-string))))

;; Getting
;;
;; If there is no cookie associated with Advent of Code, we can't
;; access input data, so let's try to read it from a file.
(cl-defgeneric aoc-load-cookie (source)
  "Load the cookie from a generic SOURCE if isn't loaded yet."
  (error "Invalid source: %s" source))

(cl-defmethod aoc-load-cookie ((source string))
  "Load the cookie from filename SOURCE.

Note that we might want to reload a cookie in case there's
something wrong with the existing one (corrupt/nonsense value,
obsolete cookie.) Hence calling this function will always clobber
the existing cookie."
  (let ((hash (aoc-retrieve-hash source)))
    (when hash
      (url-cookie-store "session"
                        hash
                        nil
                        ".adventofcode.com"
                        "/"))))

(aoc-load-cookie "~/tmp/scratch/adventofcode/session-cookie.txt")

;; Setting
;;
;; Save the new hash, and make it available in the context of the
;; current program by returning it.
(cl-defgeneric aoc-reset-cookie-storage (destination)
  "Write a new cookie-hash to DESTINATION."
  (error "Invalid destination: %s" destination))

(cl-defmethod aoc-reset-cookie ((destination string))
  "Write a new cookie-hash to filename DESTINATION, and load it."
  (let ((freshhash (read-string "Hash: ")))
    (with-temp-buffer
      (insert freshhash)
      (write-file destination))
    (aoc-load-cookie destination)))

(defun aoc-retrieve-input (input-source)
  "Retrieve and return puzzle input from URL named INPUT-SOURCE.

If unsuccessful, return NIL."
  (with-current-buffer (url-retrieve-synchronously input-source)
    (goto-char (point-min))
    (re-search-forward "HTTP/1.1 \\([[:digit:]]+\\)" (line-end-position))
    (when (string= (match-string 1) "200")
      (re-search-forward "^$" nil t)
      (delete-region (point-min) (point))
      (buffer-string))))

(progn
  (when (null url-cookie-storage)
    (aoc-load-cookie "~/tmp/scratch/adventofcode/session-cookie.txt"))

  (cl-do ((puzzle-input (aoc-retrieve-input "https://adventofcode.com/2023/day/1/input")
                        (aoc-retrieve-input "https://adventofcode.com/2023/day/1/input")))
         (puzzle-input)
    (message "hi")))

;; TODO: create directory structure. write defun.
;;
;; Assume the top-level directory is given (either as a plain global
;; variable, or as a customizable variable)
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
