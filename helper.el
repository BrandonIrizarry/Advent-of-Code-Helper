;; -*- lexical-binding: t -*-

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

(_ (aoc-reset-cookie "~/tmp/scratch/adventofcode/session-cookie.txt")
   ;; Now that the cookie situation is fixed, try again.
   (aoc-retrieve-input))

(defmacro until (test &rest body)
  "Reverse the sense of WHILE."
  (declare (indent 1))
  `(while (not ,test)
     ,@body))

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
;; read-symbol-shorthands: (("aoc-" . "advent-of-code-"))
;; End:
