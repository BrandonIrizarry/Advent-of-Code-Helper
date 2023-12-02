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

If already loaded, return T."
  (url-cookie-store "session"
                    (aoc-retrieve-hash source)
                    nil
                    ".adventofcode.com"
                    "/"))

(aoc-load-cookie "~/tmp/scratch/adventofcode/session-cookie.txt")

;; Setting
;;
;; Save the new hash, and make it available in the context of the
;; current program by returning it.
(cl-defgeneric aoc-reset-cookie-storage (destination)
  "Write a new cookie-hash to DESTINATION."
  (error "Invalid destination: %s" destination))

(cl-defmethod aoc-reset-cookie-storage ((destination string))
  "Write a new cookie-hash to filename DESTINATION."
  (let ((freshhash (read-string "Hash: ")))
    (with-temp-buffer
      (insert freshhash)
      (write-file destination)
      freshhash)))

(defun aoc-retrieve-input ()
  (with-current-buffer (url-retrieve-synchronously "https://adventofcode.com/2023/day/1/input")
    (goto-char (point-min))
    (re-search-forward "HTTP/1.1 \\([[:digit:]]+\\)" (line-end-position))
    (pcase (match-string 1)
      ("200" (re-search-forward "^$" nil t)
             (delete-region (point-min) (point))
             (buffer-string))
      (_ (aoc-reset-cookie-storage "~/tmp/scratch/adventofcode/session-cookie.txt")
         (aoc-load-cookie "~/tmp/scratch/adventofcode/session-cookie.txt")
         ;; Now that the cookie situation is fixed, try again.
         (aoc-retrieve-input)))))

(aoc-retrieve-input)

(defun aoc--set-cookie ()
  "Load a hash value from a file to use as your Advent of Code
authentication cookie.

If nonexistent, ask the user for the value, then write that value
to the file for next time."
  (let* ((filename (format "%s/session-cookie.txt" *top-dir*))
         (hash (if (file-exists-p filename)
                   (with-temp-buffer
                     (insert-file-contents filename)
                     (buffer-string))
                 (let ((freshhash (read-string "Session file missing; enter hash now: ")))
                   (with-temp-buffer
                     (insert freshhash)
                     (write-file filename)
                     freshhash)))))
    (url-cookie-store "session" hash nil ".adventofcode.com" "/")))

(defun aoc-session-init (year day)
  "Initialize problem environment for Advent of Code puzzle
corresponding to YEAR and DAY."
  (interactive "nYear: \nnDay: ")
  (defvar *top-dir*)
  (let ((*top-dir* "~/tmp/scratch/adventofcode"))
    (unless (url-cookie-retrieve ".adventofcode.com" "/")
      (aoc--set-cookie))
    (let* ((date-slug (format "%d/day/%d" year day))
           (new-directory (format "%s/%s" *top-dir* date-slug)))
      (unless (file-exists-p new-directory)
        (make-directory new-directory t))
      (url-copy-file (format "https://adventofcode.com/%s/input" date-slug)
                     (format "%s/input.txt" new-directory)
                     t))))

;; Local Variables:
;; read-symbol-shorthands: (("aoc-" . "advent-of-code-"))
;; End:
