;; -*- lexical-binding: t -*-

;; To run, eval this file then run M-x ert

(require 'ert)

(defun hym/test-monday (input)
  "Parse INPUT (\"YYYY-MM-DD\"), run hym/get-monday, return ISO string."
  (let* ((parsed (date-to-time input))
         (monday (hym/get-monday parsed)))
    (format-time-string "%F" monday)))

(ert-deftest hym/get-monday-test ()
  "Test hym/get-monday"
  (should (equal (hym/test-monday "2025-12-07") "2025-12-01"))  ;; Sunday
  (should (equal (hym/test-monday "2025-12-08") "2025-12-08"))  ;; Monday
  (should (equal (hym/test-monday "2025-12-10") "2025-12-08"))  ;; Wednesday
  (should (equal (hym/test-monday "2025-12-13") "2025-12-08"))  ;; Saturday
  (should (equal (hym/test-monday "2025-12-15") "2025-12-15"))  ;; Monday
  )
