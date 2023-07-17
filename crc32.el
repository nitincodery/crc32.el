;;; crc32.el --- CRC32 checksum calculation in Emacs Lisp -*- lexical-binding: t -*-

;; Author: Nitin Choudhary <nitin@codery.xyz>

;;; Commentary:
;;
;; This package provides a pure Emacs Lisp implementation of the CRC32
;; (Cyclic Redundancy Check 32) algorithm. It can be used to generate
;; checksums for strings to detect data corruption or verify integrity.
;;
;; The implementation uses a precomputed lookup table for efficiency
;; and follows the standard CRC32 polynomial (0xedb88320).
;;
;; To use, simply call (crc32 "your-string") to get the checksum.
;; The output format can be customized via `crc32-output-format'.

;;; Code:

(defcustom crc32-output-format 'decimal
  "Format for CRC32 checksum output.
Can be set to 'hexadecimal for hex output (e.g., \"a1b2c3d4\") or
'decimal for decimal output (e.g., \"2709874372\")."
  :type '(choice (const :tag "Hexadecimal" hexadecimal)
                 (const :tag "Decimal" decimal))
  :group 'crc32)

(defun make-crc32-table ()
  "Generate and return a CRC32 lookup table.
This function creates a 256-element vector used for efficient CRC32
calculations. The table is based on the CRC32 polynomial 0xedb88320."
  (let ((table (make-vector 256 0)))
    (dotimes (n 256)
      (let ((c n))
        (dotimes (_ 8)
          (setq c (if (= (logand c 1) 1)
                      (logxor (ash c -1) #xedb88320)
                    (ash c -1))))
        (aset table n c)))
    table))

(defun crc32 (string)
  "Calculate CRC32 checksum for STRING.
Returns the checksum as a string, formatted according to
`crc32-output-format' (either hexadecimal or decimal).
The calculation uses a precomputed lookup table for efficiency."
  (let ((crc 0)
        (crc32-table (make-crc32-table)))
    (setq crc (logxor crc #xffffffff))
    (dolist (byte (string-to-list string))
      (setq crc (logxor (logand (ash crc -8) #xffffff) 
                        (aref crc32-table (logand (logxor crc byte) #xff)))))
    (let ((result (logxor crc #xffffffff)))
      (if (eq crc32-output-format 'hexadecimal)
          (format "%x" result)
        (format "%d" result)))))

(provide 'crc32)

;;; crc32.el ends here
