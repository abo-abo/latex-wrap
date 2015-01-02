;;; latex-wrap.el --- wrap selection with a LaTeX environment

;; Copyright (C) 2015 Oleh Krehel

;; Author: Oleh Krehel <ohwoeowho@gmail.com>
;; URL: https://github.com/abo-abo/latex-wrap
;; Version: 0.1
;; Package-Requires: ((helm "1.5.3"))
;; Keywords: latex

;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;;  You can call `latex-wrap-region' with or without an active region.
;;  You'll be prompted for an environment with `helm'.
;;  If it's `itemize' or `enumerate', add "\item"s appropriately.

(require 'helm)

(defun latex-wrap-region ()
  "Wrap a LaTeX environment around the region"
  (interactive)
  (let ((env (helm :sources
                   '((name . "LaTeX environment")
                     (candidates . ("itemize"
                                    "enumerate"
                                    "left"
                                    "center"
                                    "right"))
                     (action . identity))
                   :buffer "*latex-wrap*"))
        beg end)
    (cond ((region-active-p)
           (setq end (region-end))
           (goto-char (region-beginning))
           (deactivate-mark)
           (setq beg (move-beginning-of-line 1)))

          ((looking-back "^ *")
           (setq beg (line-beginning-position))
           (setq end (line-end-position)))

          (t
           (move-end-of-line 1)
           (newline-and-indent)
           (setq beg (point))
           (setq end (point))))
    (let ((lines (or (split-string
                      (buffer-substring-no-properties beg end)
                      "\n" t) '(""))))
      (delete-region beg end)
      (indent-for-tab-command)
      (insert (format "\\begin{%s}" env))
      (dolist (line lines)
        (insert "\n"
                (if (member env '("itemize" "enumerate"))
                    "\\item "
                  "")
                line)
        (indent-for-tab-command))
      (insert (format "\n\\end{%s}" env))
      (indent-for-tab-command)
      (line-move -1)
      (move-end-of-line 1))))

(provide 'latex-wrap)
