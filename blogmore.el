;;; blogmore.el --- Tools for working with my personal blog -*- lexical-binding: t -*-
;; Copyright 2026 by Dave Pearson <davep@davep.org>

;; Author: Dave Pearson <davep@davep.org>
;; Version: 1.2
;; Keywords: convenience
;; URL: https://github.com/davep/blogmoe.el
;; Package-Requires: ((emacs "25.1") (end-it "1.20"))

;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at your
;; option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
;; Public License for more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; blog.el is a small Emacs package that provides tools for working with my
;; personal blog. It includes commands for creating new blog posts and
;; editing existing ones, as well as some helper functions for generating
;; slugs and ensuring directories exist.

;;; Code:

(require 'end-it)

(defconst blogmore-directory "~/write/davep.github.com/"
  "Root directory for my blog.")

(defconst blogmore-posts-directory (concat blogmore-directory "content/posts")
  "Directory where blog posts are stored.")

(defconst blogmore-template "---
layout: post
title: %s
category:
tags:
date: %s
---\n\n\n\n"
  "Template for new blog posts.")

(defconst blogmore--category-regexp (rx bol "category:" (* space) (group (+ any)) eol)
  "Regular expression to match category lines in blog posts.")

(defun blogmore--slug (title)
  "Generate a slug from the given TITLE."
  (thread-last
    title
    downcase
    (replace-regexp-in-string "[^a-z0-9]+" "-")
    (replace-regexp-in-string "^-\\|-$" "")))

(defun blogmore--post-directory ()
  "Get the directory for the current year's blog posts."
  (format "%s/%s" blogmore-posts-directory (format-time-string "%Y")))

(defun blogmore--ensure-directory ()
  "Ensure that the given directory exists."
  (let ((post-directory (blogmore--post-directory)))
    (unless (file-exists-p post-directory)
      (make-directory post-directory t))))

(defun blogmore--file-from-title (title)
  "Generate a filename for a blog post from the given TITLE."
  (format
   "%s/%s-%s.md"
   (blogmore--post-directory)
   (format-time-string "%Y-%m-%d")
   (blogmore--slug title)))

(defun blogmore--post-p ()
  "Does this buffer look like a blog post?"
  (save-excursion
    (goto-char (point-min))
    (looking-at "---\n")))

(defun blogmore--current-categories ()
  "Get a list of categories from existing posts."
  (sort
   (delq
    nil
    (mapcar
     (lambda (candidate)
       (when (string-match blogmore--category-regexp candidate)
         (string-trim (match-string 1 candidate))))
     (delete-dups
      (split-string
       (shell-command-to-string (format "grep -h '^category:' %s/**/*.md" blogmore-posts-directory)) "\n" t))))))

;;;###autoload
(defun blogmore-new (title)
  "Start a new blog post with a title of TITLE."
  (interactive "sTitle: ")
  (blogmore--ensure-directory)
  (find-file (blogmore--file-from-title title))
  (when (string-empty-p (buffer-string))
    (insert
     (format
      blogmore-template
      title
      (format-time-string "%Y-%m-%d %H:%M:%S %z")))
    (forward-line -2)
    (save-excursion
      (end-it))))

;;;###autoload
(defun blogmore-edit (file)
  "Edit FILE from my blog.."
  (interactive
   (list
    (completing-read
     "File: "
     (directory-files-recursively blogmore-posts-directory "\\.md$"))))
  (find-file file))

;;;###autoload
(defun blogmore-set-category (category)
  "Set the category of the post to CATEGORY."
  (interactive (list (completing-read "Category: " (blogmore--current-categories))))
  (unless (blogmore--post-p)
    (error "This doesn't look like a blog post"))
  (save-excursion
    (goto-char (point-min))
    ;; TODO: If the category is empty it seems to nuke the next line.
    (when (re-search-forward blogmore--category-regexp)
      (replace-match (concat "category: " category) t))))

(provide 'blogmore)

;;; blogmore.el ends here
