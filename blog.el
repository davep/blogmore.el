;;; blog.el --- Tools for working with my personal blog -*- lexical-binding: t -*-
;; Copyright 2026 by Dave Pearson <davep@davep.org>

;; Author: Dave Pearson <davep@davep.org>
;; Version: 1.0
;; Keywords: convenience
;; URL: https://github.com/davep/blog.el
;; Package-Requires: ((emacs "25.1"))

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

(defconst blog-directory "~/write/davep.github.com/"
  "Root directory for my blog.")

(defconst blog-posts-directory (concat blog-directory "content/posts")
  "Directory where blog posts are stored.")

(defconst blog-template "---
layout: post
title: %s
category:
tags:
date: %s
---\n\n"
  "Template for new blog posts.")

(defun blog--slug (title)
  "Generate a slug from the given TITLE."
  (thread-last
    title
    downcase
    (replace-regexp-in-string "[^a-z0-9]+" "-")
    (replace-regexp-in-string "^-\\|-$" "")))

(defun blog--post-directory ()
  "Get the directory for the current year's blog posts."
  (format "%s/%s" blog-posts-directory (format-time-string "%Y")))

(defun blog--ensure-directory ()
  "Ensure that the given directory exists."
  (let ((post-directory (blog--post-directory)))
    (unless (file-exists-p post-directory)
      (make-directory post-directory t))))

(defun blog--file-from-title (title)
  "Generate a filename for a blog post from the given TITLE."
  (format
   "%s/%s-%s.md"
   (blog--post-directory)
   (format-time-string "%Y-%m-%d")
   (blog--slug title)))

;;;###autoload
(defun blog-new (title)
  "Start a new blog post."
  (interactive "sTitle: ")
  (blog--ensure-directory)
  (find-file (blog--file-from-title title))
  (when (string-empty-p (buffer-string))
    (insert
     (format
      blog-template
      title
      (format-time-string "%Y-%m-%d %H:%M:%S %z")))))

;;;###autoload
(defun blog-edit (file)
  "Open an existing blog post for editing."
  (interactive
   (list
    (completing-read
     "File: "
     (directory-files-recursively blog-posts-directory "\\.md$"))))
  (find-file file))

(provide 'blog)

;;; blog.el ends here
