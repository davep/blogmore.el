;;; blogmore.el --- Tools for working with my personal blog -*- lexical-binding: t -*-
;; Copyright 2026 by Dave Pearson <davep@davep.org>

;; Author: Dave Pearson <davep@davep.org>
;; Version: 1.3
;; Keywords: convenience
;; URL: https://github.com/davep/blogmoe.el
;; Package-Requires: ((emacs "27.1") (end-it "1.20"))

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

(defconst blogmore--category-regexp-line (rx bol "category:" (* nonl) eol)
  "Regular expression to match category lines in blog posts.")

(defconst blogmore--category-regexp (rx bol "category:" (* space) (group (* any)) eol)
  "Regular expression for matching a category data.")

(defconst blogmore--tags-regexp-line (rx bol "tags:")
  "Regular expression to match tag lines in blog posts.")

(defconst blogmore--tags-regexp (rx bol "tags:" (* space) (group (* any)) eol)
  "Regular expression to match tag data.")

(defconst blogmore--frontmatter-marker-regexp (rx bol "---" eol)
  "Regular expression to match the frontmatter marker in blog posts.")

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

(defun blogmore--get-all (property)
  "Get a list of all values for PROPERTY from existing posts."
  (delete-dups
   (split-string
    (shell-command-to-string (format "grep -h '^%s:' %s/**/*.md" property blogmore-posts-directory)) "\n" t)))

(defun blogmore--current-categories ()
  "Get a list of categories from existing posts."
  (sort
   (delq
    nil
    (mapcar
     (lambda (candidate)
       (when (string-match blogmore--category-regexp candidate)
         (string-trim (match-string 1 candidate))))
     (blogmore--get-all "category")))))

(defun blogmore--current-tags ()
  "Get a list of tags from existing posts."
  (sort
   (delete-dups
    (flatten-list
     (mapcar
      (lambda (candidate)
        (when (string-match blogmore--tags-regexp candidate)
          (split-string (match-string 1 candidate) "," t " ")))
      (blogmore--get-all "tags"))))))

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
  "Edit FILE from my blog."
  (interactive
   (list
    (completing-read
     "File: "
     (directory-files-recursively blogmore-posts-directory "\\.md$"))))
  (find-file file))

(defun blogmore--with (prompt existing-values)
  "Prompt the user with PROMPT and offer EXISTING-VALUES as completions."
  (if (blogmore--post-p)
      (list (completing-read prompt existing-values))
    (error "This doesn't look like a blog post")))

;;;###autoload
(defun blogmore-set-category (category)
  "Set the category of the post to CATEGORY."
  (interactive (blogmore--with "Category: " (blogmore--current-categories)))
  (save-excursion
    (goto-char (point-min))
    (cond ((re-search-forward blogmore--category-regexp-line nil t)
           (replace-match (format "category: %s" category) t))
          ((re-search-forward blogmore--frontmatter-marker-regexp nil t 2)
           (beginning-of-line)
           (insert (format "category: %s\n" category)))
          (t
           (error "Could not find a location to insert the category")))))

;;;###autoload
(defun blogmore-add-tag (tag)
  "Add TAG to the post's tags."
  (interactive (blogmore--with "Tag: " (blogmore--current-tags)))
  (save-excursion
    (goto-char (point-min))
    (cond ((re-search-forward blogmore--tags-regexp-line nil t)
           (let* ((existing-tags (save-match-data
                                   (string-split (buffer-substring (point) (line-end-position)) "," t " ")))
                  (new-tags (sort (delete-dups (append existing-tags (list tag))))))
             (unless (eolp)
               (kill-line))
             (replace-match (format "tags: %s" (string-join new-tags ", ")) t)))
          ((re-search-forward blogmore--frontmatter-marker-regexp nil t 2)
           (beginning-of-line)
           (insert (format "tags: %s\n" tag)))
          (t
           (error "Could not find a location to insert the tag")))))

(provide 'blogmore)

;;; blogmore.el ends here
