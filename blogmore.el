;;; blogmore.el --- Tools for working with my personal blog -*- lexical-binding: t -*-
;; Copyright 2026 by Dave Pearson <davep@davep.org>

;; Author: Dave Pearson <davep@davep.org>
;; Version: 1.9
;; Keywords: convenience
;; URL: https://github.com/davep/blogmore.el
;; Package-Requires: ((emacs "29.1"))

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
;; blogmore.el is a small Emacs package that provides tools for working with
;; my personal blog. It includes commands for creating new blog posts and
;; editing existing ones, as well as commands for inserting category and tag
;; links.
;;
;; It is written as a companion for BlogMore <https://blogmore.davep.dev/>,
;; the static site generator I use to create my blog.



;; Configuration:

(defgroup blogmore ()
  "Tools for working with my personal blog."
  :group 'convenience
  :prefix "blogmore-")

(defcustom blogmore-posts-directory "~/write/davep.github.com/content/posts/"
  "The directory where blog posts are stored."
  :type 'directory
  :group 'blogmore)

(defcustom blogmore-template "---
title: %1$s
date: %2$s
category:
tags:
---\n\n\n\n"
  "Template for new blog posts.

Used with `format', where the first argument is the title and the second
argument is the date."
  :type 'string
  :group 'blogmore)

(defcustom blogmore-new-post-hook nil
  "Hook run after creating a new blog post."
  :type 'hook
  :group 'blogmore)

(defcustom blogmore-post-maker-function
  (lambda (file)
    (replace-regexp-in-string
     (rx bos (group (+ digit)) "-" (group (+ digit)) "-" (group (+ digit)) "-")
     "\\1/\\2/\\3/"
     (file-name-base (file-name-sans-extension file))))
  "Function to generate a link for a blog post from its filename."
  :type 'function
  :group 'blogmore)

(defcustom blogmore-category-maker-function #'blogmore--slug
  "Function to generate a slug for a category."
  :type 'function
  :group 'blogmore)

(defcustom blogmore-tag-maker-function #'blogmore--slug
  "Function to generate a slug for a tag."
  :type 'function
  :group 'blogmore)

(defcustom blogmore-post-link-format "/%s.html"
  "Format string for a link to a blog post."
  :type 'string
  :group 'blogmore)

(defcustom blogmore-category-link-format "/category/%s/"
  "Format string for a link to a category."
  :type 'string
  :group 'blogmore)

(defcustom blogmore-tag-link-format "/tag/%s/"
  "Format string for a link to a tag."
  :type 'string
  :group 'blogmore)


;;; Code:

(defconst blogmore--frontmatter-marker-regexp (rx bol "---" eol)
  "Regular expression to match the frontmatter marker in blog posts.")

(defun blogmore--slug (title)
  "Generate a slug from the given TITLE."
  (thread-last
    title
    downcase
    (replace-regexp-in-string (rx (+ (not (any "0-9a-z")))) "-")
    (replace-regexp-in-string (rx (or (seq bol "-") (seq "-" eol))) "")))

(defun blogmore--now ()
  "Return the current date and time as a string."
  (format-time-string "%Y-%m-%d %H:%M:%S%z"))

(defun blogmore--frontmatter-bounds ()
  "Return the bounds of the frontmatter as a cons cell (START . END).

START is the position immediately after the first frontmatter marker,
and END is the position of the second frontmatter marker. If no
frontmatter is found, return nil."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward blogmore--frontmatter-marker-regexp nil t)
      (let ((start (match-end 0)))
        (when (re-search-forward blogmore--frontmatter-marker-regexp nil t)
          (cons start (match-beginning 0)))))))

(defun blogmore--locate-frontmatter (property)
  "Locate the line for PROPERTY in the frontmatter.

If the property is found, return a list of the form (START END VALUE),
where START and END are the bounds of the value and VALUE is the current
value of the property. If the property is not found, return nil.

If the property is found `point' is left at the beginning of the value.
If the property is not found, `point' is left at the end of the
frontmatter."
  (when-let ((bounds (blogmore--frontmatter-bounds)))
    (with-restriction (car bounds) (cdr bounds)
      (goto-char (point-min))
      (if (re-search-forward (rx bol (literal property) ":") nil t)
          (list
           (point)
           (line-end-position)
           (string-trim (buffer-substring-no-properties (point) (line-end-position))))
        (goto-char (point-max))
        nil))))

(defun blogmore--get-frontmatter-property (property)
  "Get the value of PROPERTY from the frontmatter, or nil if it doesn't exist."
  (save-excursion
    (when-let ((location (blogmore--locate-frontmatter property)))
      (nth 2 location))))

(defun blogmore--set-frontmatter-property (property value)
  "Set the value of PROPERTY in the frontmatter to VALUE."
  (save-excursion
    (if-let ((location (blogmore--locate-frontmatter property)))
        (progn
          (goto-char (nth 0 location))
          (unless (eolp)
            (kill-line))
          (insert (format " %s" value)))
      (beginning-of-line)
      (insert (format "%s: %s\n" property value)))))

(defun blogmore--post-directory ()
  "Get the directory for the current year's blog posts."
  (format "%s%s" (file-name-as-directory blogmore-posts-directory) (format-time-string "%Y")))

(defun blogmore--ensure-directory ()
  "Ensure that the given directory exists."
  (let ((post-directory (blogmore--post-directory)))
    (unless (file-exists-p post-directory)
      (make-directory post-directory t))))

(defun blogmore--file-from-title (title)
  "Generate a filename for a blog post from the given TITLE."
  (format
   "%s%s-%s.md"
   (file-name-as-directory (blogmore--post-directory))
   (format-time-string "%Y-%m-%d")
   (blogmore--slug title)))

(defun blogmore--post-p ()
  "Does this buffer look like a blog post?"
  (blogmore--frontmatter-bounds))

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
       (when (string-match (rx bol "category:" (* space) (group (* any)) eol) candidate)
         (string-trim (match-string 1 candidate))))
     (blogmore--get-all "category")))))

(defun blogmore--current-tags ()
  "Get a list of tags from existing posts."
  (sort
   (delete-dups
    (flatten-list
     (mapcar
      (lambda (candidate)
        (when (string-match (rx bol "tags:" (* space) (group (* any)) eol) candidate)
          (split-string (match-string 1 candidate) "," t " ")))
      (blogmore--get-all "tags"))))))

(defun blogmore--post-picker ()
  "Pick a post from the list of existing posts."
  (list
   (completing-read
    "Post: "
    (directory-files-recursively blogmore-posts-directory (rx ".md" eol)))))

(defun blogmore--insert-link (link)
  "Insert Markdown to link to LINK.."
  (save-excursion
    (insert (format "[](%s)" link)))
  (forward-char))

(defun blogmore--with (prompt existing-values)
  "Prompt the user with PROMPT and offer EXISTING-VALUES as completions."
  (if (blogmore--post-p)
      (list (completing-read prompt existing-values))
    (error "This doesn't look like a blog post")))


;; Commands:

;;;###autoload
(defun blogmore-new (title)
  "Start a new blog post with a title of TITLE."
  (interactive "sTitle: ")
  (blogmore--ensure-directory)
  (find-file (blogmore--file-from-title title))
  (when (string-empty-p (buffer-string))
    (insert (format blogmore-template title (blogmore--now)))
    (forward-line -2)
    (save-excursion
      (run-hooks 'blogmore-new-post-hook))))

;;;###autoload
(defun blogmore-edit (file)
  "Edit FILE from my blog."
  (interactive (blogmore--post-picker))
  (find-file file))

;;;###autoload
(defun blogmore-set-category (category)
  "Set the category of the post to CATEGORY."
  (interactive (blogmore--with "Category: " (blogmore--current-categories)))
  (blogmore--set-frontmatter-property "category" category))

;;;###autoload
(defun blogmore-add-tag (tag)
  "Add TAG to the post's tags."
  (interactive (blogmore--with "Tag: " (blogmore--current-tags)))
  (blogmore--set-frontmatter-property
   "tags"
   (string-join
    (sort
     (delete-dups
      (append
       (string-split (or (blogmore--get-frontmatter-property "tags") "") "," t " ")
       (list tag)))) ", ")))

;;;###autoload
(defun blogmore-update-date ()
  "Update the date of the post to the current date and time."
  (interactive)
  (blogmore--set-frontmatter-property "date" (blogmore--now)))

;;;###autoload
(defun blogmore-update-modified ()
  "Update the modified date of the post to the current date and time."
  (interactive)
  (blogmore--set-frontmatter-property "modified" (blogmore--now)))

;;;###autoload
(defun blogmore-link-post (file)
  "Insert a link to FILE from my blog."
  (interactive (blogmore--post-picker))
  (blogmore--insert-link
   (format blogmore-post-link-format
           (funcall blogmore-post-maker-function file))))

;;;###autoload
(defun blogmore-link-category (category)
  "Insert a link to CATEGORY on my blog."
  (interactive (blogmore--with "Category: " (blogmore--current-categories)))
  (blogmore--insert-link
   (format blogmore-category-link-format
           (funcall blogmore-category-maker-function category)))
  (save-excursion
    (insert category)))

;;;###autoload
(defun blogmore-link-tag (tag)
  "Insert a link to TAG on my blog."
  (interactive (blogmore--with "Tag: " (blogmore--current-tags)))
  (blogmore--insert-link
   (format blogmore-tag-link-format
           (funcall blogmore-tag-maker-function tag)))
  (save-excursion
    (insert tag)))

(provide 'blogmore)

;;; blogmore.el ends here
