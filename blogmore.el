;;; blogmore.el --- Tools for working with my personal blog -*- lexical-binding: t -*-
;; Copyright 2026 by Dave Pearson <davep@davep.org>

;; Author: Dave Pearson <davep@davep.org>
;; Version: 2.1
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
;; blogmore.el is a small Emacs package that provides tools that help when
;; I'm writing blog posts for my personal blogs. It is written as a
;; companion for BlogMore <https://blogmore.davep.dev/>, the static site
;; generator I use to create my blogs.
;;
;; It's designed to work out of the box with any blog that has a similar
;; structure to mine, but also design with plenty of customisation options
;; to allow it to be used with a wide variety of different setups.


(eval-when-compile
  (require 'cl-lib))


;; Configuration:

(defgroup blogmore ()
  "Tools for working with my personal blog."
  :group 'convenience
  :prefix "blogmore-")

(defcustom blogmore-blogs nil
  "An association list of blogs to work on and their post paths.

The keys are the names of the blogs, and the values are lists of the form
  (POSTS-DIRECTORY
   POST-TEMPLATE
   POST-MAKER-FUNCTION
   CATEGORY-MAKER-FUNCTION
   TAG-MAKER-FUNCTION
   POST-LINK-FORMAT
   CATEGORY-LINK-FORMAT
   TAG-LINK-FORMAT)

Where:
- POSTS-DIRECTORY is the directory where the blog's posts are stored.
- POST-TEMPLATE is a template for new posts. If nil,
  `blogmore-default-post-template' is used.
- POST-MAKER-FUNCTION is a function that takes a filename and returns a
  string to be used in the post's URL. If nil,
  `blogmore-default-post-maker-function' is used.
- CATEGORY-MAKER-FUNCTION is a function that takes a category name and
  returns a string to be used in the category's URL. If nil,
  `blogmore-default-category-maker-function' is used.
- TAG-MAKER-FUNCTION is a function that takes a tag name and returns a
  string to be used in the tag's URL. If nil,
  `blogmore-default-tag-maker-function' is used.
- POST-LINK-FORMAT is a format string for the post's URL, where %s is
  replaced with the value returned by the post maker function. If nil,
  `blogmore-default-post-link-format' is used.
- CATEGORY-LINK-FORMAT is a format string for the category's URL, where
  %s is replaced with the value returned by the category maker function.
  If nil, `blogmore-default-category-link-format' is used.
- TAG-LINK-FORMAT is a format string for the tag's URL, where %s is
  replaced with the value returned by the tag maker function. If nil,
  `blogmore-default-tag-link-format' is used."
  :type
  '(alist :key-type
          (string :tag "Blog")
          :value-type
          (list :tag "Settings"
                (directory :tag "Posts")
                (choice :tag "Post template"
                        (const :tag "Default" nil)
                        (string :tag "Custom"))
                (choice :tag "Post maker function"
                        (const :tag "Default" nil)
                        (function :tag "Custom"))
                (choice :tag "Category maker function"
                        (const :tag "Default" nil)
                        (function :tag "Custom"))
                (choice :tag "Tag maker function"
                        (const :tag "Default" nil)
                        (function :tag "Custom"))
                (choice :tag "Post link format"
                        (const :tag "Default" nil)
                        (string :tag "Custom"))
                (choice :tag "Category link format"
                        (const :tag "Default" nil)
                        (string :tag "Custom"))
                (choice :tag "Tag link format"
                        (const :tag "Default" nil)
                        (string :tag "Custom"))))
  :group 'blogmore)

(defcustom blogmore-default-post-template "---
title: %1$s
date: %2$s
category:
tags:
---\n\n\n\n"
  "Default template for new blog posts.

Used with `format', where the first argument is the title and the second
argument is the date."
  :type 'string
  :group 'blogmore)

(defcustom blogmore-new-post-hook nil
  "Hook run after creating a new blog post."
  :type 'hook
  :group 'blogmore)

(defcustom blogmore-default-post-maker-function
  (lambda (file)
    (replace-regexp-in-string
     (rx bos (group (+ digit)) "-" (group (+ digit)) "-" (group (+ digit)) "-")
     "\\1/\\2/\\3/"
     (file-name-base (file-name-sans-extension file))))
  "Default function to generate a link for a blog post from its filename."
  :type 'function
  :group 'blogmore)

(defcustom blogmore-default-category-maker-function #'blogmore--slug
  "Default function to generate a slug for a category."
  :type 'function
  :group 'blogmore)

(defcustom blogmore-default-tag-maker-function #'blogmore--slug
  "Default function to generate a slug for a tag."
  :type 'function
  :group 'blogmore)

(defcustom blogmore-default-post-link-format "/%s.html"
  "Default format string for a link to a blog post."
  :type 'string
  :group 'blogmore)

(defcustom blogmore-default-category-link-format "/category/%s/"
  "Default format string for a link to a category."
  :type 'string
  :group 'blogmore)

(defcustom blogmore-default-tag-link-format "/tag/%s/"
  "Default format string for a link to a tag."
  :type 'string
  :group 'blogmore)


;;; Code:

(cl-defstruct (blogmore--blog (:type list))
  "A struct representing the settings for a single blog."
  (blog-title
   nil
   :documentation "The title of the blog")
  (posts-directory
   nil
   :documentation "The directory where the blog's posts are stored")
  (post-template
   nil
   :documentation "A template for new posts")
  (post-maker-function
   nil
   :documentation "A function for making a post's URL")
  (category-maker-function
   nil
   :documentation "A function for making a category's URL")
  (tag-maker-function
   nil
   :documentation "A function for making a tag's URL")
  (post-link-format
   nil
   :documentation "Format string for a link to a postL")
  (category-link-format
   nil
   :documentation "Format string for a link to a category")
  (tag-link-format
   nil
   :documentation "Format string for a link to a tag"))

(defvar blogmore--current-blog nil
  "The current blog being worked on.")

(defun blogmore--chosen-blog ()
  "Get the details of the currently-chosen blog.."
  (cond (blogmore--current-blog
         ;; The user has chosen a blog, so use that.
         blogmore--current-blog)
        ((= (length blogmore-blogs) 1)
         ;; There's only one blog defined, so use that.
         (car blogmore-blogs))
        (blogmore-blogs
         ;; There are multiple blogs defined, so we can't work out the best
         ;; option.
         (error "Please select a blog to work on first; see `blogmore-work-on'"))
        (t
         ;; There are no blogs defined, so we can't work out the best
         ;; option.
         (error "No blogs defined; please add one to `blogmore-blogs'"))))

(defun blogmore--blog-title ()
  "Get the title of the current blog."
  (blogmore--blog-blog-title (blogmore--chosen-blog)))

(defun blogmore--posts-directory ()
  "Get the posts directory for the current blog."
  (expand-file-name (blogmore--blog-posts-directory (blogmore--chosen-blog))))

(defmacro blogmore--setting (setting)
  "Generate a function to get the value of SETTING for the current blog."
  `(defun ,(intern (format "blogmore--%s" setting)) ()
     ,(format "Get the %s for the current blog." setting)
     (or (,(intern (format "blogmore--blog-%s" setting)) (blogmore--chosen-blog))
         ,(intern (format "blogmore-default-%s" setting)))))

(blogmore--setting post-template)
(blogmore--setting post-maker-function)
(blogmore--setting category-maker-function)
(blogmore--setting tag-maker-function)
(blogmore--setting post-link-format)
(blogmore--setting category-link-format)
(blogmore--setting tag-link-format)

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
  (format "%s%s" (file-name-as-directory (blogmore--posts-directory)) (format-time-string "%Y")))

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
    (shell-command-to-string
     (format
      (if (executable-find "rg")
          "rg --no-filename --no-line-number --no-heading \"^%1$s:\" \"%2$s\" -g \"*.md\""
        "find \"%2$s\" -type f -name \"*.md\" -exec grep -hi \"^%1$s:\" /dev/null {} +")
      property (blogmore--posts-directory))) "\n" t)))

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
    (format "Post from %s: " (blogmore--blog-title))
    (directory-files-recursively (blogmore--posts-directory) (rx ".md" eol)))))

(defun blogmore--insert-link (link)
  "Insert Markdown to link to LINK.."
  (save-excursion
    (insert (format "[](%s)" link)))
  (forward-char))

(defun blogmore--with (prompt existing-values)
  "Prompt the user with PROMPT and offer EXISTING-VALUES as completions."
  (if (blogmore--post-p)
      (list (completing-read (format "%s from %s:" prompt (blogmore--blog-title)) existing-values))
    (error "This doesn't look like a blog post")))


;; Commands:

;;;###autoload
(defun blogmore-work-on (blog)
  "Set the current blog to BLOG."
  (interactive (list (completing-read "Blog: " blogmore-blogs nil t)))
  (setq blogmore--current-blog (assoc blog blogmore-blogs)))

;;;###autoload
(defun blogmore-new (title)
  "Start a new blog post with a title of TITLE."
  (interactive (list (read-string (format "Title of new post for %s: " (blogmore--blog-title)))))
  (blogmore--ensure-directory)
  (find-file (blogmore--file-from-title title))
  (when (string-empty-p (buffer-string))
    (insert (format (blogmore--post-template) title (blogmore--now)))
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
  (interactive (blogmore--with "Category" (blogmore--current-categories)))
  (blogmore--set-frontmatter-property "category" category))

;;;###autoload
(defun blogmore-add-tag (tag)
  "Add TAG to the post's tags."
  (interactive (blogmore--with "Tag" (blogmore--current-tags)))
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
   (format (blogmore--post-link-format)
           (funcall (blogmore--post-maker-function) file))))

;;;###autoload
(defun blogmore-link-category (category)
  "Insert a link to CATEGORY on my blog."
  (interactive (blogmore--with "Category" (blogmore--current-categories)))
  (blogmore--insert-link
   (format (blogmore--category-link-format)
           (funcall (blogmore--category-maker-function) category)))
  (save-excursion
    (insert category)))

;;;###autoload
(defun blogmore-link-tag (tag)
  "Insert a link to TAG on my blog."
  (interactive (blogmore--with "Tag" (blogmore--current-tags)))
  (blogmore--insert-link
   (format (blogmore--tag-link-format)
           (funcall (blogmore--tag-maker-function) tag)))
  (save-excursion
    (insert tag)))

(provide 'blogmore)

;;; blogmore.el ends here
