;;; blogmore.el --- Tools for working with my personal blog -*- lexical-binding: t -*-
;; Copyright 2026 by Dave Pearson <davep@davep.org>

;; Author: Dave Pearson <davep@davep.org>
;; Version: 4.1
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
;; structure to mine, but also designed with plenty of customisation options
;; to allow it to be used with a wide variety of different setups.


;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))
(require 'eieio-custom)
(require 'parse-time)
(require 'transient)
(require 'ucs-normalize)


(defclass blogmore-blog ()
  ((blog-title
    :initarg :title
    :initform ""
    :type string
    :custom string
    :label "Blog Title"
    :accessor blogmore-blog-title
    :documentation "The title of the blog")
   (posts-directory
    :initarg :posts-directory
    :initform ""
    :type string
    :custom directory
    :label "Posts Directory"
    :documentation "The directory where the blog's posts are stored")
   (post-subdirectory-function
    :initarg :post-subdirectory-function
    :initform nil
    :type (or null function)
    :custom (choice (const :tag "Default") function)
    :label "Post Subdirectory Function"
    :documentation "A function for generating a subdirectory for a new post")
   (post-file-name-from-title-function
    :initarg :post-file-name-from-title-function
    :initform nil
    :type (or null function)
    :custom (choice (const :tag "Default") function)
    :label "Post File Name From Title Function"
    :documentation "A function for generating a filename for a new post from its title")
   (post-template
    :initarg :post-template
    :initform nil
    :type (or null string)
    :custom (choice (const :tag "Default") function)
    :label "Post Template"
    :documentation "A template for new posts")
   (post-maker-function
    :initarg :post-maker-function
    :initform nil
    :type (or null function)
    :custom (choice (const :tag "Default") function)
    :label "Post Maker Function"
    :documentation "A function for making a post's URL")
   (category-maker-function
    :initarg :category-maker-function
    :initform nil
    :type (or null function)
    :custom (choice (const :tag "Default") function)
    :label "Category Maker Function"
    :documentation "A function for making a category's URL")
   (tag-maker-function
    :initarg :tag-maker-function
    :initform nil
    :type (or null function)
    :custom (choice (const :tag "Default") function)
    :label "Tag Maker Function"
    :documentation "A function for making a tag's URL")
   (post-link-format
    :initarg :post-link-format
    :initform nil
    :type (or null string)
    :custom (choice (const :tag "Default") string)
    :label "Post Link Format"
    :documentation "Format string for a link to a post")
   (category-link-format
    :initarg :category-link-format
    :initform nil
    :type (or null string)
    :custom (choice (const :tag "Default") string)
    :label "Category Link Format"
    :documentation "Format string for a link to a category")
   (tag-link-format
    :initarg :tag-link-format
    :initform nil
    :type (or null string)
    :custom (choice (const :tag "Default") string)
    :label "Tag Link Format"
    :documentation "Format string for a link to a tag"))
  :documentation "A class representing the settings for a single blog.")


;; Frontmatter internals.

(defconst blogmore--frontmatter-marker-regexp (rx bol "---" eol)
  "Regular expression to match the frontmatter marker in blog posts.")

(defun blogmore--frontmatter-bounds ()
  "Return the bounds of the frontmatter as a cons cell (START . END).

START is the position immediately after the first frontmatter marker,
and END is the position of the second frontmatter marker. If no
frontmatter is found, return nil."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward blogmore--frontmatter-marker-regexp nil t)
      (let ((start (1+ (match-end 0))))
        (when (re-search-forward blogmore--frontmatter-marker-regexp nil t)
          (cons start (match-beginning 0)))))))

(cl-defstruct blogmore--frontmatter-property-location
  "A struct representing the location of a property in the frontmatter."
  (start nil :documentation "The position of the start of the property's value.")
  (end nil :documentation "The position of the end of the property's value.")
  (value nil :documentation "The current value of the property."))

(defun blogmore--locate-frontmatter (property)
  "Locate the line for PROPERTY in the frontmatter.

If the property is found, return an instance of
`blogmore--frontmatter-property-location' with the start, end, and value
of the property. If the property is not found, return nil.

If the property is found `point' is left at the beginning of the value.
If the property is not found, `point' is left at the end of the
frontmatter.

If there are no frontmatter markers, return nil and leave `point`
unchanged."
  (when-let ((bounds (blogmore--frontmatter-bounds)))
    (with-restriction (car bounds) (cdr bounds)
      (goto-char (point-min))
      (if (re-search-forward (rx bol (literal property) ":") nil t)
          (make-blogmore--frontmatter-property-location
           :start (point)
           :end (line-end-position)
           :value (string-trim
                   (buffer-substring-no-properties
                    (point)
                    (line-end-position))))
        (ignore (goto-char (point-max)))))))

(defun blogmore--frontmatter-p (property)
  "Return non-nil if PROPERTY exists in the frontmatter."
  (save-excursion
    (blogmore--locate-frontmatter property)))


;; Public utility macros and functions.

(defmacro blogmore-with-post (post-file &rest body)
  "Execute BODY with POST-FILE as the current buffer."
  (declare (indent 1))
  `(with-temp-buffer
     (insert-file-contents ,post-file)
      ,@body))

(defun blogmore-clean-time-string (time-string)
  "Clean TIME-STRING to the format YYYY-MM-DDTHH:MM:SS+NNNN.

Given a time string that is roughly in ISO8601 format, clean it to a
format that can be parsed by `parse-iso8601-time-string'. Currently this
revolves around cleaning up any space between the date and time, and
between the time and timezone, to ensure that the string is in a format
that can be parsed."
  (replace-regexp-in-string
   (rx (group (= 4 digit) "-" (= 2 digit) "-" (= 2 digit))
       (1+ (any " T"))
       (group (= 2 digit) ":" (= 2 digit) ":" (= 2 digit))
       (0+ space)
       (group (any "+-") (= 4 digit)))
   "\\1T\\2\\3"
   time-string))

(defun blogmore-slug (title)
  "Generate a slug from the given TITLE."
  (thread-last
    title
    ;; Make everything lowercase.
    downcase
    ;; "ASCIIfy" as many accented characters as possible.
    ucs-normalize-NFKD-string
    (seq-filter (lambda (char) (or (< char #x300) (> char #x36F))))
    concat
    ;; Characters that are just flat out removed.
    (replace-regexp-in-string (rx (one-or-more (any "'\""))) "")
    ;; Replace any run of characters that aren't letters or numbers with a
    ;; single dash.
    (replace-regexp-in-string (rx (one-or-more (not (any "0-9a-z")))) "-")
    ;; Remove any leading or trailing dashes that may have been introduced
    ;; by the previous step.
    (replace-regexp-in-string (rx (or (seq bol "-") (seq "-" eol))) "")))

(defun blogmore-get-frontmatter (property)
  "Get the value of PROPERTY from the frontmatter, or nil if it doesn't exist."
  (save-excursion
    (when-let ((location (blogmore--locate-frontmatter property)))
      (blogmore--frontmatter-property-location-value location))))

(defun blogmore-set-frontmatter (property value)
  "Set the value of PROPERTY in the frontmatter to VALUE.

If a frontmatter section can't be found in the current buffer, the
function returns nil, otherwise PROPERTY is set to VALUE and the
function returns t."
  (when (blogmore--frontmatter-bounds)
    (save-excursion
      (if-let ((location (blogmore--locate-frontmatter property)))
          (progn
            (goto-char (blogmore--frontmatter-property-location-start location))
            (kill-region (point) (line-end-position))
            (insert (format " %s" value)))
        (beginning-of-line)
        (insert (format "%s: %s\n" property value)))
      t)))

(defun blogmore-remove-frontmatter (property)
  "Remove PROPERTY from the frontmatter.

If a frontmatter section can't be found in the current buffer, the
function returns nil, otherwise PROPERTY is removed and the function
returns t. In the event that PROPERTY is not found, the function returns
t and the buffer is left unchanged."
  (when (blogmore--frontmatter-bounds)
    (save-excursion
      (when-let ((location (blogmore--locate-frontmatter property)))
        (goto-char (blogmore--frontmatter-property-location-start location))
        (delete-region (line-beginning-position) (line-end-position))
        (kill-line)))
    t))

(defun blogmore-toggle-frontmatter (property)
  "Toggle the existence of boolean PROPERTY in the frontmatter.

If the property doesn't exist, it is added with a value of true. If it
does exist and if its value is true, it is removed. If it does exist and
if its value is not true, its value is set to true.

If a frontmatter section can't be found in the current buffer, the
function returns nil, otherwise the property is toggled and the function
returns t."
  (if (and
       (blogmore--frontmatter-p property)
       (string-equal-ignore-case (blogmore-get-frontmatter property) "true"))
      (blogmore-remove-frontmatter property)
    (blogmore-set-frontmatter property "true")))


;; Configuration:

(defgroup blogmore ()
  "Tools for working with my personal blog."
  :group 'convenience
  :prefix "blogmore-"
  :link
  '(url-link
    :tag
    "blogmore.el on GitHub"
    "https://github.com/davep/blogmore.el")
  :link
  '(url-link
    :tag
    "BlogMore static site generator"
    "https://blogmore.davep.dev/"))

(defcustom blogmore-blogs nil
  "A list of blogs to work with."
  :type `(repeat (object :class blogmore-blog :value ,(blogmore-blog)))
  :group 'blogmore)

(defcustom blogmore-new-post-hook nil
  "Hook run after creating a new blog post."
  :type 'hook
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

(defcustom blogmore-default-post-maker-function
  (lambda (file)
    (format
     "%s/%s"
     (format-time-string
      "%Y/%m/%d"
      (blogmore-with-post file
        (parse-iso8601-time-string
         (blogmore-clean-time-string (blogmore-get-frontmatter "date")))))
     (replace-regexp-in-string
      (rx bol (= 4 digit) "-" (= 2 digit) "-" (= 2 digit) "-")
      ""
      (file-name-base file))))
  "Default function to generate a link for a blog post from its filename."
  :type 'function
  :group 'blogmore)

(defcustom blogmore-default-post-subdirectory-function
  (lambda ()
    (format-time-string "%Y/%m/%d/"))
  "Default function to generate the subdirectory for a blog post.

This is combined with the posts directory to generate the full path for
a new blog post."
  :type 'function
  :group 'blogmore)

(defcustom blogmore-default-post-file-name-from-title-function
  (lambda (title)
    (format "%s-%s.md"
            (format-time-string "%Y-%m-%d")
            (blogmore-slug title)))
  "Default function to generate a filename for a new blog post from its title."
  :type 'function
  :group 'blogmore)

(defcustom blogmore-default-category-maker-function #'blogmore-slug
  "Default function to generate a slug for a category."
  :type 'function
  :group 'blogmore)

(defcustom blogmore-default-tag-maker-function #'blogmore-slug
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


;; Command support code:

(defvar blogmore--current-blog nil
  "The current blog being worked on.")

(defmacro blogmore--within-post (&rest body)
  "Execute BODY within a blog post, or signal an error if we're not in a blog post."
  `(if (blogmore--blog-post-p)
       (progn ,@body)
     (user-error "This doesn't look like a blog post")))

(defun blogmore--chosen-blog-sans-error ()
  "Get the details of the currently-chosen blog, or nil.

This function is similar to `blogmore--chosen-blog', but returns nil
instead of signalling an error if no blog is chosen or if there are
multiple blogs defined."
  (cond (blogmore--current-blog
         ;; The user has chosen a blog, so use that.
         blogmore--current-blog)
        ((= (length blogmore-blogs) 1)
         ;; There's only one blog defined, so use that.
         (car blogmore-blogs))))

(defun blogmore--chosen-blog ()
  "Get the details of the currently-chosen blog.

If the current blog can't be determined, signal an error asking the user
to select a blog to work on first."
  (if-let ((blog (blogmore--chosen-blog-sans-error)))
      blog
    (if blogmore-blogs
        (user-error "Please select a blog to work on first; see `blogmore-work-on'")
      (user-error "No blogs defined; please add one to `blogmore-blogs'"))))

(defmacro blogmore--setting (setting)
  "Generate a function to get the value of SETTING for the current blog."
  `(defun ,(intern (format "blogmore--%s" setting)) ()
     ,(format "Get the %s for the current blog." setting)
     (with-slots (,setting) (blogmore--chosen-blog)
       (or ,setting
           (if (boundp ',(intern (format "blogmore-default-%s" setting)))
               ,(intern (format "blogmore-default-%s" setting))
             (user-error "No value for %s and no default" ,setting))))))

(blogmore--setting blog-title)
(blogmore--setting posts-directory)
(blogmore--setting post-template)
(blogmore--setting post-subdirectory-function)
(blogmore--setting post-file-name-from-title-function)
(blogmore--setting post-maker-function)
(blogmore--setting category-maker-function)
(blogmore--setting tag-maker-function)
(blogmore--setting post-link-format)
(blogmore--setting category-link-format)
(blogmore--setting tag-link-format)

(defun blogmore--now ()
  "Return the current date and time as a string."
  (format-time-string "%Y-%m-%d %H:%M:%S%z"))

(defun blogmore--blog-post-p ()
  "Return non-nil if a blog is selected we are visiting a blog post."
  (and (blogmore--chosen-blog-sans-error) (blogmore--frontmatter-bounds)))

(defun blogmore--post-directory ()
  "Get the full directory for a new blog post file."
  (format
   "%s%s"
   (file-name-as-directory (blogmore--posts-directory))
   (file-name-as-directory (funcall (blogmore--post-subdirectory-function)))))

(defun blogmore--ensure-directory ()
  "Ensure that the given directory exists."
  (let ((post-directory (blogmore--post-directory)))
    (unless (file-exists-p post-directory)
      (make-directory post-directory t))))

(defun blogmore--file-from-title (title)
  "Generate a filename for a blog post from the given TITLE."
  (format
   "%s%s"
   (file-name-as-directory (blogmore--post-directory))
   (funcall (blogmore--post-file-name-from-title-function) title)))

(defun blogmore--get-all (property)
  "Get a list of all values for PROPERTY from existing posts."
  (seq-uniq
   (split-string
    (shell-command-to-string
     (format
      (if (executable-find "rg")
          "rg --no-filename --no-line-number --no-heading \"^%1$s:\" \"%2$s\" -g \"*.md\""
        "find \"%2$s\" -type f -name \"*.md\" -exec grep -hi \"^%1$s:\" /dev/null {} +")
      property (expand-file-name (blogmore--posts-directory)))) "\n" t)
   #'string-equal-ignore-case))

(defun blogmore--current-categories ()
  "Get a list of categories from existing posts."
  (sort
   (delq
    nil
    (mapcar
     (lambda (candidate)
       (when (string-match (rx bol "category:" (* space) (group (* any)) eol) candidate)
         (string-trim (match-string 1 candidate))))
     (blogmore--get-all "category")))
   #'string-lessp))

(defun blogmore--current-tags ()
  "Get a list of tags from existing posts."
  (seq-uniq
   ;; Sorting *before* making unique because I want to favour upper-case
   ;; over lower-case in the resulting set.
   (sort
    (flatten-list
     (mapcar
      (lambda (candidate)
        (when (string-match (rx bol "tags:" (* space) (group (* any)) eol) candidate)
          (split-string (match-string 1 candidate) "," t " ")))
      (blogmore--get-all "tags")))
    #'string-lessp)
   #'string-equal-ignore-case))

(defun blogmore--post-picker ()
  "Pick a post from the list of existing posts."
  (list
   (completing-read
    (format "Post from %s: " (blogmore--blog-title))
    (directory-files-recursively (blogmore--posts-directory) (rx ".md" eol)))))

(defun blogmore--insert-link (link)
  "Insert Markdown to link to LINK."
  (save-excursion
    (insert (format "[](%s)" link)))
  (forward-char))

(defun blogmore--with (prompt existing-values)
  "Prompt the user with PROMPT and offer EXISTING-VALUES as completions."
  (blogmore--within-post
   (list
    (completing-read
     (format "%s from %s: " prompt (blogmore--blog-title))
     existing-values))))

(defsubst blogmore--image-extension (image)
  "Get the extension of IMAGE, or nil if it doesn't have one."
  (when-let ((extension (downcase (or (file-name-extension image) ""))))
    (if (string= extension "jpg")
        "jpeg"
      extension)))

(defconst blogmore--image-type-options '("jpeg" "png" "gif" "webp")
  "The options for image types that can be cycled through.")

(defun blogmore--cycle-image-type (current)
  "Given image extension CURRENT return the next option."
  (if-let ((next (cadr (member (blogmore--image-extension current) blogmore--image-type-options))))
      next
    (car blogmore--image-type-options)))


;; Commands:

;;;###autoload
(defun blogmore-work-on (blog)
  "Set the current blog to BLOG."
  (interactive (list (completing-read "Blog: " (mapcar #'blogmore-blog-title blogmore-blogs) nil t)))
  (setq blogmore--current-blog
        (seq-find
         (lambda (candidate)
           (string= (blogmore-blog-title candidate) blog))
         blogmore-blogs))
  (message "Now working on %s" blog)
  (when transient-current-prefix
    (call-interactively #'blogmore)))

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
      (run-hooks 'blogmore-new-post-hook))
    (basic-save-buffer)))

;;;###autoload
(defun blogmore-edit (file)
  "Edit FILE from my blog."
  (interactive (blogmore--post-picker))
  (find-file file))

;;;###autoload
(defun blogmore-toggle-draft ()
  "Toggle the draft status of the post."
  (interactive)
  (blogmore--within-post
   (blogmore-toggle-frontmatter "draft")))

;;;###autoload
(defun blogmore-set-category (category)
  "Set the category of the post to CATEGORY."
  (interactive (blogmore--with "Category" (blogmore--current-categories)))
  (blogmore-set-frontmatter "category" category))

(defun blogmore--post-tags ()
  "Get the tags for the current post as a list."
  (when-let ((tags (blogmore-get-frontmatter "tags")))
    (string-split tags "," t " ")))

;;;###autoload
(defun blogmore-add-tag (tag)
  "Add TAG to the post's tags."
  (interactive (blogmore--with "Tag" (blogmore--current-tags)))
  (blogmore-set-frontmatter
   "tags"
   (string-join
    (seq-uniq
     ;; Sorting *before* making unique because I want to favour upper-case
     ;; over lower-case in the resulting list of tags.
     (sort (append (blogmore--post-tags) (list tag)) #'string-lessp)
     #'string-equal-ignore-case)
    ", "))
  (message "Added tag '%s'" tag)
  (when transient-current-prefix
    (call-interactively #'blogmore-add-tag)))

;;;###autoload
(defun blogmore-remove-tag (tag)
  "Remove TAG from the post's tags."
  (interactive
   (list (when-let (tags (blogmore--post-tags))
           (completing-read "Tag to remove: " tags nil t))))
  (when tag
    (blogmore-set-frontmatter "tags" (string-join (remove tag (blogmore--post-tags)) ", "))
    (message "Removed tag '%s'" tag)
    (when transient-current-prefix
      (call-interactively #'blogmore-remove-tag))))

;;;###autoload
(defun blogmore-update-date ()
  "Update the date of the post to the current date and time."
  (interactive)
  (blogmore-set-frontmatter "date" (blogmore--now)))

;;;###autoload
(defun blogmore-update-modified ()
  "Update the modified date of the post to the current date and time."
  (interactive)
  (blogmore-set-frontmatter "modified" (blogmore--now)))

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

(defun blogmore-cycle-image-at-point ()
  "Cycle the type of the image at `point'."
  (interactive)
  (let ((line (buffer-substring-no-properties
               (line-beginning-position)
               (line-end-position))))
    (if-let* ((image-data
               (string-match
                (rx
                 (group
                  "!["
                  (minimal-match (zero-or-more anything))
                  "]")
                 "("
                 (group (one-or-more (not (any "#" ")"))))
                 (group (zero-or-more (not (any ")"))))
                 ")")
                line))
              (caption (match-string 1 line))
              (filename (match-string 2 line))
              (anchor (match-string 3 line)))
        (save-excursion
          (delete-region (line-beginning-position) (line-end-position))
          (insert
           (format "%s(%s.%s%s)"
                   caption
                   (file-name-sans-extension filename)
                   (blogmore--cycle-image-type filename)
                   anchor)))
      (user-error "No image found at point"))))

;;;###autoload
(transient-define-prefix blogmore ()
  "Show a transient for BlogMore commands."
  [:description
   (lambda ()
     (format "BlogMore: %s\n"
             (if (blogmore--chosen-blog-sans-error)
                 (blogmore--blog-title)
               "No blog selected")))
   ["Blog"
    ("b"  "Select blog" blogmore-work-on)]
   ["Post"
    ("n" "New post" blogmore-new :inapt-if-not blogmore--chosen-blog-sans-error)
    ("e" "Edit post" blogmore-edit :inapt-if-not blogmore--chosen-blog-sans-error)
    ("d" "Toggle draft status" blogmore-toggle-draft :inapt-if-not blogmore--blog-post-p)
    ("c" "Set post category" blogmore-set-category :inapt-if-not blogmore--blog-post-p)
    ("t" "Add tag" blogmore-add-tag :inapt-if-not blogmore--blog-post-p)
    ("T" "Remove tag" blogmore-remove-tag :inapt-if-not blogmore--blog-post-p)
    ("u d" "Update date" blogmore-update-date :inapt-if-not blogmore--blog-post-p)
    ("u m" "Update modified date" blogmore-update-modified :inapt-if-not blogmore--blog-post-p)]
   ["Links"
    ("l c" "Link to a category" blogmore-link-category :inapt-if-not blogmore--blog-post-p)
    ("l p" "Link to a post" blogmore-link-post :inapt-if-not blogmore--blog-post-p)
    ("l t" "Link to a tag" blogmore-link-tag :inapt-if-not blogmore--blog-post-p)
    ""
    "Other"
    ("i" "Cycle image type at point" blogmore-cycle-image-at-point :inapt-if-not blogmore--blog-post-p)]])

(provide 'blogmore)

;;; blogmore.el ends here
