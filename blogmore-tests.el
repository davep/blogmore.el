(require 'ert)
(require 'cl-lib)
(require 'blogmore)

(defconst blogmore--test-titles
  '(("" . "")
    ("Hello World!" . "hello-world")
    ("Emacs & Lisp" . "emacs-lisp")
    ("That's a nice defun" . "thats-a-nice-defun")
    ("Café Ëmacs" . "cafe-emacs")
    ("2026: A Blog Odyssey" . "2026-a-blog-odyssey")))

(ert-deftest blogmore-slug-test ()
  "Test that blogmore-slug produces expected slugs."
  (dolist (case blogmore--test-titles)
    (should (equal (blogmore-slug (car case)) (cdr case)))))

(ert-deftest blogmore--cycle-image-type-test ()
  "Test that blogmore--cycle-image-type cycles through image types correctly."
  (should (equal (blogmore--cycle-image-type "foo.jpg") "png"))
  (should (equal (blogmore--cycle-image-type "foo.jpeg") "png"))
  (should (equal (blogmore--cycle-image-type "foo.png") "gif"))
  (should (equal (blogmore--cycle-image-type "foo.gif") "webp"))
  (should (equal (blogmore--cycle-image-type "foo.webp") "jpeg"))
  (should (equal (blogmore--cycle-image-type "foo") "jpeg")))

(ert-deftest blogmore--frontmatter-bounds-test ()
   "Test detection of frontmatter bounds."
   (with-temp-buffer
     (insert "---\ntitle: Test\ndate: 2026-04-02\n---\nContent")
     (let ((bounds (blogmore--frontmatter-bounds)))
       (should (consp bounds))
       (goto-char (car bounds))
       (should (looking-at "Title"))
       (goto-char (cdr bounds))
       (should (looking-at "---"))))
   (with-temp-buffer
     (insert "No frontmatter here")
     (should-not (blogmore--frontmatter-bounds))))

(ert-deftest blogmore--locate-frontmatter-test ()
   "Test locating frontmatter properties."
   (with-temp-buffer
     (insert "---\ntitle: Test Title\ndate: 2026-04-02\n---\n\nContent")
     (let ((result (blogmore--locate-frontmatter "title")))
       (should (blogmore--frontmatter-property-location-p result))
       (should (equal
                (string-trim
                 (buffer-substring
                  (blogmore--frontmatter-property-location-start result)
                  (blogmore--frontmatter-property-location-end result)))
                (blogmore--frontmatter-property-location-value result)))
       (should (equal (blogmore--frontmatter-property-location-value result) "Test Title")))
     (should-not (blogmore--locate-frontmatter "category"))))

(ert-deftest blogmore--frontmatter-p-test ()
   "Test detection of frontmatter presence."
   (with-temp-buffer
     (insert "---\ntitle: Test\n---\n\nContent")
     (should (blogmore--frontmatter-p "title"))
     (should-not (blogmore--frontmatter-p "category")))
   (with-temp-buffer
     (insert "No frontmatter here")
     (should-not (blogmore--frontmatter-p "title"))))

(ert-deftest blogmore-get-frontmatter-test ()
   "Test retrieval of frontmatter values."
   (with-temp-buffer
     (insert "---\ntitle: Test Title\ndate: 2026-04-02\n---\n\nContent")
     (should (equal (blogmore-get-frontmatter "title") "Test Title"))
     (should-not (blogmore-get-frontmatter "category")))
   (with-temp-buffer
     (insert "No frontmatter here")
     (should-not (blogmore-get-frontmatter "title"))))

(ert-deftest blogmore-set-frontmatter-test ()
   "Test setting frontmatter properties."
   (let ((blogmore--current-blog (blogmore-blog :posts-directory "/tmp/")))
     (with-temp-buffer
       (insert "---\n---\n")
       (should (blogmore-set-frontmatter "title" "New Title"))
       (should (equal (blogmore-get-frontmatter "title") "New Title"))
       (should (blogmore-set-frontmatter "title" "Newer Title"))
       (should (equal (blogmore-get-frontmatter "title") "Newer Title"))
       (should (blogmore-set-frontmatter "category" "Tech"))
       (should (equal (blogmore-get-frontmatter "category") "Tech"))
       (should (blogmore-set-frontmatter "empty" ""))
       (should (equal (blogmore-get-frontmatter "empty") "")))
     (with-temp-buffer
       (insert "No frontmatter here")
       (should-not (blogmore-set-frontmatter "title" "New Title")))))

(ert-deftest blogmore-remove-frontmatter-test ()
   "Test removal of frontmatter properties."
   (let ((blogmore--current-blog (blogmore-blog :posts-directory "/tmp/")))
     (with-temp-buffer
       (insert "---\ntitle: Test\ncategory: Tech\n---\n\nContent")
       (should (blogmore-remove-frontmatter "title"))
       (should-not (blogmore-get-frontmatter "title"))
       (should (blogmore-get-frontmatter "category")))
     (with-temp-buffer
       (insert "No frontmatter here")
       (should-not (blogmore-remove-frontmatter "title")))))

(ert-deftest blogmore-toggle-frontmatter-test ()
   "Test toggling frontmatter properties."
   (let ((blogmore--current-blog (blogmore-blog :posts-directory "/tmp/")))
     (with-temp-buffer
       (insert "---\ntitle: Test\nother: false\n---\n\nContent")
       (should (blogmore-toggle-frontmatter "test"))
       (should (equal (blogmore-get-frontmatter "test") "true"))
       (should (blogmore-toggle-frontmatter "test"))
       (should-not (blogmore-get-frontmatter "test"))
       (should (equal (blogmore-get-frontmatter "other") "false"))
       (should (blogmore-toggle-frontmatter "other"))
       (should (equal (blogmore-get-frontmatter "other") "true")))
     (with-temp-buffer
       (insert "No frontmatter here")
       (should-not (blogmore-toggle-frontmatter "test")))))

(ert-deftest blogmore--blog-post-p-test ()
  "Test blogmore--blog-post-p returns correct values."
  (let ((blogmore--current-blog (blogmore-blog :posts-directory "/tmp/")))
    (with-temp-buffer
      (insert "---\ntitle: Test\n---\n\nContent")
      (goto-char (point-min))
      (should (blogmore--blog-post-p))))
  (let ((blogmore--current-blog nil))
    (with-temp-buffer
      (insert "---\ntitle: Test\n---\n\nContent")
      (goto-char (point-min))
      (should-not (blogmore--blog-post-p))))
  (let ((blogmore--current-blog (blogmore-blog :posts-directory "/tmp/")))
    (with-temp-buffer
      (insert "No frontmatter here")
      (goto-char (point-min))
      (should-not (blogmore--blog-post-p)))))

(ert-deftest blogmore--insert-link-test ()
   "Test blogmore--insert-link inserts a Markdown link and point on the closing ]."
   (with-temp-buffer
     (blogmore--insert-link "https://example.com")
     (should (equal (buffer-string) "[](https://example.com)"))
     (should (looking-at "]"))))

(ert-deftest blogmore--within-post-test ()
   "Test blogmore--within-post macro behavior."
  (let ((blogmore--current-blog (blogmore-blog :posts-directory "/tmp/")))
    (with-temp-buffer
      (insert "---\ntitle: Test\n---\n\nContent")
      (should (blogmore--within-post t)))
    (with-temp-buffer
      (insert "No frontmatter here")
      (should-error (blogmore--within-post t) :type 'user-error))))

(ert-deftest blogmore--file-from-title-test ()
  "Test filename generation from post titles."
  (let ((blogmore--current-blog (blogmore-blog :posts-directory "/tmp/")))
    ;; Default filename generation.
    (let ((post-dir (format-time-string "%Y/%m/%d"))
          (today (format-time-string "%Y-%m-%d")))
      (dolist (case blogmore--test-titles)
        (should (string-match
                 (format "/tmp/%s/%s-%s.md"
                         post-dir
                         today
                         (cdr case))
                 (blogmore--file-from-title (car case))))))
    ;; Custom default filename generation.
    (let ((blogmore-default-post-file-name-from-title-function
           (lambda (title)
             (format "%s.md" (blogmore-slug title))))
          (post-dir (format-time-string "%Y/%m/%d")))
      (dolist (case blogmore--test-titles)
        (should (string-match
                 (format "/tmp/%s/%s.md"
                         post-dir
                         (cdr case))
                 (blogmore--file-from-title (car case))))))))

(ert-deftest blogmore--current-categories-test ()
  "Test extraction of categories from post content."
  (cl-letf (((symbol-function 'blogmore--get-all)
             (lambda (_ &optional _)
               '("Z" "Emacs" "Lisp" "Life" "A"))))
    (should (equal (blogmore--current-categories) '("A" "Emacs" "Life" "Lisp" "Z")))))

(ert-deftest blogmore--current-tags-test ()
  "Test extraction of tags from post content."
  (cl-letf (((symbol-function 'blogmore--get-all)
             (lambda (_ &optional _)
               '("Emacs" "Lisp" "Emacs" "Org" "Lisp" "Emacs" "Z" "A"))))
    (should (equal (sort (blogmore--current-tags) #'string-lessp)
                   '("A" "Emacs" "Lisp" "Org" "Z")))))

(ert-deftest blogmore--chosen-blog-sans-error-test ()
  "Test that blogmore--chosen-blog-sans-error smartly defaults."
  ;; With no blog selected by the user.
  (let ((blogmore--current-blog))
    ;; No blogs defined.
    (let ((blogmore-blogs))
      (should-not (blogmore--chosen-blog-sans-error)))
    ;; One blog defined.
    (let ((blogmore-blogs (list (blogmore-blog :title "Test Blog"))))
      (should (equal (blogmore--chosen-blog-sans-error) (car blogmore-blogs))))
    ;; Multiple blogs defined.
    (let ((blogmore-blogs (list (blogmore-blog :title "Test Blog 1")
                                (blogmore-blog :title "Test Blog 2"))))
      (should-not (blogmore--chosen-blog-sans-error))
      ;; Select the first blog and try again.
      (let ((blogmore--current-blog (car blogmore-blogs)))
        (should (equal (blogmore--chosen-blog-sans-error) (car blogmore-blogs)))))))

(ert-deftest blogmore--chosen-blog-test ()
  "Test that blogmore--chosen-blog smartly defaults."
  ;; With no blog selected by the user.
  (let ((blogmore--current-blog))
    ;; No blogs defined.
    (let ((blogmore-blogs))
      (should-error (blogmore--chosen-blog) :type 'user-error))
    ;; One blog defined.
    (let ((blogmore-blogs (list (blogmore-blog :title "Test Blog"))))
      (should (equal (blogmore--chosen-blog) (car blogmore-blogs))))
    ;; Multiple blogs defined.
    (let ((blogmore-blogs (list (blogmore-blog :title "Test Blog 1")
                                (blogmore-blog :title "Test Blog 2"))))
      (should-error (blogmore--chosen-blog) :type 'user-error)
      ;; Select the first blog and try again.
      (let ((blogmore--current-blog (car blogmore-blogs)))
        (should (equal (blogmore--chosen-blog) (car blogmore-blogs)))))))

(ert-deftest blogmore-add-tag-test ()
  "Test that blogmore-add-tag adds a tag to the current post."
  (let ((blogmore--current-blog (blogmore-blog :posts-directory "/tmp/")))
    (with-temp-buffer
      (insert "---\ntitle: Test\n---\n\nContent")
      (let ((inhibit-message t))
        (should-not (blogmore-get-frontmatter "tags"))
        (blogmore-add-tag "Emacs")
        (should (equal (blogmore-get-frontmatter "tags") "Emacs"))
        (blogmore-add-tag "Lisp")
        (should (equal (blogmore-get-frontmatter "tags") "Emacs, Lisp"))
        (blogmore-add-tag "Emacs")
        (should (equal (blogmore-get-frontmatter "tags") "Emacs, Lisp"))
        (blogmore-add-tag "A")
        (should (equal (blogmore-get-frontmatter "tags") "A, Emacs, Lisp"))))))

(ert-deftest blogmore-remove-tag-test ()
  "Test that blogmore-remove-tag removes a tag from the current post."
  (let ((blogmore--current-blog (blogmore-blog :posts-directory "/tmp/")))
    (with-temp-buffer
      (insert "---\ntitle: Test\ntags: A, Emacs, Lisp\n---\n\nContent")
      (let ((inhibit-message t))
        (should (equal (blogmore-get-frontmatter "tags") "A, Emacs, Lisp"))
        (blogmore-remove-tag "Emacs")
        (should (equal (blogmore-get-frontmatter "tags") "A, Lisp"))
        (blogmore-remove-tag "A")
        (should (equal (blogmore-get-frontmatter "tags") "Lisp"))
        (blogmore-remove-tag "Lisp")
        (should (equal (blogmore-get-frontmatter "tags") ""))))))

;;; blogmore-tests.el ends here
