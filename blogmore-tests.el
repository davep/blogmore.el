(require 'ert)
(require 'cl-lib)
(require 'blogmore)

(defconst blogmore--test-titles
  '(("" . "")
    ("Hello World!" . "hello-world")
    ("Emacs & Lisp" . "emacs-lisp")
    ("2026: A Blog Odyssey" . "2026-a-blog-odyssey")))

(ert-deftest blogmore--slug-test ()
  "Test that blogmore--slug produces expected slugs."
  (dolist (case blogmore--test-titles)
    (should (equal (blogmore--slug (car case)) (cdr case)))))

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
     (should (null (blogmore--frontmatter-bounds)))))

(ert-deftest blogmore--locate-frontmatter-test ()
   "Test locating frontmatter properties."
   (with-temp-buffer
     (insert "---\ntitle: Test Title\ndate: 2026-04-02\n---\n\nContent")
     (let ((result (blogmore--locate-frontmatter "title")))
       (should (consp result))
       (should (equal (string-trim (buffer-substring (nth 0 result) (nth 1 result))) (nth 2 result)))
       (should (equal (nth 2 result) "Test Title")))
     (should (null (blogmore--locate-frontmatter "category")))))

(ert-deftest blogmore--frontmatter-p-test ()
   "Test detection of frontmatter presence."
   (with-temp-buffer
     (insert "---\ntitle: Test\n---\n\nContent")
     (goto-char (point-min))
     (should (blogmore--frontmatter-p "title"))
     (should (not (blogmore--frontmatter-p "category"))))
   (with-temp-buffer
     (insert "No frontmatter here")
     (goto-char (point-min))
     (should-not (blogmore--frontmatter-p "title"))))

(ert-deftest blogmore--get-frontmatter-property-test ()
   "Test retrieval of frontmatter values."
   (with-temp-buffer
     (insert "---\ntitle: Test Title\ndate: 2026-04-02\n---\n\nContent")
     (goto-char (point-min))
     (should (equal (blogmore--get-frontmatter-property "title") "Test Title"))
     (should-not (blogmore--get-frontmatter-property "category")))
   (with-temp-buffer
     (insert "No frontmatter here")
     (goto-char (point-min))
     (should-not (blogmore--get-frontmatter-property "title"))))

(ert-deftest blogmore--set-frontmatter-property-test ()
   "Test setting frontmatter properties."
   (let ((blogmore--current-blog (make-blogmore--blog :posts-directory "/tmp/")))
     (with-temp-buffer
       (insert "---\ntitle: Old Title\n---\n\nContent")
       (goto-char (point-min))
       (blogmore--set-frontmatter-property "title" "New Title")
       (should (equal (blogmore--get-frontmatter-property "title") "New Title"))
       (blogmore--set-frontmatter-property "category" "Tech")
       (should (equal (blogmore--get-frontmatter-property "category") "Tech")))
     (with-temp-buffer
       (insert "No frontmatter here")
       (goto-char (point-min))
       (should-error (blogmore--set-frontmatter-property "title" "New Title") :type 'user-error))))

(ert-deftest blogmore--post-p-test ()
  "Test detection of blog post frontmatter."
  (with-temp-buffer
    (insert "---\ntitle: Test\n---\n\nContent")
    (goto-char (point-min))
    (should (blogmore--post-p)))
  (with-temp-buffer
    (insert "No frontmatter here")
    (goto-char (point-min))
    (should-not (blogmore--post-p))))

(ert-deftest blogmore--blog-post-p-test ()
  "Test blogmore--blog-post-p returns correct values."
  (let ((blogmore--current-blog (make-blogmore--blog :posts-directory "/tmp/")))
    (with-temp-buffer
      (insert "---\ntitle: Test\n---\n\nContent")
      (goto-char (point-min))
      (should (blogmore--blog-post-p))))
  (let ((blogmore--current-blog nil))
    (with-temp-buffer
      (insert "---\ntitle: Test\n---\n\nContent")
      (goto-char (point-min))
      (should-not (blogmore--blog-post-p))))
  (let ((blogmore--current-blog (make-blogmore--blog :posts-directory "/tmp/")))
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
  (let ((blogmore--current-blog (make-blogmore--blog :posts-directory "/tmp/")))
    (with-temp-buffer
      (insert "---\ntitle: Test\n---\n\nContent")
      (should (blogmore--within-post t)))
    (with-temp-buffer
      (insert "No frontmatter here")
      (should-error (blogmore--within-post t) :type 'user-error))))

(ert-deftest blogmore--file-from-title-test ()
  "Test filename generation from post titles."
  (let ((blogmore--current-blog (make-blogmore--blog :posts-directory "/tmp/"))
        (post-dir (format-time-string "%Y/%m/%d"))
        (today (format-time-string "%Y-%m-%d")))
    (dolist (case blogmore--test-titles)
      (should (string-match
               (format "/tmp/%s/%s-%s.md"
                       post-dir
                       today
                       (cdr case))
               (blogmore--file-from-title (car case)))))))

(ert-deftest blogmore--current-categories-test ()
  "Test extraction of categories from post content."
  (cl-letf (((symbol-function 'blogmore--get-all)
             (lambda (_)
               '("category: Z"
                 "category: Emacs"
                 "category: Lisp"
                 "category: Life"
                 "category: A"))))
    (should (equal (blogmore--current-categories) '("A" "Emacs" "Life" "Lisp" "Z")))))

(ert-deftest blogmore--current-tags-test ()
  "Test extraction of tags from post content."
  (cl-letf (((symbol-function 'blogmore--get-all)
             (lambda (_)
               '("tags: Emacs, Lisp"
                 "tags: Emacs, Org"
                 "tags: Lisp, Emacs"
                 "tags: Z, A"))))
    (should (equal (sort (blogmore--current-tags) #'string-lessp)
                   '("A" "Emacs" "Lisp" "Org" "Z")))))

;;; blogmore-tests.el ends here
