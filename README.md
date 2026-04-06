# blogmore.el

## Introduction

`blogmore.el` is a small Emacs package that provides tools that help when
I'm writing blog posts for my personal blogs (such as my [main
blog](https://blog.davep.org/) or [my
photoblog](https://seen-by.davep.dev/)). It is written as a companion for
[BlogMore](https://blogmore.davep.dev/), the static site generator I use to
create my blogs.

It's designed to work out of the box with any blog that has a similar
structure to mine, but also designed with plenty of customisation options to
allow it to be used with a wide variety of different setups.

## Installation

`blogmore.el` is probably best installed with `use-package`, for example:


```elisp
(use-package blogmore
  :ensure t
  :vc (:url "https://github.com/davep/blogmore.el" :rev :newest))
```

will install the latest version. It's probably best to configure it using
`use-package` too. Here's what my full `use-package` declaration looks like
as of the time of writing:

```elisp
(use-package blogmore
  :ensure t
  :defer t
  :vc (:url "https://github.com/davep/blogmore.el" :rev :newest)
  :init
  ;; Add an end-of-file marker to any new post.
  (add-hook 'blogmore-new-post-hook #'end-it)
  ;; Always start out working on my personal blog.
  (blogmore-work-on "blog.davep.org")
  ;; Add some useful abbrevs for inserting commonly-used links into my blog
  ;; posts.
  (define-abbrev-table 'markdown-mode-abbrev-table
    '(("bm" "[BlogMore](https://blogmore.davep.dev/)")
      ("bme" "[`blogmore.el`](https://github.com/davep/blogmore.el)")
      ("pblog" "[photoblog](https://seen-by.davep.dev/)")))
  :custom
  (blogmore-blogs
   (list
    (blogmore-blog
     :title "blog.davep.org"
     :posts-directory "~/write/davep.github.com/content/posts/"
     :post-subdirectory-function (lambda () (format-time-string "%Y/%m/")))
    (blogmore-blog
     :title "seen-by.davep.dev"
     :posts-directory "~/write/seen-by/content/posts/")))
  :bind
  ("<f12> b" . blogmore))
```

and you can always see the latest approach I'm using [in my Emacs
configuration](https://github.com/davep/.emacs.d/blob/main/init.d/packages.d/personal/blogmore.el).

## Assumptions

While, as I say above, I've tried to make it as configurable as possible,
there are still some assumptions made and if your blog or posts don't match
these then `blogmore.el` might not work so well for you.

### Tags

It is assumed that your tags are always given as a comma-separated list like
this:

```yaml
tags: Emacs, Lisp, Emacs Lisp, coding
```

rather than as an actual *list*:

```yaml
tags: [Emacs, Lisp, Emacs Lisp, coding]
```

or this kid of actual list:

```yaml
tags:
  - Emacs
  - Lisp
  - Emacs Lisp
  - coding
```

The frontmatter modification code in `blogmore.el` is pretty naive and
*isn't* a proper frontmatter YAML parser. Perhaps at some point in the
future, but not right now.

### Post dates

It is assumed that your post dates will be something akin to ISO8601 format.
So ideally something like:

```yaml
date: 2026-04-05 10:11:12+0100
```

[//]: # (README.md ends here)
