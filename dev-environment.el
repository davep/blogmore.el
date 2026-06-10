;; Isolated development environment.

(require 'package)

(setq package-user-dir (expand-file-name ".packages" default-directory)
      package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")))

(package-initialize)

(unless package-archive-contents
  (message "Refreshing package archives from ELPA...")
  (package-refresh-contents))

(defvar project-dependencies '(yaml)
  "List of packages required to build and test the project.")

(dolist (pkg project-dependencies)
  (unless (package-installed-p pkg)
    (message "Installing dependency: %s" pkg)
    (package-install pkg)))

(package-activate-all)

;;; dev-environment.el ends here
