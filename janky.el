(require 'url)
(require 'json)

(eval-when-compile (require 'cl))

(defcustom janky-auth-user ""
  "The user for auth basic authorization"
  :group 'janky
  :type 'string)

(defcustom janky-auth-password ""
  "The password for auth basic authorization"
  :group 'janky
  :type 'string)

(defcustom janky-base-url ""
  "The janky url"
  :group 'janky
  :type 'string)

(defun janky-base64-auth ()
  (base64-encode-string
   (concat janky-auth-user ":" janky-auth-password)))

(defun janky-extra-headers ()
  `(("Authorization" . ,(concat "Basic " (janky-base64-auth)))))

(defun* janky-request (url &key (format :json) (method "GET"))
  "Makes a request to URL and returns a parsed json object"
  (let ((url-request-method method)
        (json-object-type 'plist)
        (url-request-extra-headers (janky-extra-headers)))
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char (point-min))
      (re-search-forward "^$")
      (janky-parse-response (buffer-substring (point) (point-max))
                            format))))

(defun janky-parse-response (raw-data format)
  (if (equal format :json)
      (json-read-from-string raw-data)
    raw-data))

(defun janky-replace-json-false (data)
  (mapcar (lambda (item) (if (equal :json-false item) nil item)) data))

(defun janky-repo-branch-path (repo branch)
  (concat janky-base-url repo "/" branch))

(defun janky-read-repo-branch-args ()
  (let ((repo (read-string "Repository: "))
        (branch (read-string "Branch: ")))
    (list repo branch)))

(defun janky-build (repo branch)
  (interactive (janky-read-repo-branch-args))
  (janky-request (janky-repo-branch-path repo branch)
                 :format :raw
                 :method "POST"))

(defun janky-status (repo branch)
  (interactive (janky-read-repo-branch-args))
  (janky-request (janky-repo-branch-path repo branch)))

(defun janky-last-build (repo branch)
  (interactive (janky-read-repo-branch-args))
  (elt (janky-status repo branch) 0))

(defun janky-show-status (repo branch)
  (interactive (janky-read-repo-branch-args))
  (let ((status (plist-get (janky-last-build repo branch) :green)))
    (if (equal status t)
        (message "Build success!")
      (message "Build failed!"))))

(provide 'janky)
