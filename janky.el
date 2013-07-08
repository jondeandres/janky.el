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

(defun janky-extra-headers ()
  `(("Authorization" . , (concat "Basic "
                                 (base64-encode-string
                                  (concat janky-auth-user
                                          ":"
                                          janky-auth-password))))))

(defun* janky-request (url callback &key (format :json) (method "GET"))
  "Makes a request to URL and returns a parsed json object"
  (let ((url-request-method method)
        (json-object-type 'plist)
        (url-request-extra-headers (janky-extra-headers))
        (data)
        (raw-data)
        (status))
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char (point-min))
      (re-search-forward "^$")
      (setq status url-http-response-status)
      (setq raw-data (buffer-substring (point) (point-max)))
      (setq data (if (equal format :json)
                     (json-read-from-string raw-data)
                   raw-data))
      (if callback
          (funcall callback status data)
        data))))

(defun janky-repo-branch-path (repo branch)
  (concat janky-base-url repo "/" branch))

(defun janky-ci-build (repo branch)
  (janky-request (janky-repo-branch-path repo branch)
                 nil
                 :format :raw :method "POST"))

(defun janky-ci-status (repo branch)
  (janky-request (janky-repo-branch-path repo branch)
                        nil))

(provide 'janky)
