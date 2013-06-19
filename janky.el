(require 'url)
(require 'json)

(defvar janky-url "")

(defun janky-request (url)
  "Makes a request to URL and returns a parsed json object"
  (with-current-buffer (url-retrieve-synchronously janky-url)
    (goto-char (point-min))
    (re-search-forward "^$")
    (let ((json-object-type 'plist))
      (json-read))))

