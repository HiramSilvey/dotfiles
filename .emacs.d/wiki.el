(defun hs/wikipedia-summary-buffer ()
  "Return the \"*Wikipedia summary*\" buffer.
If it does not exist, create it and switch it to `special-mode'."
  (or (get-buffer "*Wikipedia summary*")
      (with-current-buffer (get-buffer-create "*Wikipedia summary*")
        (special-mode)
        (current-buffer))))

(defun hs/wiki-summary (input)
  (interactive "sSearch Wikipedia: ")
  (plz 'get (concat "https://en.wikipedia.org/w/api.php?format=json&action=query&prop=extracts&exintro&explaintext&redirects=1&titles=" (url-hexify-string input))
    :as #'json-read
    :then (lambda (response)
            (pop-to-buffer (wikipedia-summary-buffer))
            (let ((inhibit-read-only t))
              (erase-buffer)
              (insert
               (format "%s"
                       (alist-get 'extract
                                  (car (last
                                        (car (last
                                              (car (last response)))))))))))))
