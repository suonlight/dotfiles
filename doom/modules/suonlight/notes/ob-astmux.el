;; Define the ob-astmux language
(org-babel-do-load-languages
 'org-babel-load-languages
 '((astmux . t)))

(defconst astmux-server-path
  (expand-file-name "astmux_server.py" (file-name-directory load-file-name))
  "Path to the astmux server script.")

(defun org-babel-execute:astmux (body params)
  "Execute the astmux commands specified in BODY using EPC."
  (let* ((session (cdr (assoc :session params)))
          (lang (cdr (assoc :lang params)))
          (connection (epc:start-epc "python3" `(,astmux-server-path)))
          (result (epc:call-sync connection 'execute_astmux `(,session ,lang ,body))))
    (epc:stop-epc connection)
    result))
