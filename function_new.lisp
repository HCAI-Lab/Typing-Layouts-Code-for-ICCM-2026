(defvar *load-pathname-directory* (pathname-directory *load-pathname*))
(setf *load-pathname-directory* (rest *load-pathname-directory*))
(defvar *load-pathname-directory-string* (format nil "~{~a/~}" *load-pathname-directory*))
(defvar *python-address* (format nil "C:/~aVisiTor.py" *load-pathname-directory-string*))
(defvar *directory* (format nil "C:/~aImages_v3" *load-pathname-directory-string*))
(defvar *clicking-occurred* nil)
;; Python interpreter used to run VisiTor.py. Set this to the full path of
;; your interpreter (e.g. an Anaconda python.exe) if "python" is not on PATH.
(defvar *python-executable* "python")
(ql:quickload 'inferior-shell)
(defvar *clicking-occurred* nil)

(
    defun run-shell-command (command &optional arglist)
  (let ((shell-request-list
         (append (list *python-executable*) (list *python-address*)
                 (list command) '("--Dir") (list *directory*) '("--arg2") arglist)))
    (print shell-request-list)
    (inferior-shell:run/ss shell-request-list))
)

(
    defun what-is-on-screen (l)
  (let ((on-screen (run-shell-command "whatisonscreen" l)))
 ;   (act-r-output "what is on screen function ~A!!!" on-screen)
    on-screen)
)
(
    defun end-press ()
    (run-shell-command "endkeypress" '("temp"))
    )
(
    defun set-pattern (p)
  (setf *pattern-global* p)
 ; (act-r-output " Now we have ~s !!!!" *pattern-global*)
)
(defun move-attention-output-creator (x)
  (format nil "moving attention to ~A" x))

(
    defun respond-to-move-cursor ( z)
 ; (act-r-output "move-cursor to ~A!!!" z)
  (let ((result-from-java (run-shell-command "movecursortopattern" z)))
    result-from-java)
 )

(
    defun respond-to-where-is ( z)
 ; (act-r-output "move-cursor to ~A!!!" z)
  (let ((result-from-java (run-shell-command "whereis" z)))
    result-from-java)
 )

;(defun respond-to-finger (k)
 ; (run-shell-command "peck" k))

 ; (when (equal k '("enter"))
 ;   (update-model-click)))

(defun respond-to-key-press (k)
  (run-shell-command "Keypress" k)
  (when (equal k '("enter"))
    (update-model-click)))
(defun check-string (x)
  (if (and (char-alpha-p (char x 0))
           (every #'digit-char-p (subseq x 1)))
      "cell"
      "text"))

;
;(defun type-check (key-string-temp)
;  (setf key-string (string-trim "#\\" key-string))
;  (let ((command "typecheck"))
;    (cond
;     ((string-equal key-string "*") (setf key-string "*"))
;     ((string-equal key-string "q") (setf key-string ":"))
;     ((string-equal key-string "]") (setf key-string ")"))
;     ((string-equal key-string "[") (setf key-string "(")))
;    (let ((on-screen (run-shell-command command key-string)))
;      (model-output on-screen)
;      on-screen)))
(
    defun type-check ()
  "true"
)
(
    defun respond-to-click-mouse (x y z)
  (run-shell-command "click" '("temp"))
    ;;  (sleep 0.1)
;(update-model-click)  ; Set the variable when clicking occurs

)

(
    defun get-key (l sublist)
  "Finds a key given the pair of values associated with it"
  (car (find-if (lambda (parts) (equal sublist (cadr parts))) l))
)
(
    defun concatenate-strings (num1 )
  (concatenate 'string
                "task"
               (write-to-string num1))
)
(
    defun concatenate-strings-subtask (num1 num2)
  (concatenate 'string
               "subtask"
               (write-to-string num1)
               "_"
               (write-to-string num2)
               "_list")
)
(
    defun get-element-from-list (num1 num2 which)
  (let ((desired_string (concatenate-strings-subtask num1 num2)))
    (case which
      (1 (car (read-from-string (concat "(" desired_string ")"))))
      (2 (cadr (read-from-string (concat "(" desired_string ")"))))
      (t nil)))
)

(
    defun get-chars-after-sequence-ci (char1 char2 char3 string)
  (let* ((seq (concatenate 'string char1 char2 char3))
         (pos (search seq string :test #'string-equal)))
    (if pos
        (let ((len (length string)))
          (list (if (< (+ pos (length seq)) len) (string (char string (+ pos (length seq)))) nil)
                (if (< (+ pos (length seq) 1) len) (string (char string (+ pos (length seq) 1))) nil)
                (if (< (+ pos (length seq) 2) len) (string (char string (+ pos (length seq) 2))) nil)))
        nil))
)
(
    defun replace-at-positions (lst vi txt)
  (let ((counter 0)) ; initialize counter to traverse the list
    (mapcar (lambda (x)
              (incf counter) ; increment the counter for each item
              (cond
               ((= counter 15) vi) ; 15th item in the list
               ((= counter 19) txt) ; 19th item in the list
               (t x))) ; for all other items, keep the original value
            lst))
)
;(defun read-json-file (filename)
;  "Reads a JSON file and converts it to a Lisp data structure."
;  ;; Implement JSON reading functionality here.
;  ;; This can be straightforward or complex based on the Lisp system and libraries you have.
;)
;
;(defun create-agi-elements (elements)
;  "Create AGI elements based on the provided list."
;  (loop for elem in elements do
;    (let ((name (getf elem 'name))
;          (position (getf elem 'position))
;          (color (getf elem 'color))
;          (text (getf elem 'text)))
;      ;; Use the appropriate AGI function based on the element type.
;      ;; This is an example for text elements.
;      (add-text-to-exp-window *my-agi-window* text :x (first position) :y (second position) :color color))))
;
;;; Example usage
;(defvar *elements* (read-json-file "visicons.json"))
;(create-agi-elements *elements*)
(defun clicking-occurred? (x)
  *clicking-occurred*)
