(defpackage :shesh
  (:use :cl)
  (:export #:*current-shell*
           #:*stdout*
           #:init
           #:exec
           #:stdout
           #:close
           #:sh
           #:exec-timeout))

(in-package :shesh)

(defvar *current-shell* nil
  "The current shell process object managed by UIOP.")

(defvar *stdout* ""
  "Buffer to store the cumulative shell output.")

(defvar *shell-output-stream* nil
  "The output stream of the shell process.")

(defvar *shell-input-stream* nil
  "The input stream of the shell process.")

(defun init (&optional (shell-command "bash"))
  "Initialize a new shell process.
  
  Args:
    shell-command: The shell command to execute (default: 'bash')
  
  Returns:
    The shell process object
  
  Side effects:
    - Closes any existing shell process
    - Creates new shell process with interactive streams
    - Configures shell for predictable output
    - Clears stdout buffer"
  (when *current-shell*
    (close))

  ;; Launch the shell with interactive streams
  (setf *current-shell*
    (uiop:launch-program shell-command
      :input :stream
      :output :stream
      :error-output :stream))

  ;; Store the streams for easy access
  (setf *shell-input-stream* (uiop:process-info-input *current-shell*)
    *shell-output-stream* (uiop:process-info-output *current-shell*))

  ;; Clear stdout buffer
  (setf *stdout* "")

  ;; Set non-interactive mode for more predictable output
  (format *shell-input-stream* "set +o vi~%") ; Disable vi mode if set
  (format *shell-input-stream* "set +o emacs~%") ; Disable emacs mode if set
  (format *shell-input-stream* "PS1='$ '~%") ; Simple prompt
  (format *shell-input-stream* "set +o nocasematch~%") ; Ensure case sensitivity
  (force-output *shell-input-stream*)

  ;; Consume initial output
  (sleep 0.1)
  (stdout)

  *current-shell*)

;;; Function to consume stdout
(defun stdout ()
  "Consume available output from the shell and append to *stdout*"
  (when (and *current-shell* *shell-output-stream*)
        (loop while (listen *shell-output-stream*)
              do (let ((char (read-char-no-hang *shell-output-stream* nil nil)))
                   (when char
                         (setf *stdout* (concatenate 'string *stdout* (string char))))))))

;;; Main exec function
(defun exec (command)
  "Execute a command in the current shell"
  (unless *current-shell*
    (error "No shell initialized. Call (init-shell) first."))

  ;; Clear stdout buffer before executing
  (setf *stdout* "")

  ;; Generate a unique marker for this command
  (let ((marker (format nil "SHESH-END-~A" (get-universal-time))))
    ;; Send the command followed by echo of the marker
    (format *shell-input-stream* "~A ; echo ~A~%" command marker)
    (force-output *shell-input-stream*)

    ;; Read output until we see the marker
    (loop
   do (stdout)
     (sleep 0.01)
   until (and (> (length *stdout*) (length marker))
              (search marker *stdout*)))

    ;; Remove the marker and any trailing newlines from the output
    (let ((marker-pos (search marker *stdout*)))
      (when marker-pos
            (setf *stdout* (subseq *stdout* 0 marker-pos))
            ;; Remove trailing newlines
            (setf *stdout* (string-right-trim '(#\Newline #\Return) *stdout*)))))

  ;; Return the output
  *stdout*)

;;; Macro to write commands without quotes
(defmacro sh (&rest command-parts)
  "Execute a shell command without needing quotes.
   Examples:
   (sh pwd)
   (sh ls -la)
   (sh echo hello world)
   (sh cd ..)
   (let ((x 10)) (sh echo $x))  ; Will echo 10
   (sh echo $undefined)         ; Will echo $undefined
   
   The command parts are converted to strings and joined with spaces.
   Variables prefixed with $ are evaluated and substituted if bound."
  `(exec (format nil "~{~A~^ ~}"
           (list ,@(loop for part in command-parts
                         collect (if (and (symbolp part)
                                          (> (length (string part)) 1)
                                          (char= (char (string part) 0) #\$))
                                     ;; It's a variable reference like $x
                                     (let ((var-name (intern (subseq (string part) 1))))
                                       `(or (ignore-errors (princ-to-string ,var-name))
                                            ;; If error (unbound), keep original $variable
                                            (string-downcase (princ-to-string ',part))))
                                     ;; Regular part - use string-downcase to preserve lowercase
                                     `(string-downcase (princ-to-string ',part))))))))

;;; Close the shell
(defun close-shell ()
  "Close the current shell process"
  (when *current-shell*
        (ignore-errors
          (format *shell-input-stream* "exit~%")
          (force-output *shell-input-stream*))
        (uiop:terminate-process *current-shell*)
        (setf *current-shell* nil
          *shell-input-stream* nil
          *shell-output-stream* nil
          *stdout* "")))

;;; Alternative exec with timeout
(defun exec-with-timeout (command &optional (timeout 1.0))
  "Execute a command with a timeout for output collection"
  (unless *current-shell*
    (error "No shell initialized. Call (init-shell) first."))

  ;; Clear stdout buffer
  (setf *stdout* "")

  ;; Send command
  (format *shell-input-stream* "~A~%" command)
  (force-output *shell-input-stream*)

  ;; Collect output with timeout
  (let ((start-time (get-internal-real-time)))
    (loop while (< (/ (- (get-internal-real-time) start-time)
                      internal-time-units-per-second)
                   timeout)
          do (stdout)
            (sleep 0.05)))

  *stdout*)
