
(asdf:load-system "shesh")

(in-package :shesh)

(defun ex-basic ()
  "Demonstrate basic shell command execution."
  (format t "~%=== Basic Usage Examples ===~%~%")

  (init)

  (format t "1. Getting current directory:~%")
  (sh pwd)
  (format t "   Result: ~A~%~%" *stdout*)

  (format t "2. Listing files:~%")
  (sh ls -la)
  (format t "   Files in current directory...~%~%")

  (format t "3. Echo without quotes:~%")
  (sh echo Hello World from Common Lisp)
  (format t "   Result: ~A~%~%" *stdout*)

  (close))

(defun ex-vars ()
  "Demonstrate variable expansion with $ syntax."
  (format t "~%=== Variable Expansion Examples ===~%~%")

  (init)

  (format t "1. Simple variable substitution:~%")
  (let ((name "Alice"))
    (sh echo Hello $name)
    (format t "   Result: ~A~%~%" *stdout*))

  (format t "2. Numeric calculation:~%")
  (let ((x 10)
        (y 5))
    (let ((result (* x y)))
      (sh echo $x times $y equals $result)
      (format t "   Result: ~A~%~%" *stdout*)))

  (format t "3. Variable containing spaces:~%")
  (let ((message "Common Lisp rocks!"))
    (sh echo Message is: $message)
    (format t "   Result: ~A~%~%" *stdout*))

  (close))

(defun ex-practical ()
  "Practical examples of shell integration."
  (format t "~%=== Practical Examples ===~%~%")

  (init)

  (format t "1. Working with files:~%")
  (let ((filename "/tmp/shesh-test.txt")
        (content "Hello from Shesh!"))
    (exec (format nil "echo '~A' > ~A" content filename))
    (sh cat $filename)
    (format t "   File contents: ~A~%" *stdout*)
    (sh rm $filename)
    (format t "   File cleaned up.~%~%"))

  (format t "2. Getting process information:~%")
  (exec "ps aux | grep sbcl | head -1")
  (format t "   Current SBCL process: ~A~%~%"
    (subseq *stdout* 0 (min 60 (length *stdout*))))

  (format t "3. Checking git status:~%")
  (exec "git status --short 2>/dev/null || echo 'Not a git repository'")
  (format t "   Status: ~A~%~%" *stdout*)

  (close))

(defun ex-advanced ()
  "Demonstrate advanced features."
  (format t "~%=== Advanced Features ===~%~%")

  (init)

  (format t "1. Command completion detection:~%")
  (let ((start (get-internal-real-time)))
    (exec "sleep 0.3 && echo 'Task completed'")
    (let ((elapsed (/ (- (get-internal-real-time) start)
                      internal-time-units-per-second)))
      (format t "   Result: ~A~%" *stdout*)
      (format t "   Time taken: ~,3F seconds~%~%" elapsed)))

  (format t "2. Shell state is preserved between commands:~%")
  (sh cd /tmp)
  (sh pwd)
  (format t "   Current dir: ~A~%" *stdout*)
  (sh cd -)
  (sh pwd)
  (format t "   Back to: ~A~%~%" *stdout*)

  (format t "3. Error handling:~%")
  (exec "ls /nonexistent 2>&1 || echo 'Directory not found'")
  (format t "   Result: ~A~%~%" *stdout*)

  (close))

(defun run-all ()
  "Run all example functions."
  (ex-basic)
  (ex-vars)
  (ex-practical)
  (ex-advanced)
  (format t "~%=== All examples completed! ===~%"))

(format t "~%Shesh examples loaded. Run (run-all) to see all examples.~%")
(format t "Or run individual examples:~%")
(format t "  - (ex-basic)~%")
(format t "  - (ex-vars)~%")
(format t "  - (ex-practical)~%")
(format t "  - (ex-advanced)~%~%")
