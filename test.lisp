
(asdf:load-system "shesh")

(in-package :shesh)

(init)

(format t "~%=== SHESH COMPREHENSIVE TESTS ===~%~%")

(format t "--- Section 1: Basic functionality ---~%~%")

(format t "Test 1.1 - Simple command (pwd):~%")
(sh pwd)
(format t "Result: ~A~%~%" *stdout*)

(format t "Test 1.2 - Command with arguments (ls -la | head -5):~%")
(exec "ls -la | head -5")
(format t "Result: ~A~%~%" *stdout*)

(format t "Test 1.3 - Echo without quotes:~%")
(sh echo hello world from lisp)
(format t "Result: ~A~%~%" *stdout*)

(format t "--- Section 2: Variable expansion with $ ---~%~%")

(format t "Test 2.1 - Simple variable expansion:~%")
(let ((x 42))
  (sh echo The answer is $x))
(format t "Result: ~A~%~%" *stdout*)

(format t "Test 2.2 - Multiple variables:~%")
(let ((name "Alice")
      (age 30))
  (sh echo Hello $name you are $age years old))
(format t "Result: ~A~%~%" *stdout*)

(format t "Test 2.3 - Variable containing spaces:~%")
(let ((message "Hello World from Lisp"))
  (sh echo Message is $message))
(format t "Result: ~A~%~%" *stdout*)

(format t "Test 2.4 - Numeric calculation:~%")
(let ((a 5)
      (b 3))
  (let ((sum (+ a b)))
    (sh echo $a + $b = $sum)))
(format t "Result: ~A~%~%" *stdout*)

(format t "Test 2.5 - Variable at beginning of command:~%")
(let ((cmd "echo"))
  (sh $cmd This is a test))
(format t "Result: ~A~%~%" *stdout*)

(format t "--- Section 3: Unbound variable handling ---~%~%")

(format t "Test 3.1 - Unbound variable (should be empty):~%")
(sh echo $UNDEFINED_VAR)
(format t "Result: '~A'~%~%" *stdout*)

(format t "Test 3.2 - Mix of bound and unbound:~%")
(let ((x 100))
  (sh echo x=$x but y=$y))
(format t "Result: ~A~%~%" *stdout*)

(format t "Test 3.3 - Shell environment variable:~%")
(exec "echo HOME=$HOME")
(format t "Result: ~A~%~%" *stdout*)

(format t "--- Section 4: Command completion detection ---~%~%")

(format t "Test 4.1 - Fast command timing:~%")
(let ((start-time (get-internal-real-time)))
  (sh echo Quick test)
  (let ((elapsed (/ (- (get-internal-real-time) start-time)
                    internal-time-units-per-second)))
    (format t "Result: ~A~%" *stdout*)
    (format t "Time taken: ~,3F seconds~%~%" elapsed)))

(format t "Test 4.2 - Command with 0.5 second delay:~%")
(let ((start-time (get-internal-real-time)))
  (exec "sleep 0.5 && echo 'After sleep'")
  (let ((elapsed (/ (- (get-internal-real-time) start-time)
                    internal-time-units-per-second)))
    (format t "Result: ~A~%" *stdout*)
    (format t "Time taken: ~,3F seconds (should be > 0.5)~%~%" elapsed)))

(format t "Test 4.3 - Command with no output:~%")
(sh true)
(format t "Result (should be empty): '~A'~%~%" *stdout*)

(format t "Test 4.4 - Command with error:~%")
(exec "ls /nonexistent 2>&1 || echo 'Error handled'")
(format t "Result: ~A~%~%" *stdout*)

(format t "--- Section 5: Advanced usage ---~%~%")

(format t "Test 5.1 - Chained commands:~%")
(exec "echo 'First' && echo 'Second' && echo 'Third'")
(format t "Result: ~A~%~%" *stdout*)

(format t "Test 5.2 - Variable in file path:~%")
(let ((dir "/tmp"))
  (exec (format nil "ls ~A | head -3" dir)))
(format t "Result (first 3 lines): ~A~%~%" *stdout*)

(format t "Test 5.3 - Complex example with variables:~%")
(let ((prefix "RESULT")
      (count 3))
  (sh echo $prefix - Found $count items in $UNKNOWN_DIR))
(format t "Result: ~A~%~%" *stdout*)

(format t "Test 5.4 - Shell state preservation:~%")
(sh cd /tmp)
(sh pwd)
(format t "Current directory: ~A~%" *stdout*)
(sh cd -)
(sh pwd)
(format t "Back to: ~A~%~%" *stdout*)

(close)
(format t "=== ALL TESTS COMPLETED ===~%")
