# Shesh: A Shell Interface for Common Lisp

**🚧 Work in Progress 🚧**

Shesh is a lightweight Common Lisp library that provides a simple and intuitive interface for interacting with shell processes.

Coded with the help of a coding assistant. 

## Installation

1. Clone this repository to your local machine. Clone it in your `quicklisp/local-project` or declare it in `source-registry.conf`
2. Load the system

```lisp
(asdf:load-system "shesh")
```

or 

```lisp
(ql:quickload "shesh")
```

## Basic Usage

```lisp
(in-package :shesh)

;; Initialize a shell session
(init-shell)

;; Execute commands using the sh macro (no quotes needed!)
(sh pwd)
;; => "/home/user/projects"

(sh ls -la)
;; => "total 64\ndrwxr-xr-x  5 user user 4096 ..."

;; Use Lisp variables in shell commands
(let ((name "Alice")
      (count 5))
  (sh echo Hello $name, you have $count messages))
;; => "hello Alice, you have 5 messages"

;; Access the output
*stdout*
;; => "hello Alice, you have 5 messages"

;; Change directory (state is preserved)
(sh cd /tmp)
(sh pwd)
;; => "/tmp"

;; Close the shell when done
(close-shell)
```

## Variable Expansion

Shesh supports automatic variable expansion using the `$` prefix:

```lisp
;; Simple variable substitution
(let ((x 42))
  (sh echo The answer is $x))
;; => "the answer is 42"

;; Variables with spaces
(let ((message "Hello World"))
  (sh echo $message))
;; => "Hello World"

;; Multiple variables
(let ((user "Bob")
      (dir "/home"))
  (sh echo User $user home is $dir/$user))
;; => "user Bob home is /home/Bob"

;; Unbound variables are passed to the shell as-is
(sh echo $HOME)  ; Shell environment variable
;; => "/Users/username"
```

## API Reference

### Macros

- `(sh &rest command-parts)` - Execute a shell command without quotes
  - Supports variable expansion with `$` prefix
  - Example: `(sh echo hello world)`

### Functions

- `(init-shell &optional shell-command)` - Initialize a new shell process
- `(exec command)` - Execute a command string in the current shell
- `(stdout)` - Manually consume available output from the shell
- `(close-shell)` - Close the current shell process

### Special Variables

- `*current-shell*` - The current shell process object
- `*stdout*` - Buffer containing the shell output


## Testing

Run the test suite with:

```lisp
(load "test.lisp")
