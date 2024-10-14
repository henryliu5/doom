;;; test.el -*- lexical-binding: t; -*-

(require 'treesit)
(define-minor-mode my-context-mode
  "A minor mode that displays parent nodes in the header line using Tree-Sitter."
  :lighter " Context" ;; This shows in the mode line when the mode is active
  :global nil ;; Make it buffer-local
  (if my-context-mode
      (my-context-mode-enable)
    (my-context-mode-disable)))

(defun my-context-mode-enable ()
  "Enable the context mode."
  (add-hook 'post-command-hook 'my-update-header-line nil t) ;; Update after every command
  ;; Ensure Tree-Sitter is active
  (unless (bound-and-true-p tree-sitter-mode)
    (tree-sitter-mode 1))
  (add-hook 'tree-sitter-after-on-hook 'my-update-header-line nil t)
  (add-hook 'tree-sitter-after-first-parse-hook 'my-update-header-line nil t))

(defun my-context-mode-disable ()
  "Disable the context mode."
  (remove-hook 'post-command-hook 'my-update-header-line t)
  (setq header-line-format nil)) ;; Clear the header line when the mode is disabled

(defun my-update-header-line ()
  "Update the header line with the number of parent nodes from the syntax tree."
  (add-parents-to-header)
)

(defun get-not-visible-parent-nodes ()
  "Return a list of useful parent nodes (of the current node)
   that are outside of the visible buffer.

   Useful nodes are either class or function definitions"
  (interactive)
  (let ((node (tree-sitter-node-at-pos))
        (parents '()))
    (while node
      ;; Move to the parent node and add it to the list of parents
      (setq node (tsc-get-parent node))
      (when (and node
             (member (tsc-node-type node) '(function_definition class_definition))
             ;; (< (line-number-at-pos (tsc-node-start-position node)) (line-number-at-pos (window-start)))
             )
            (push node parents))
      )  ;; Add the node to the parents list
    parents
    )
)

(defun get-parent-nodes-first-lines ()
  "LLM generated:
   Return a string of the first line of text for each parent node,
   preserving indentation, separated by newlines."
  (interactive)
  (let ((parents (get-not-visible-parent-nodes))  ;; Get all parent nodes
        (result ""))
    ;; Iterate over each parent node
    (dolist (parent parents)
      (let* ((node-start (tsc-node-start-position parent))
             (line-no (line-number-at-pos node-start))
             )
        (when node-start
          ;; Extract the first line of the node, preserving indentation
          (save-excursion
            (goto-char node-start)
            (let* ((line-text (buffer-substring
                              (line-beginning-position)
                              (line-end-position)))
                   (format-text (string-trim line-text))
                   )  ;; Get text from node start to end of line
              ;; Append to the result with a newline
              (setq result (concat result (format "\t %s %s \n" line-no format-text))))))))  ;; Append the first line to the result
    result))

(defun get-current-header-text ()
  "Get the parent nodes' first lines and display them in the minibuffer."
  (interactive)
  (let ((header-text (get-parent-nodes-first-lines)))
    ;; Ensure header-text is not nil and is a string
    (message "%s" (or header-text "No parent nodes found")))
)

(defun add-parents-to-header ()
  "show parent nodes in header line"
  (interactive)
  (let* ((text (get-parent-nodes-first-lines))
         (format-text (replace-regexp-in-string "\n" "->" (string-remove-suffix "\n" text))))
    (setq header-line-format (list format-text))
))
