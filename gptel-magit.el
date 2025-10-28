;;; gptel-magit.el --- Generate commit messages for magit using gptel -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Authors
;; SPDX-License-Identifier: Apache-2.0

;; Author: Ragnar Dahlén <r.dahlen@gmail.com>
;; Version: 1.0
;; Package-Requires: ((emacs "28.1") (magit "4.0") (gptel "0.9.8"))
;; Keywords: vc, convenience
;; URL: https://github.com/ragnard/gptel-magit

;;; Commentary:

;; This package uses the gptel library to add LLM integration into
;; magit. Currently, it adds functionality for generating commit
;; messages.

;;; Code:

(require 'gptel)
(require 'magit)

(defconst gptel-magit-prompt-zed
  "You are an expert at writing Git commits. Your job is to write a short clear commit message that summarizes the changes.

If you can accurately express the change in just the subject line, don't include anything in the message body. Only use the body when it is providing *useful* information.

Don't repeat information from the subject line in the message body.

Only return the commit message in your response. Do not include any additional meta-commentary about the task. Do not include the raw diff output in the commit message.

Follow good Git style:

- Separate the subject from the body with a blank line
- Try to limit the subject line to 50 characters
- Capitalize the subject line
- Do not end the subject line with any punctuation
- Use the imperative mood in the subject line
- Wrap the body at 68 characters
- Keep the body short and concise (omit it entirely if not useful)"
  "A prompt adapted from Zed (https://github.com/zed-industries/zed/blob/main/crates/git_ui/src/commit_message_prompt.txt).")

(defconst gptel-magit-prompt-conventional-commits
  "You are an expert at writing Git commits. Your job is to write a short clear commit message that summarizes the changes.

The commit message should be structured as follows:

    <type>(<optional scope>): <description>

    [optional body]

- Commits MUST be prefixed with a type, which consists of one of the followings words: build, chore, ci, docs, feat, fix, perf, refactor, style, test
- The type feat MUST be used when a commit adds a new feature
- The type fix MUST be used when a commit represents a bug fix
- An optional scope MAY be provided after a type. A scope is a phrase describing a section of the codebase enclosed in parenthesis, e.g., fix(parser):
- A description MUST immediately follow the type/scope prefix. The description is a short description of the code changes, e.g., fix: array parsing issue when multiple spaces were contained in string.
- Try to limit the whole subject line to 60 characters
- Capitalize the subject line
- Do not end the subject line with any punctuation
- A longer commit body MAY be provided after the short description, providing additional contextual information about the code changes. The body MUST begin one blank line after the description.
- Use the imperative mood in the subject line
- Keep the body short and concise (omit it entirely if not useful)"
  "A prompt adapted from Conventional Commits (https://www.conventionalcommits.org/en/v1.0.0/).")

(defcustom gptel-magit-commit-prompt
  gptel-magit-prompt-conventional-commits
  "The prompt to use for generating a commit message.
The prompt should consider that the input will be a diff of all
staged changes."
  :type 'string
  :group 'gptel-magit)

(defcustom gptel-magit-diff-explain-prompt
  "You are an expert at understanding and explaining code changes by reading diff output. Your job is to write a short clear summary explanation of the changes the changes. Answer in Markdown format."
  "The prompt to use for explaining diff changes.
The prompt should consider that the input will be a diff some changes."
  :type 'string
  :group 'gptel-magit)

(custom-declare-variable
 'gptel-magit-model nil
 "The gptel model to use, defaults to `gptel-model` if nil.

See `gptel-model` for documentation.

If set to a model that uses a different backend than
`gptel-backend`, also requires `gptel-magit-backend' to be set to
the correct backend."
 :type (get 'gptel-model 'custom-type)
 :group 'gptel-magit)

(custom-declare-variable
 'gptel-magit-backend nil
 "The gptel backend to use, defaults to `gptel-backend` if nil.

See `gptel-backend` for documentation."
 :type (get 'gptel-backend 'custom-type)
 :group 'gptel-magit)

(defcustom gptel-magit-fill-body t
  "If non-nil, wrap the commit body to `gptel-magit-body-fill-column`."
  :type 'boolean
  :group 'gptel-magit)

(defcustom gptel-magit-body-fill-column 72
  "Fill column used when `gptel-magit-fill-body` is non-nil."
  :type 'integer
  :group 'gptel-magit)

(defcustom gptel-magit-subject-max-length 50
  "Maximum length for commit message subject line.
If the generated subject exceeds this length, it will be truncated
and an ellipsis will be added."
  :type 'integer
  :group 'gptel-magit)


(defun gptel-magit--fill-body-safely (body)
  "Fill BODY at `gptel-magit-body-fill-column`, but skip:
- fenced code blocks (``` ... ``` or ~~~ ... ~~~)
- list blocks (-, *, +, or numbered lists)
- ATX headings (# ...)

Fills only plain text paragraphs."
  (with-temp-buffer
    (insert body)
    (goto-char (point-min))
    (let ((fill-column gptel-magit-body-fill-column)
          (in-fence nil)
          (fence-rx (rx line-start (* space) (or "```" "~~~")))
          (list-rx  (rx line-start (* space)
                        (or (: (+ digit) ".") "-" "+" "*") (+ space)))
          (head-rx  (rx line-start (* space) "#" (+ space))))
      ;; Don't let adaptive prefixes cause odd wrapping.
      (setq-local adaptive-fill-mode nil)
      (while (not (eobp))
        (cond
         ;; Toggle code-fence regions: never fill inside.
         ((looking-at fence-rx)
          (setq in-fence (not in-fence))
          (forward-line 1))

         (in-fence
          (forward-line 1))

         ;; Skip list blocks: consume contiguous list lines unchanged.
         ((looking-at list-rx)
          (while (and (not (eobp))
                      (or (looking-at list-rx) (looking-at (rx line-start (* space) line-end))))
            (forward-line 1)))

         ;; Skip headings.
         ((looking-at head-rx)
          (forward-line 1))

         ;; Blank line: just move on.
         ((looking-at (rx line-start (* space) line-end))
          (forward-line 1))

         ;; Plain paragraph: fill this paragraph only.
         (t
          (let ((start (point)))
            (while (and (not (eobp))
                        (not (looking-at (rx line-start (* space) line-end)))
                        (not (looking-at fence-rx))
                        (not (looking-at list-rx))
                        (not (looking-at head-rx)))
              (forward-line 1))
            (fill-region start (point))))))
      (string-trim-right (buffer-string)))))

(defun gptel-magit--format-commit-message (message)
  "Keep subject as-is; optionally wrap the body.
MESSAGE is the full commit message to format."
  (let* ((msg (string-trim-right message))
         (nl (string-match "\n" msg))
         (subject (if nl (substring msg 0 nl) msg))
         (body (and nl (string-trim (substring msg (1+ nl)))))
         (overflow nil))
    ;; Move overflow content from subject if it exceeds max length
    (when (> (length subject) gptel-magit-subject-max-length)
      (let* ((truncate-pos gptel-magit-subject-max-length)
             ;; Find last word boundary before max length
             (last-space (or (string-match-p " [^ ]*\\'"
                                             (substring subject 0 truncate-pos))
                             truncate-pos))
             (split-pos (if (and last-space (> last-space 0))
                            last-space
                          truncate-pos)))
        (setq overflow (string-trim (substring subject split-pos)))
        (setq subject (string-trim-right (substring subject 0 split-pos)))))
    ;; Format the original body if needed
    (when (and body (not (string-empty-p body)) gptel-magit-fill-body)
      (setq body (gptel-magit--fill-body-safely body)))
    ;; Construct final message: subject + overflow (unwrapped) + body (wrapped)
    (cond
     ((and overflow body (not (string-empty-p body)))
      (format "%s\n\n%s\n\n%s" subject overflow body))
     (overflow
      (format "%s\n\n%s" subject overflow))
     ((and body (not (string-empty-p body)))
      (format "%s\n\n%s" subject body))
     (t subject))))

(defun gptel-magit--request (&rest args)
  "Call `gptel-request` with ARGS.

Respects configured model/backend options."
  (declare (indent 1))
  (let* ((gptel-backend (or gptel-magit-backend gptel-backend))
         (gptel-model (or gptel-magit-model gptel-model)))
    (apply #'gptel-request args)))

(defun gptel-magit--generate (callback)
  "Generate a commit message for current magit repo.
Invokes CALLBACK with the generated message when done."
  (let ((diff (magit-git-output "diff" "--cached")))
    (gptel-magit--request diff
      :system gptel-magit-commit-prompt
      :context nil
      :callback (lambda (response _info)
                  (let ((msg (gptel-magit--format-commit-message response)))
                    (funcall callback msg))))))

(defun gptel-magit-generate-message ()
  "Generate a commit message when in the git commit buffer."
  (interactive)
  (unless (magit-commit-message-buffer)
    (user-error "No commit in progress"))
  (gptel-magit--generate (lambda (message)
                           (with-current-buffer (magit-commit-message-buffer)
                             (save-excursion
                               (goto-char (point-min))
                               (insert message)))))
  (message "magit-gptel: Generating commit message..."))

(defun gptel-magit-commit-generate (&optional args)
  "Create a new commit with a generated commit message.
Uses ARGS from transient mode."
  (interactive (list (magit-commit-arguments)))
  (gptel-magit--generate
   (lambda (message)
     (magit-commit-create (append args `("--message" ,message "--edit")))))
  (message "magit-gptel: Generating commit..."))

(defun gptel-magit--show-diff-explain (text)
  "Popup a buffer with diff explanation TEXT."
  (let ((buffer-name "*gptel-magit diff-explain*"))
    (when-let ((existing-buffer (get-buffer buffer-name)))
      (kill-buffer existing-buffer))
    (let ((buffer (get-buffer-create buffer-name)))
      (with-current-buffer buffer
        (insert text)
        (setq fill-column 72)
        (fill-region (point-min) (point-max))
        (markdown-view-mode)
        (goto-char (point-min)))
      (pop-to-buffer buffer))))

(defun gptel-magit--do-diff-request (diff)
  "Send request for an explanation of DIFF."
  (gptel-magit--request diff
    :system gptel-magit-diff-explain-prompt
    :context nil
    :callback (lambda (response _info)
                (gptel-magit--show-diff-explain response)))
  (message "magit-gptel: Explaining diff..."))

(defun gptel-magit-diff-explain ()
  "Ask for an explanation of diff at current section."
  (interactive)
  (when-let* ((section (magit-current-section))
              (start (oref section content))
              (end (oref section end))
              (content (buffer-substring start end)))
    (gptel-magit--do-diff-request content)))

;;;###autoload
(defun gptel-magit-install ()
  "Install gptel-magit functionality."
  (define-key git-commit-mode-map (kbd "M-g") 'gptel-magit-generate-message)
  (transient-append-suffix 'magit-commit #'magit-commit-create
    '("g" "Generate commit" gptel-magit-commit-generate))
  (transient-append-suffix 'magit-diff #'magit-stash-show
    '("x" "Explain" gptel-magit-diff-explain)))

(provide 'gptel-magit)
;;; gptel-magit.el ends here
