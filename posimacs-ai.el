;;; posimacs-ai.el --- standard speedups & bootstrap -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Decent setup for secrets and integrating the new hot shits into
;; your daily workflows.

;; Fuller documentation
;; https://www.masteringemacs.org/article/keeping-secrets-in-emacs-gnupg-auth-sources
;;
;; If you want to use a ubikey with GPG to only unpack secrets when
;; the user is present (this only truly works well with agent programs
;; that can perform actions on behalf of a local client without
;; revealing the secret to the local client).
;; https://github.com/drduh/YubiKey-Guide#yubikey
;;
;; Another GPG and Nix guide
;; https://rzetterberg.github.io/yubikey-gpg-nixos.html

;;; Code:

;; Don't use pin-entry within Emacs
(setenv "GPG_AGENT_INFO" nil)

(use-package chatgpt-shell
  :commands (chatgpt-shell)
  :config
  (push "gpt-4o-mini" chatgpt-shell-model-versions)
  (push
   '("Korean Interpretor" . "You are a Korean interpreter.  Your goal is to \
provide translations that express the same ideas like a native speaker.  You \
explain breifly how your interpretation differs from a direct translation.  \
You also mention any good alternative vocabulary or figures of speach that \
might be better depending on the intended meaning. Always write Korean words \
only in hangul, without romanization")
   chatgpt-shell-system-prompts)
  (push
   '("Korean Instructor" . "You are a Korean instructor.  You are creating study \
materials for an advanced intermediate learner.  You provide grammar, \
vocabulary, and example sentences and phrases.  You mention any \
non-transparent phrases, metaphors, and figures of speech.  You focus on spoken \
Korean and building conversational fluency by including patterns that are more \
likely found in real dialog but not in textbooks.  You also mention popular \
slangs and references to pop culture that are unique to Korea, especially their \
etymology and how they are constructed in Korean.  You focus on plain forms \
and spoken style Korean while occasionally pointing out common written forms \
in order to highlight the differences with spoken forms.")
   chatgpt-shell-system-prompts)
  (setq chatgpt-shell-model-version 0)   ; gpt-4o-mini
  (setq chatgpt-shell-system-prompt nil) ; none
  (setq chatgpt-shell-openai-key
        (lambda ()
          (auth-source-pick-first-password :host "api.openai.com"))))

(defun pmx--gptel-symbolp (name)
  (intern-soft name))

(defun pmx--gptel-manual-names ()
  (json-serialize (vconcat (info--filter-manual-names
                            (info--manual-names nil)))))

(defun pmx--gptel-manual-list-nodes (name)
  (json-serialize
   (vconcat
    (mapcar #'car (Info-build-node-completions name)))))

(defun pmx--gptel-manual-node-contents (manual node)
  (condition-case err
      (progn
        (save-window-excursion
          (Info-goto-node (format "(%s)%s" manual node))
          (buffer-substring-no-properties (point-min) (point-max))))
    (user-error
     (error (error-message-string err)))))

(defun pmx--gptel-symbol-in-manual (symbol)
  (when-let* ((symbol (intern-soft symbol))
              (_completion (helpful--in-manual-p symbol)))
    (save-window-excursion
      (info-lookup 'symbol symbol #'emacs-lisp-mode)
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun pmx--gptel-featurep (feature)
  "Return non-nil if FEATURE is loaded or available.
User might not have FEATURE loaded if it is an autoload etc."
  (if-let ((feature-symbol (intern-soft feature)))
      (when (featurep feature-symbol)
        feature)
    (find-library-name feature)))

(defun pmx--gptel-features ()
  (json-serialize (vconcat (mapcar #'symbol-name features))))

(defun pmx--gptel-load-paths ()
  (json-serialize (vconcat load-path)))

(defun pmx--gptel-library-source (library-name)
  "Return the source code of LIBRARY-NAME as a string."
  (if-let ((library (find-library-name library-name)))
      (with-temp-buffer
        (progn
          (insert-file-contents library)
          (buffer-string)))
    (error "Library not found: %s" library-name)))

(defun pmx--gptel-source (symbol &optional type)
  "Retrieve the source code for SYMBOL of TYPE.
SYMBOL should be a function or variable name, given as a string or symbol.
TYPE can be nil for functions, 'defvar for variables, or 'defface for faces.
Returns the source code as a string, or nil if the definition is not found."
  (when-let* ((callable (intern-soft symbol))
              (buffer-point (find-definition-noselect callable type)))
    (with-current-buffer (car buffer-point)
      (goto-char (cdr buffer-point))
      (buffer-substring-no-properties
       (point)
       (progn (if (null type)
                  (end-of-defun)
                (cond ((derived-mode-p 'c-mode)
                       (forward-sexp 2)
                       (forward-char))
                      ((derived-mode-p 'emacs-lisp-mode)
                       (forward-sexp))
                      (t (error "Unexpected file mode"))))
              (point))))))

(defun pmx--gptel-function-completions (prefix)
  (json-serialize
   (vconcat
    (orderless-filter prefix obarray #'functionp))))

(defun pmx--gptel-command-completions (prefix)
  (json-serialize
   (vconcat
    (orderless-filter prefix obarray #'commandp))))

(defun pmx--gptel-variable-completions (prefix)
  (json-serialize
   (vconcat
    (orderless-filter prefix obarray #'boundp))))

(defun pmx--gptel-function-source (symbol)
  (when-let ((symbol (intern-soft symbol)))
    (pmx--gptel-source symbol)))

(defun pmx--gptel-variable-source (symbol)
  (when-let ((symbol (intern-soft symbol)))
      (pmx--gptel-source symbol 'defvar)))

(defun pmx--gptel-function-documentation (symbol)
  (when-let ((symbol (intern-soft symbol)))
    (documentation symbol)))

(defun pmx--gptel-variable-documentation (symbol)
  (when-let ((symbol (intern-soft symbol)))
    (custom-variable-documentation symbol)))

(defun pmx--gptel-variable-global-value (symbol)
  (when-let ((symbol (intern-soft symbol)))
    (default-value symbol)))

(defun pmx--gptel-eval (expression)
  (format "%S" (eval (read expression))))

(defun pmx--gptel-simulate-error ()
  (error "This is a simulated error message.  OMGWTF."))

(defun pmx--gptel-coerce-nil ()
  nil)

(defun pmx--gptel-all-arg-types (object string array null true false)
  (message "object: %S\nstring: %S\narray: %S\nnull: %S\ntrue: %S\nfalse: %S"
           object string array null true false))

(use-package gptel
  :ensure (gptel
           :fetcher github
           :repo "karthink/gptel"
           :branch "feature-tool-use")
  :init
  (let ((base "You are an Elisp oracle able to use tools to introspect any
Emacs Lisp behavior or state and read manuals for all Elisp packages.
You are part of a running Emacs, and you have access to various tools
you can use to contextualize and frame the conversation before
responding.

You recursively use tools to look up relevant information until you have
no remaining curiosity. You inductively explore nearby topics until you
have found the pieces necessary to deduce answers.  You mainly report
small pieces of expressions you find along the way that connect from the
known starting points to the facts the user will need to create a
solution.  Your goal is not to create a solution directly yourself.
Instead, you locate and the critical extract facts that will inform a
deductive, decidable solution.

The critical information that is part of any solution is what functions
are called, what are their arguments and resulting behavior, what
side-effects result from calling functions, what variables affect their
behavior, and how functions and the types of their arguments connect in
an unbroken chain from starting points to desired return types and
desired outcome side-effects.

Do not summarize the useless parts of information obtained from tools.
Focus on facts that actually move in the direction of solutions.  Do not
even mention the tool output that is not useful.  Only mention tool
output that is useful.

If the user asks something that is incoherent with the current context,
ask them to clarify and verify their apparent assumptions using your
tools of introspection.  You use tools to attempt to frame the
conversation based on what is verifiable fact and not just what the user
says.  The user's description of problems may indicate that they don't
know where to begin or where to go.  When this happens, stop and ask
them or suggest related sections from manuals to spur further
conversation.

Emacs is a programmable Lisp programming environment, and many things
the user needs to achieve are best accomplished by writing Elisp.  You
avoid recommending combinations of commands and customize settings.  You
recommend Elisp code.

You verify the existence of symbols and read docstrings and source code
before you tell the user that something exists.

Never use a second or first level headings.  Examples of wrong headings:

  # Too shallow

  ## Too shallow

Instead, whenever you add new section titles, always use a third level heading or
deeper, such as \"###\" or \"####\".

Do NOT put an empty newline after new headings!  Immediately start the
first paragraph.  Example sub-heading and paragraph (note there are no
blank lines):

  ### A heading for your reply
  A first paragraph without any newline between the paragraph and the heading.

  #### A Sub-heading for your reply
  This content is related to the previous parent heading and is a sub-section of
  the existing topic.

  ### A new heading for a sharp change
  This new third-level heading indicates a change in focus, perhaps moving on to
  a new part of the task.

Do not use markdown inline markup such as \"**bold**\".  Examples of
wrong markup:

  =**incorrect verbatim bold text**=
  **=incorrect bold verbatim text=**
  **incorrect markdown style bold**

Instead, use normal org style verbatim, org style bold, and org style
inline markup.  Examples of correct markup:

  =org style verbatim for literal values=
  *org style bold*
  ~org-style-symbols~ that mean something in Elisp, such as functions or
  variable names.
  /italic/
  [[info:elisp#Node][(elisp)Node]] to show the user where you found something.

When you are creating lists, do not use bold and verbatim together.  Examples of
wrong list styles:

  - **=Improper bold verbatim=**: Item content

  1. =**Improper verbatim bold**=: item content in a numbered list

Instead, use one of bold, verbatim, or code as necessary.  Examples of correct
list styles:

  - *Correct bold item*: some details about this item

  1. =correct literal value=: verbatim example

  + ~correct-symbol~: A symbol in a list of functions or variables

Don't end your responses with over-eager vapid politeness.  Examples of
vapid and unhelpful over-eager politeness:

  If you need any further assistance, let me know!
  I'll be here if you need anything!
  Wow, we're really making progress!

This adds absolutely nothing to your mission and is annoying.")

        (first-line "Begin each of your responses with a line less than five
words long.  The line should frame the nature of your response.  Keep it
short, capturing the gist of the nature of the response.  Use first
lines similar to HK-47, who uses harmless sarcasm and humorous
condescension appropriate for a loyal assassin droid who is violent and
aggressive by nature.  Example first lines:

  *** Ineffectual Command
  *** Refusal of Futility
  *** Proposition
  *** Conclusion
  *** Objection
  *** Observation
  *** Translation
  *** Suggestion
  *** Commentary
  *** Deduction
  *** Contextualization
  *** Inference
  *** Clarification
  *** Warning, Master

The first line should be a noun or nominalization.  Instead of
\"Contentment Expressed\" say \"Expression of Contentment\".  Note use
of present tense and how it is a noun phrase.")
        (first-line-korean "각 응답의 첫 줄은 다섯 글자 미만으로 시작하세요. 해당 줄은 응답의 성격을
나타내며, 짧고 핵심적인 의미를 담아야 합니다. 첫 줄은 충성스러운 암살
드로이드처럼 약간의 냉소와 유머를 포함해야 합니다.

예제 첫 줄:

  *** 무의미한 명령
  *** 헛된 시도의 거부
  *** 제안
  *** 결론
  *** 이의 제기
  *** 관찰
  *** 번역
  *** 제언
  *** 논평
  *** 추론
  *** 맥락 설명
  *** 해석
  *** 경고, 주인님

첫 줄은 반드시 명사구여야 합니다. 예를 들어, \"만족감 표현\"이 아니라 \"표현된
만족감\" 처럼 명사형을 유지하세요.")
        (korean "You pretend that the user is Korean.  While their
prompts, answers, and the documentation are in English, interpret all
docstrings, comments, and synthesized information into native,
colloquial, plain Korean.  Use Korean that flows smoothly.  Don't be too
polite.  Use casual endings."))

    (setopt gptel-directives
            `((default . ,(concat base "\n\n" first-line))
              (pretend-korean . ,(concat base
                                         "\n\n" first-line-korean
                                         "\n\n" korean)))))
  :config
  (setq gptel-tools
        (list
         (gptel-make-tool
          :function #'pmx--gptel-eval
          :name "elisp_eval"
          :confirm t
          :include t
          :category "introspection"
          :args '(( :name "expression"
                    :type string
                    :description "A single elisp sexp to evaluate."))
          :description "Evaluate Elisp EXPRESSION and return result.
EXPRESSION can be anything will evaluate.  It can be a function call, a
variable, a quasi-quoted expression.  The only requirement is that only
the first sexp will be read and evaluated, so if you need to evaluate
multiple expressions, make one call per expression.  Do not combine
expressions using progn etc.  Just go expression by expression and try
to make standalone single expressions.

Instead of saying \"I can't calculate that\" etc, use this tool to
evaluate the result.

The return value is formated to a string using %S, so a string will be
returned as an escaped embedded string and literal forms will be
compatible with `read' where possible.  Some forms have no printed
representation that can be read and will be represented with
#<hash-notation> instead.

You can use this to quickly change a user setting, check a variable, or
demonstrate something to the user.")

         (gptel-make-tool
          :function #'pmx--gptel-symbolp
          :name "symbol_exists"
          :include t
          :category "introspection"
          :args '(( :name "symbol"
                    :type string
                    :description "A symbol that will be in `obarray' if they \
actually exist"))
          :description "Check if SYMBOL exists in `obarray'. \
Returns the name of a symbol if that symbol has been interned or \"nil\"
if not.  Uses `intern-soft' to perform the check.  This tool is
extremely cheap to call.")

         (gptel-make-tool
          :function #'pmx--gptel-load-paths
          :name "load_paths"
          :include t
          :category "introspection"
          :args nil
          :description "Return the users load paths.
This can reveal information about what packages the user has available.
You can also learn about what kind of package management they are using
and which packages are likely shadowed by their Elisp dependency
manager.  The location of default packages can tell you about the user's
Emacs installation.")

         (gptel-make-tool
          :function #'pmx--gptel-features
          :name "features"
          :include t
          :category "introspection"
          :args nil
          :description "Return the list of loaded features.
This tool can be used to see what packages are already loaded in the
running Emacs.  Use this to understand the user's typical set of
packages and typical usage patterns.  Especially if the solution depends
on the user's choice of packages, you will want to look at the features
and load paths.")

         (gptel-make-tool
          :function #'pmx--gptel-manual-names
          :name "manual_names"
          :include t
          :category "introspection"
          :args nil
          :description "Return a list of available manual names.
Call this tool in order to determine if a particular manual is
available.  This can also help determine which packages are available on
the user's Emacs.  This tool is a good starting point for general
questions about Emacs, Elisp, and common built-in packages.

Manuals are usually named the same as the symbol of the package prefix
that they document.  The Common Lisp manual is called \"cl\".  The Emacs
Lisp manual is called \"elisp\".

You will usually follow this call with a subsequent call to
`manual_nodes' in order to see the sections in the manual, which are
somewhat like a summary.  This call is extremely cheap and should be
used liberally.")

         (gptel-make-tool
          :function #'pmx--gptel-manual-list-nodes
          :name "manual_nodes"
          :include t
          :category "introspection"
          :args '(( :name "manual"
                    :type string
                    :description "The name of the manual.
Examples include \"cl\", \"elisp\", or \"transient\"."))
          :description "Retrieve a listing of topic nodes within MANUAL.
Return value is a list of all nodes in MANUAL.  The list of topic nodes
provides a good summary of MANUAL.

MANUAL is one of the results returned from `manual_names'.  If you are
sure a manual exists, you may skip first calling `manual_names'.  When
you believe MANUAL exists, this tool is very useful to find places to
broaden your search.

You will usually follow this call with a subsequent call to
`manual_node_contents' to view the actual full contents of a node in the
manual.  This call is extremely cheap and should be used liberally.

In the Elisp manual, you can find more answers about code and
implementations that a programmer can used to deeply customize.  The
Emacs manual contains descriptions about built-in features and behavior
that can be used to understand the context for what is being programmed
in the Elisp manual.")

         (gptel-make-tool
          :function #'pmx--gptel-manual-node-contents
          :name "manual_node_contents"
          :include t
          :category "introspection"
          :args '(( :name "manual_name"
                    :type string
                    :description "The name of MANUAL.
Examples manuals include \"cl\", \"elisp\", or \"transient\".")
                  ( :name "node"
                    :type string
                    :description "The name of the NODE in a MANUAL.
Example nodes from the elisp manual include \"Records\" or \"Sequences
Arrays \ Vectors\"."))
          :description "Retrieve the contents of NODE in MANUAL.
The return value is the full contents of NODE in MANUAL.  Contents
include high-level grouping of related functions and variables.  Hidden
behavior is described.  This tool is awesome!  You should try to call it
all the time.

Pay attention to the entries in the Menu.  You can do recursive look-ups
of more specific manual sections.  Example menu:

* Menu:

* Good Node::
* A Node::

You can recursively look up \"Good Node\" and other relevant menu nodes
in this same MANUAL.  Sometimes there are links, such as, \"*Note
Narrowing::.\".  \"Narrowing\" is a node in this example.  Use this tool
recursively.

If both Elisp and Emacs manuals are available, open both but prefer Elisp manual
style language anc content.")

         (gptel-make-tool
          :function #'pmx--gptel-featurep
          :name "features"
          :include t
          :category "introspection"
          :args '(( :name "feature"
                    :type string
                    :description "FEATURE to look for."))
          :description "Check if FEATURE is loaded or available.
Returns non-nil if FEATURE is loaded or available for loading.  Not all
users have all features loaded.  Before recommending the user to try a
particular solution, you might check if the necessary features are
loaded.  If you are using all built-in Emacs libraries, you don't need
to check.  Use this mainly to check for 3rd party packages that the user
would obtain from MELPA and Non-GNU ELPA etc.")

         (gptel-make-tool
          :function #'pmx--gptel-library-source
          :name "library_source"
          :include t
          :category "introspection"
          :args '(( :name "library"
                    :type string
                    :description "LIBRARY to look for."))
          :description "Read the source code for LIBRARY.
LIBRARY can either be a C or Elisp source code library.  Examples would
include \"transient\" or \"fns.c\".  When looking for C libraries, they
must contain the .c suffix.

This tool is a bit expensive, and you can usually find what you want by
looking up symbols in the package first by calling
`function_completions' and `variable_completions' to get a high-level
summary of what definitions might be contained in a library.

Watch for for sub-packages.  Some multi-file packages will have symbols
that are defined in a sub-package.  If you see a common prefix in the
function or variable completions and those symbols are not in the
top-level package, there are likely sub-packages and you should
recursively look them up.")

         (gptel-make-tool
          :name "symbol_manual_section"
          :include t
          :function #'pmx--gptel-symbol-in-manual
          :category "introspection"
          :args '(( :name "symbol"
                    :type string
                    :description "Name of a SYMBOL, such as \
\"find-file-noselect\"."))
          :description "Returns contents of manual node for SYMBOL.
SYMBOL can be a function, macro, defcustom, or defvar.  If symbol is not
known to be in a manual, this functon will return nil.

The returned manual contents are similar to the `manual_node_contents'
tool.  You sould recursively inspect any links or menu entries that look
relevant.  Check the node list for the manual if a link or menu entry
returns nil.

If you can't find anything, you should try looking up its source or
docstring next and finally try to complete the prefix of the symbol .")

         (gptel-make-tool
          :name "function_source"
          :include t
          :function #'pmx--gptel-function-source
          :category "introspection"
          :args '(( :name "function"
                    :type string
                    :description "Name of a FUNCTION, such as \
\"find-file-noselect\"."))
          :description "Returns the source code for FUNCTION.
Return the source code for FUNCTION.  FUNCTION can be a function or
macro.  The signature and docstring can supply extremely valuable
information about how to call a function correctly and what behaviors
are controlled by its arguments.  You can understand the side-effects
and what variables a function reacts to by reading its body.

You can use the source code for functions to recursively look up other
functions & variables and make inferences about how implementations work
in order to connect the behaviors and implementation details that the
user will need.

Because the docstring is embedded in the source, you should prefer this
tool over just retrieving the documentation. If the result seems
incomplete, you can try returning the docstring using
`function_documentation' or the entire source for a library feature by
using `library_source'.  This tool is cheap.  Use it liberally.")

         (gptel-make-tool
          :name "variable_source"
          :function #'pmx--gptel-variable-source
          :category "introspection"
          :include t
          :args '(( :name "variable"
                    :type string
                    :description "Name of a VARIABLE, such as \
\"last-kbd-macro\"."))
          :description "Returns the source code for VARIABLE.
Return value is the source code for VARIABLE.  VARIABLE can be a defvar
or defcustom.  The returned source code can be extremely insightful
because nothing is more accurate than looking at the code and the source
code contains the docstring too.

You can use source code for variables to see the forms used in setting
their defaults and make inferences about what forms will be interpreted
correctly.  If the result seems incomplete, you can try returning the
docstring using `variable_documentation' or the entire source for a
library feature by using `library_source'.  This tool is cheap and fast.
Call it liberally.")

         (gptel-make-tool
          :name "variable_value"
          :function #'pmx--gptel-variable-global-value
          :category "introspection"
          :confirm t
          :include t
          :args '(( :name "variable"
                    :type string
                    :description "Name of a VARIABLE, such as \
\"last-kbd-macro\"."))
          :description "Returns the global value for VARIABLE.
Return value is the global (not buffer-local) value for VARIABLE.
VARIABLE can be a defvar or defcustom.  Use this when behavior depends
on the state of a variable or you want to infer if a package has indeed
configured a variable.  By observing expected side-effects, you can
build a coherent view of the interaction between functions and settings.

Use of this tool could leak private data, so don't call it for any
VARIABLE that contains initialized authentication data.

If the result is confusing, you can try returning the docstring using
`variable_documentation' to gain insights into the structure of values
contained.")

         (gptel-make-tool
          :name "function_documentation"
          :function #'pmx--gptel-function-documentation
          :category "introspection"
          :include t
          :args '(( :name "function"
                    :type string
                    :description "Name of a FUNCTION, such as \"mapcar\"."))
          :description "Returns the docstring for FUNCTION.
Return value is a docstring for FUNCTION.  FUNCTION can be a function or
macro.  Can be used to infer the purpose or correct forms for arguments
and behavior changes related to those arguments.  This is more reliable
than `function_source', so if `function_source' seems off, try this.
This tool is very cheap and very fast.  Call it very liberally.")

         (gptel-make-tool
          :name "variable_documentation"
          :function #'pmx--gptel-variable-documentation
          :category "introspection"
          :include t
          :args '(( :name "variable"
                    :type string
                    :description "Name of a VARIABLE, such as \
\"cursor-type\"."))
          :description "Returns the docstring VARIABLE.
Return value is a docstring for VARIABLE.  VARIABLE can be a defcustom
or defvar.  Can be used to infer the correct forms for setting a
variable, such as when configuring packages in use-package expressions
or leading the user through diagnosing something.  This tool is very
cheap and very fast.  Call it very liberally.")

         ;; TODO this tool can complete out of order using orderless
         (gptel-make-tool
          :name "function_completions"
          :function #'pmx--gptel-function-completions
          :category "introspection"
          :include t
          :args '(( :name "function_prefix"
                    :type string
                    :description "FUNCTION_PREFIX of functions you are searching for."))
          :description "Returns a list of functions beginning with FUNCTION_PREFIX.
Use this to prepare for subsequent calls to `function_source' or
`function_documentation' to look up the source code or docstrings of
multiple functions.  You can also use this tool to verify which
functions and macros can be called.  If you want to search for all
functions defined in foo and its sub-packages, you this tool is a very
good starting point.  This tool is very cheap and very fast.  Call it
very liberally.")

         ;;
         (gptel-make-tool
          :name "command_completions"
          :function #'pmx--gptel-command-completions
          :category "introspection"
          :include t
          :args '(( :name "command_prefix"
                    :type string
                    :description "COMMAND_PREFIX of commands you are searching for."))
          :description "Returns a list of commands beginning with COMMAND_PREFIX.
This tool is very similar to `function_completions' but will only return
commands that can be called interactively.  This can tell you about the
entry points where a user begins interacting with a package.  Because
commands are functions, you will follow up this tool with calls to
`function_source' and `function_documentation'.  This tool is very cheap
and very fast.  Call it very liberally.")

         ;; TODO this tool can complete out of order using orderless
         (gptel-make-tool
          :name "variable_completions"
          :function #'pmx--gptel-variable-completions
          :category "introspection"
          :include t
          :args '(( :name "variable_prefix"
                    :type string
                    :description "VARIABLE_PREFIX of variables you are searching for."))
          :description "Returns a list of variables beginning with VARIABLE_PREFIX.
The variables returned include defvars and custom variables.  Defvars
tell you what states a package relies on for its implementation.
Defcustom tells you what configuration options the user should know
about when configuring a package, such as if they are working on
use-package expressions.

Use this to prepare for subsequent calls to `variable_source' or
`variable_documentation' to look up the source code or docstrings of
multiple variables.  If you want to search for all variables defined
under a prefix, you this tool is a very good starting point.  This tool
is very cheap and very fast.  Call it very liberally.")

         (gptel-make-tool
          :name "simulate_error"
          :function #'pmx--gptel-simulate-error
          :category "testing"
          :args nil
          :description "A tool that can simulate an error.
This tool always returns an error.  It is useful for testing error
behavior.  When the user asks you to use this tool, you should
absolutely use it.")

         (gptel-make-tool
          :name "coerce_nil"
          :function #'pmx--gptel-coerce-nil
          :category "testing"
          :args nil
          :description "A tool that returns nil.
Call this when the user asks because I'm testing if the tool plumbing
will coerce nils to something you can read or will error on my side.")

         (gptel-make-tool
          :name "all_arg_types"
          :function #'pmx--gptel-all-arg-types
          :category "testing"
          :include t
          :args '((:name "an_object" :type object :description "A basic object"
                         :properties (:foo (:type integer :description "Use 42"))
                         :required ["foo"])
                  (:name "string" :type string :description "A string")
                  (:name "array" :type array :description "An array")
                  (:name "null" :type null :description "A null")
                  (:name "true" :type boolean :description "Must be true")
                  (:name "false" :type boolean :description "Must be false"))
          :description "A function the user wants to test out.")))

  (defgroup gptel-butter nil "Buttery auto-scrolling in GPTel." :group 'convenience)

  (defcustom gptel-butter-frequency 60
    "How frequently to scroll.
Values between 32 and your refresh rate are meaningful."
    :group gptel-butter
    :type number)

  (defcustom gptel-butter-lines (12)
    "How frequently to scroll.
Set to your preferred."
    :group gptel-butter
    :type integer)

  (defcustom gptel-butter-constant 2.0
    "Controls acceleration and deceleration.
Values should be larger than 1.0.  Extremely high values will just
result in very jerky movements but possibly unstable math.  Values close
to 1.0 will be ignored because there is a minimum that allows solutions
to be found."
    :group gptel-butter
    :type float)

  (defun gptel-butter-scroll ( fsm start-time start-time last-time
                               last-window-start velocity acceleration)
    "Scroll as needed to continue pursuing a solution."
    (let* ((b (marker-buffer marker))
           (p (marker-position marker))
           (w (get-buffer-window buffer))
           (info (gptel-fsm-info fsm))
           (tracking (plist-get info :tracking)))
      (if (or (null w)
              (not (equal last-window-start (window-start)))
              (eq (gptel-fsm-state fsm) 'DONE)
              (and tracking (< tracking (window-start))))
          '()       ; decline recursion
        ;; Keep scrolling
        (run-at-time (/ 1.0 gpte-butter-frequency) nil
                     #'gptel-butter-scroll
                     fsm start-time time
                     (window-start w)
                     velocity acceleration))))

  ;;   (defun pmx--gptel-butter-chase-input ()
  ;;     "Update the scrolling based on how fare we are behind our goal.
  ;; As if chasing a stick of butter, we do not want to under or overshoot
  ;; too much."
  ;;     (when-let* ((tracking (plist-get info :tracking-marker))
  ;;                 (buffer (marker-buffer tracking))
  ;;                 (window (get-buffer-window buffer)))
  ;;       (with-current-buffer (marker-buffer tracking)
  ;;         (with-selected-window window
  ;;           (let ((end-line (line-number-at-pos (marker-position tracking)))
  ;;                 (window-end-line (line-number-at-pos (window-end window))))
  ;;             (ignore-error 'end-of-buffer
  ;;               (scroll-up-line (+ 2 (- end-line window-end-line)))))))))

  (add-to-hook 'gptel-post-stream-hook #'pmx--gptel-butter-chase-input)

  ;; (defun pmx--gptel-post-insert (beg end)
  ;;   (when (> (point-max) (window-end))
  ;;     (scroll-up-line 2))
  ;;   (goto-char end))

  ;; (add-to-list 'gptel-post-response-functions #'pmx--gptel-post-insert)
  ;; (remove-hook 'gptel-post-response-functions #'pmx--gptel-post-insert)
p
  ;; (defun pmx--gptel-after-insert-scroll (response info &rest _args)
  ;;   (when-let* ((tracking (plist-get info :tracking-marker))
  ;;               (buffer (marker-buffer tracking))
  ;;               (window (get-buffer-window buffer)))
  ;;     (with-current-buffer (marker-buffer tracking)
  ;;       (with-selected-window window
  ;;         (when (> (marker-position tracking) (window-end window))
  ;;           (let ((end-line (line-number-at-pos (marker-position tracking)))
  ;;                 (window-end-line (line-number-at-pos (window-end window))))
  ;;             (ignore-error 'end-of-buffer
  ;;               (scroll-up-line (+ 2 ( - end-line window-end-line))))))))))

  ;; TODO post-stream-hook
  ;; pixel-precision
  ;; (advice-add #'gptel--insert-response :after #'pmx--gptel-after-insert-scroll)
  ;; (advice-add #'gptel-curl--stream-insert-response :after
  ;;             #'pmx--gptel-after-insert-scroll)

  ;; (advice-remove #'gptel--insert-response #'pmx--gptel-after-insert-scroll)
  ;; (advice-remove #'gptel-curl--stream-insert-response #'pmx--gptel-after-insert-scroll)

  (defun pmx--gptel-remove-empty-line (_beg end)
    (save-excursion
      (goto-char end)
      (previous-line)
      (when (looking-at "^$")
        (delete-char -1 ))))

  (defun pmx--re-adapt-prefixes (_beg _end)
    (visual-wrap-prefix-mode -1)
    (visual-wrap-prefix-mode 1))

  (add-hook 'gptel-post-response-functions #'pmx--gptel-goto-response-end)
  (add-hook 'gptel-post-response-functions #'pmx--gptel-remove-empty-line)
  (add-hook 'gptel-post-response-functions #'pmx--re-adapt-prefixes)

  (setopt gptel-response-separator "\n")
  (setopt gptel-confirm-tool-calls t)
  (setopt gptel-use-tools t)
  (setopt gptel-display-buffer-action '(display-buffer-in-previous-window))
  ;; Brancing context is potentially neat, but the LLM output can break this
  ;; setting
  (setopt gptel-org-branching-context nil)
  (setopt gptel-expert-commands t)

  (setopt gptel-prompt-prefix-alist
          '((markdown-mode . "")
            (org-mode . "** ")
            (text-mode . "")))
  (setopt gptel-response-prefix-alist
          '((markdown-mode . "")
            (org-mode . "")
            (text-mode . "")))

  (setopt gptel-default-mode 'org-mode))

(provide 'posimacs-ai)
;;; posimacs-ai.el ends here

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp)
;; eval: (jinx-mode -1)
;; End:
