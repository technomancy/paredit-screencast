# Paredit - Deftness with Parentheses

## Intro

Some languages, most famously Lisp, require heavy use of
parentheses. A good editor will provide a few tools to help you with
this. A great editor (such as Emacs with Paredit installed) will go
further, which is what we'll explore here.

## Insertion and Deletion

Let's write some Emacs Lisp. Switching to the \*scratch\* buffer gives
us a sandbox in which to play around.

    M-x paredit-mode
    (defun plus (x y) (+ x y))

Now you'll notice that as you type open parens, the closing ones are
inserted for you. This is no real surprise, as it's something many
other editors provide. But we're just getting started.

    Move to right before (+, C-k

The next thing you'll notice is that deleting works differently. When
you press C-k to kill a line, the whole line doesn't get
deleted. Paredit is doing its best to make sure that the structure of
your code remains valid. It knows you probably didn't want to actually
kill the whole line, just everything up to the closing paren.

    Press backspace to delete x and y.

Similarly, pressing backspace will pass through the parens and only
delete x and y.

    Press backspace again to get rid of the paren.

But once a pair of parens is empty, then deleting one of them deletes
the other.

    M-b to the front of plus, hit M-(

Meta-paren will wrap the expression at point in parentheses.

    Press )

And pressing close paren won't insert one, but just jumps to the close
of the current expression instead.

    Press SPC, "

So far everything that works with parentheses also applies to other
matched characters. Double-quotes, square brackets, and curly braces
(if your language uses them) all behave similarly.

Of course, paredit knows that these rules don't apply when you're
inside a string or a comment, so it doesn't try to enforce its
structure there.

## Workarounds

Now these features are helpful, but they're not perfect. Let's see
what happens when the rules are violated.

    Open file invalid.el

The first thing to note is that paredit won't activate if it detects
unbalanced characters. So let's fix it and activate paredit manually.

    Insert paren in front of message
    M-x paredit

You can still get a document in a bad state if you don't watch
out. Killing a region with C-w does not enforce the rules.

    Mark (message and C-w it
    C-e

See how the end of the line is highlighted differently? That's an
indicator that things are unbalanced.

One thing that's helpful to remember is that Emacs lets you prefix a
letter with C-q to insert it literally rather than activating whatever
that key is bound to. Use this if you need to insert a paren or two to
fix your code.

## Navigation

## Wrangling (depth-changing)

## Other Languages

While paredit-mode was designed to work with Lisp languages, it can be
used in others as well. In Ruby it works pretty well, although it does
not consider do/end to be matching elements. For Javascript and other
languages based on cc-mode, there are a few hiccups, but it can be
made to work.

## Installation and Enabling

If you use the Emacs Starter Kit, you've got Paredit already
installed. Otherwise hit up the Emacs Lisp Package Archive, or ELPA
for a copy. ELPA can be downloaded from http://tromey.com/elpa.

[Show installation]

You'll still need to choose which modes to enable it for though. Add
hooks for that:

  (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
  (add-hook 'scheme-mode-hook     'enable-paredit-mode)
  (add-hook 'clojure-mode-hook    'enable-paredit-mode)
  (add-hook 'ruby-mode-hook       'enable-paredit-mode)

Add a hook for each mode for which you want paredit activated, and
you're good to go.

If you're interested in learning more about Emacs or Lisp, check out
my PeepCode screencasts, each available for $9:

Meet Emacs - http://peepcode.com/products/meet-emacs
Functional Programming with Clojure - http://peepcode.com/products/functional-programming-with-clojure

Thanks for watching!
