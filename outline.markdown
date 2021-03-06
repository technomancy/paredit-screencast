# paredit (parenthetical deftness)

## 1. Intro

Some languages, most famously Lisp, require heavy use of
parentheses. A good editor will provide a few tools to help you with
this. A great editor (such as Emacs with paredit installed) will go
further, which is what we'll explore here.

## 2. Insertion and Deletion

Let's write some Emacs Lisp.

    C-x C-f init.el
    M-x paredit-mode
    down to line 2
    insert: (setq paredit-awesome-p t)

Now you'll notice that as you type open parens, the closing ones are
inserted for you. This is no real surprise, as it's something many
other editors provide. But we're just getting started.

    point to right after paredit-
    C-k

The next thing you'll notice is that deleting works differently. When
you press C-k to kill a line, the whole line doesn't always get
deleted. Paredit is doing its best to make sure that the structure of
your code remains valid. It knows you probably didn't want to actually
kill the whole line, just everything up to the closing paren.

    insert: numbers (list 1 (+ 2 \n 3 4))
    point to before (+
    C-k

If the rest of the line contains an expression that spans many lines,
it will remove the whole thing instead of just up to the end of the
line.

    C-e
    backspace through 1 and list

Pressing backspace will pass through the parens and only delete elements.

    backspace again

But once a pair of parens is empty, then deleting one of them deletes
the other.

    M-b
    Press )

And pressing close paren won't insert one, but just jumps to the close
of the current expression instead.

    C-b
    insert: [1 2 3]

So far everything that works with parentheses also applies to other
matched characters. Double-quotes, square brackets, and curly braces
(if your language uses them) all behave similarly.

    C-e
    insert "hey look: [unmatched)!"

Of course, paredit knows that these rules don't apply when you're
inside a string or a comment, so it doesn't try to enforce its
structure there.

## 3. Workarounds

Now these features are helpful, but they assume that the file you're
working with has a valid structure. For various reasons, that's not
always true. Let's see what happens when the rules are violated.

    Open file invalid.el

The first thing to note is that paredit won't even activate if it
detects unbalanced characters in a file you're opening. So let's fix
it and activate paredit manually.

    Insert paren in front of message
    M-x paredit-mode

You can still get a document in a bad state if you don't watch
out. Killing a region with C-w does not enforce the rules, so remember
that when you use it, you're stepping outside the bounds of paredit
and should be a little more careful.

    Mark (message and C-w it
    C-e

See how the end of the line is highlighted differently? That's
show-paren-mode indicating that things are unbalanced. It's not part
of paredit, but it's definitely worth enabling.

Another thing that's helpful to remember is that Emacs lets you prefix
a key with C-q to insert it literally rather than activating whatever
the key is bound to. Use this if you need to insert a lone paren to
fix things. You can also prefix backspace with C-u to force it.

## 4. Wrangling (depth-changing)

    open rooms.clj
    point at right before mire.rooms
    M-(
    Mark "declare rooms"
    Press (

You can wrap the next expression in parens with M-(. If you want to
wrap multiple expressions, simply mark them and then hit (.

    Back to mire.rooms
    M-s

If you're inside a list and want to merge it with its parent, use M-s
to splice.

    Back to "declare rooms"; point in rooms
    C-S-]
    C-S-0

Of course we can't neglect to mention the imaginatively named "barf"
and "slurp" commands. If you're inside a list, you can "barf" the last
expression out of the list. The reverse operation "slurps" the next
element outside into the list. Yum!

    C-S-[
    C-S-9

Barfing and slurping have forward and backward variations.

    Point to front of rooms
    M-S-s
    M-S-j

This is pretty straightforward; just use M-S-s and M-S-j to split and
join lists.

## 5. Other Languages

    Open concourse.js
    M-x paredit-mode
    [1, 2, 3].map()
    Point over 2
    C-k
    insert: 4

While paredit-mode was designed to work with Lisp languages, it can be
used in others as well. It works with most modes based on cc-mode
(including espresso-mode for Javascript), but there are problems with
js2-mode. It also works in ruby-mode. The list modification commands
don't expect list elements to need commas between them, so this is not
ideal. Other than that, the functionality it provides is quite
helpful.

## 6. Installation and Enabling

If you use the Emacs Starter Kit, you've got Paredit already
installed. Otherwise hit up the Emacs Lisp Package Archive, or ELPA
for a copy. ELPA can be downloaded from http://tromey.com/elpa.

    [Show installation]

You'll still need to choose which modes to enable it for though. Add
hooks for that:

    (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
    (add-hook 'scheme-mode-hook     'enable-paredit-mode)
    (add-hook 'clojure-mode-hook    'enable-paredit-mode)
    (add-hook 'ruby-mode-hook       'esk-paredit-nonlisp)
    (add-hook 'espresso-mode-hook   'esk-paredit-nonlisp)

Add a hook for each mode for which you want paredit activated, and
you're good to go.

The esk-paredit-nonlisp function customizes and enables paredit for
non-Lisp languages. It's included in the Starter Kit, but if you want
to use it elsewhere, it looks like this:

    [TODO: This only works with my patched paredit! Get it upstream or
           rework this section.]

    (defun esk-paredit-nonlisp ()
      "Turn on paredit mode for non-lisps."
      (set (make-local-variable paredit-space-delimiter-chars) (list ?\"))
      (paredit-mode +1))

## 7. Conclusion

Hopefully now you've picked up some techniques that will make you more
effective in your coding.

If you're interested in learning more about Emacs or Lisp, check out
my PeepCode screencasts, each available for $9:

    [Show each URL in a browser.]

Meet Emacs - http://peepcode.com/products/meet-emacs
Functional Programming with Clojure - http://peepcode.com/products/functional-programming-with-clojure

Thanks for watching!
