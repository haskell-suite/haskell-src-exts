# Contributing to haskell-src-exts

## Filing an issue

Thanks for reporting an issue with haskell-src-exts. Here's what you
should expect in response:

### Build failure

If there's a trivial build failure (e.g. due to a new version of a
dependency), it should be fixed ASAP — ideally within a day.

Even if the required change is trivial, if you've made and tested it, don't
hesitate to submit a pull request.

If there's a delay, please contact [Roman Cheplyaka][rc] and [Adam
Bergmark][ab].

[rc]: https://github.com/feuerbach
[ab]: https://github.com/bergmark

### Semantic bug

Most often reported issues are parser bugs and unsupported extensions.
Unfortunately, at the moment haskell-src-exts lacks full-time developers who
would be responsible for fixing the bugs and implementing extensions.

Luckily, there are a number of enthusiasts who regularly contribute to
haskell-src-exts, so there's a chance that your issue will be fixed sooner or
later. But ultimately the best way to ensure that the problem is fixed is to fix
it yourself.

## Contributing a patch

### Bug fix or new extension

So, you've fixed a bug or implemented an extension. Awesome!

We strive to get every such pull request reviewed and merged within a month.

For best results, please follow these guidelines:

1.  Send patches via github pull requests
1.  Each pull request should achieve a specific goal and have a descriptive
    title. Don't put multiple unrelated changes in a single pull request. (An
    exception can be made for multiple very simple self-contained commits, or for
    changes with a hard dependency on each other.)
1.  Typically, a pull request should consist of just a few commits. Rewrite the
    history (see `git rebase`) to make commits logical, not historical.
1.  Write descriptive commit messages. Here's an [example of a good commit
    message][commitmsg]
1.  If you want to amend a pull request, rewrite your branch and leave a
    comment. Do not add commits to the branch or open new pull requests for that.
1.  Make sure the tests pass:

    ```
    cabal configure --enable-tests
    cabal build
    dist/build/test/test --hide-successes
    ```

    If there are expected discrepancies, run `dist/build/test/test --accept`, then
    commit.

    See `dist/build/test/test --help` and
    [tasty docs](http://documentup.com/feuerbach/tasty) for more options.
1.  Unless the change is trivial, it should be accompanied by the tests that show
    the effects of the change. To add a test, put a Haskell source file under
    `tests/examples`, then run `dist/build/test/test --accept`. Use the
    `--pattern NAME` option to run only these new tests.

    Tests should be added in a commit separate from the code changes.
1.  Build with `cabal build --ghc-options=-Wall` and check that your changes
    don't generate new warnings.
1.  Run `git diff --check` on your changes to check that you aren't committing any
    sinful whitespace.

[commitmsg]: https://github.com/haskell-suite/haskell-src-exts/commit/ed3e51aaa8c05d0ebc7c26b5ff70ff0dfce4af11

### Refactoring or architectural changes

Such changes should usually go through [Niklas
Broberg](https://github.com/niklasbroberg). You may want to contact him
directly.

## Other ways to contribute

### Review pull requests

You can help by reviewing pull requests submitted by others. If you've found any
issues, raise them in the comments. If you've done a review and think that
everything is fine, please say so, too, — it does help!

### Fix warnings

Make haskell-src-exts build cleanly with `-Wall`.

### Fix tests

Some tests simply document the existing (but wrong) behavior.

Make sure that there is an existing issue in the tracker about such a
wrong behavior. Link to the issue in the test.

Then try to fix it :-)
