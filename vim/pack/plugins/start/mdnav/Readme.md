**NOTE: this project is no longer maintained, as I stopped using it. If you forked the project and continue to develop it, feel free to send me a message to include a link**.


# mdnav - vim plugin for navigating links in markdown files

Vim plugin for navigating links in markdown files.
It can handle:

- **local text links**:
    `[foo](second.md)` will be opened inside vim.
    If the target contains line number as in `[foo](second.md:30)`, the line
    will be jumped to.
    Also anchors are supported, for example `[foo](second.md#custom-id)`.
- **URL links**:
    `[google](https://google.com)` will be opened with the OS browser.
- **non text files**:
    if the option `g:mdnav#Extensions` is set, non text files will be opened
    via the operating system.
    This behavior is handy when linking to binary documents, for example PDFs.
- **internal links**:
    `[Link Text](#Target)`, will link to the heading `# Target`.
    Following the link will jump to the heading inside vim.
    Currently both github style anchors, all words lowercased and hyphenated,
    and jupyter style anchros, all words hyphenated, are supported.
- **reference style links**:
    for links of the form `[foo][label]`, mdnav will lookup the corresponding
    label and open the target referenced there.
    This mechanism works will all link targets.
- **implicit name links**:
    for links of the form `[foo][]` will use `foo` as the label and then follow
    the logic of reference style links.
- **custom ids via attribute lists**:
    the id a link target can be defined via [attribute lists][attr-lists] of
    the form `{: #someid ...}`.
    This way fixed name references can be defined to prevent links from going
    stale after headings have been changed.
- **local link format of pelican**:
    mdnav handles `|filename| ...` and `{filename} ...` links as expected, for
    example `[link](|filename|./second.md)` and
    `[link]({filename}../posts/second.md)`.

Note, all links above are functional with vim and mdnav installed.
While mdnav is inspired by [follow-markdown-links][fml], mdnav can handle many
more link formats and types of link targets (MD files, URLs, non text files,
...).

[label]: https://google.com
[foo]: https://wikipedia.org
[fml]: https://github.com/prashanthellina/follow-markdown-links
[attr-lists]: https://pythonhosted.org/Markdown/extensions/attr_list.html

## Installation

Install the plugin via your favorite plugin manager, say [Vundle][vundle]:

    Plugin 'chmp/mdnav'

To work, vim needs to be configured with python support.

## Usage

Inside normal model with an open markdown document, you may press enter on a
markdown link to open it.
If the link is a local file it will be opened in vim (`C-o` will get you back),
otherwise it will be opened by the current webbrowser.

The following links can be used (the possible cursor positions are indicated by
`^`):


    This [link](https://example.com) will be opened inside the browser.
         ^^^^^^^^^^^^^^^^^^^^^^^^^^^

    This [link](./foo.md) will open `./foo.md` inside vim.
         ^^^^^^^^^^^^^^^^

    This [link](|filename|./foo.md) will open `./foo.md` inside vim.
         ^^^^^^^^^^^^^^^^^^^^^^^^^^

    If `g:mdnav#Extensions` is set to `.md, .MD`, enter will open
    `example.pdf` inside the default PDF reader for this
    [link](|filename|./example.pdf).
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

    Internal linking works, too: to link to the heading Usage, use
    this [link](#usage).
         ^^^^^^^^^^^^^^

    Reference style [links][ref-style-link] work too.
                    ^^^^^^^^^^^^^^^^^^^^^^^

    [ref-style-link]: http://example.com


The behavior of mdnav can be configured via the following options:

- `g:mdnav#Extensions`:
    a comma separated list of file extensions.
    Only file s with the given extensions will be opened in vim, all other
    files will be opened via the configured application (using `open` on OSX
    and `xdg-open` on linux).
    This option may be useful to link to non-text documents, say PDF files.

- `g:mdnav#DebugMode`:
    if set to `true` it, extensive debug information will be logged.

## Running tests

	pip install -r test-requirements.txt
	python -m pytest tests

## License

>  The MIT License (MIT)
>  Copyright (c) 2017 Christopher Prohm
>
>  Permission is hereby granted, free of charge, to any person obtaining a copy
>  of this software and associated documentation files (the "Software"), to
>  deal in the Software without restriction, including without limitation the
>  rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
>  sell copies of the Software, and to permit persons to whom the Software is
>  furnished to do so, subject to the following conditions:
>
>  The above copyright notice and this permission notice shall be included in
>  all copies or substantial portions of the Software.
>
>  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
>  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
>  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
>  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
>  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
>  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
>  DEALINGS IN THE SOFTWARE.

[vundle]: https://github.com/VundleVim/Vundle.vim

