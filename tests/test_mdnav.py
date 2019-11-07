import pytest
import mdnav

# NOTE: the cursor is indicated with ^, the cursor will be placed on the
# following character
parse_link_cases = [
    # default cases
    (['foo ^[bar](baz.md)'], 'baz.md'),
    (['foo [b^ar](baz.md)'], 'baz.md'),
    (['foo [b^ar](baz.md) [bar](bar.md)'], 'baz.md'),
    (['foo [b^ar][bar]', '[bar]: baz.md'], 'baz.md'),
    (['foo [b^ar][bar]', '[bar]: |filename|./baz.md'], '|filename|./baz.md'),
    (['foo [b^ar][bar] [bar][baz]', '[bar]: |filename|./baz.md'], '|filename|./baz.md'),

    # empty link target
    (['foo [b^ar][]', '[bar]: baz.md'], 'baz.md'),
    (['foo [@b^ar][]', '[@bar]: baz.md'], 'baz.md'),
    (['foo [^@bar][]', '[@bar]: baz.md'], 'baz.md'),

    # cursor outside link area
    (['foo^  [bar](baz.md) '], None),
    (['foo ^ [bar](baz.md) '], None),
    (['foo [bar](baz.md) ^ '], None),
    (['foo [bar](baz.md)^  '], None),

    # cursor inside target part
    (['foo [bar][b^ar]', '[bar]: baz.md'], 'baz.md'),
    (['foo [bar](b^az.md) [bar](bar.md)'], 'baz.md'),

    # malformed links
    (['][b^ar](bar.md)'], 'bar.md'),

    # empty line
    (['^'], None),

    # multiple [] pairs
    (["- [ ] checkout [la^bel][target] abs", "[target]: example.com"], "example.com"),
    (["- [ ] checkout [label]^[target] abs", "[target]: example.com"], "example.com"),
    (["- [ ] checkout [label][tar^get] abs", "[target]: example.com"], "example.com"),

    # reference definitions
    (["[f^oo]: test.md"], "test.md"),
    (["[foo]: test.md^"], "test.md"),
    (["[foo]: ^test.md"], "test.md"),
    (["^[foo]: test.md"], "test.md"),
]


@pytest.mark.parametrize('lines, expected', parse_link_cases)
def test_parse_link(lines, expected):
    cursor, mod_lines = _find_cursor(lines)
    actual = mdnav.parse_link(cursor, mod_lines)
    assert actual == expected


def _find_cursor(lines):
    lines_without_cursor = []
    cursor = None

    for (row, line) in enumerate(lines):
        pos = line.find('^')

        if pos < 0:
            lines_without_cursor.append(line)

        else:
            cursor = (row, pos)
            lines_without_cursor.append(line[:pos] + line[pos + 1:])

    return cursor, lines_without_cursor


open_link_cases = [
    (None, {}, mdnav.NoOp(None)),
    ('baz.md', {}, mdnav.VimOpen('/abs/baz.md')),
    ('baz.md:20', {}, mdnav.VimOpen('/abs/baz.md:20')),
    ('baz.MD', {'open_in_vim_extensions': ['.md']}, mdnav.OSOpen('/abs/baz.MD')),
    ('baz.md', {'open_in_vim_extensions': ['.md']}, mdnav.VimOpen('/abs/baz.md')),
    ('baz.md:20', {'open_in_vim_extensions': ['.md']}, mdnav.VimOpen('/abs/baz.md:20')),
    ('|filename|/foo/baz.md', {}, mdnav.VimOpen('/foo/baz.md')),
    ('/foo/bar.md', {}, mdnav.VimOpen('/foo/bar.md')),
    ('http://example.com', {}, mdnav.BrowserOpen('http://example.com')),
    ('http://example.com', {'open_in_vim_extensions': ['.md']}, mdnav.BrowserOpen('http://example.com')),
]


@pytest.mark.parametrize('target, open_link_kwargs, expected', open_link_cases)
def test_open_link(target, open_link_kwargs, expected):
    open_link_kwargs.setdefault('current_file', '/abs/foo.md')
    actual = mdnav.open_link(target, **open_link_kwargs)

    assert actual == expected


jump_to_anchor_cases = [
    ('#foo', ['a', '# foo', 'b'], 1),
    ('#foo-bar-baz', ['a', '#  Foo  BAR  Baz', 'b'], 1),
    ('#foo', ['a', '#  Bar', 'b'], None),
    ('#Foo-Bar-Baz', ['a', '### Foo Bar Baz', 'b'], 1),

    # use attr-lists to define custom ids
    ('#hello-world', ['a', '### Foo Bar Baz {: #hello-world } ', 'b'], 1),

    # first match wins
    ('#hello-world', ['# hello world', '### Foo Bar Baz {: #hello-world } ', 'b'], 0),
]


@pytest.mark.parametrize('target, buffer, expected', jump_to_anchor_cases)
def test_jump_to_anchor(target, buffer, expected):
    actual = mdnav.JumpToAnchor.find_anchor(target, buffer)
    assert actual == expected


@pytest.mark.parametrize('path, expected_path, expected_line, expected_anchor', [
    ('foo.md', 'foo.md', None, None),
    ('foo:bar.md', 'foo:bar.md', None, None),
    ('foo.md:30', 'foo.md', '30', None),
    ('foo.md#hello-world', 'foo.md', None, 'hello-world'),
    ('foo.md#happy:)', 'foo.md', None, 'happy:)'),
])
def test_parse_path(path, expected_path, expected_line, expected_anchor):
    path = mdnav.parse_path(path)

    assert path.path == expected_path
    assert path.line == expected_line
    assert path.anchor == expected_anchor

