# Autogenerated config.py
# Documentation:
#   qute://help/configuring.html
#   qute://help/settings.html

# Uncomment this to still load settings configured via autoconfig.yml
# config.load_autoconfig()

# User agent to send.  The following placeholders are defined:  *
# `{os_info}`: Something like "X11; Linux x86_64". * `{webkit_version}`:
# The underlying WebKit version (set to a fixed value   with
# QtWebEngine). * `{qt_key}`: "Qt" for QtWebKit, "QtWebEngine" for
# QtWebEngine. * `{qt_version}`: The underlying Qt version. *
# `{upstream_browser_key}`: "Version" for QtWebKit, "Chrome" for
# QtWebEngine. * `{upstream_browser_version}`: The corresponding
# Safari/Chrome version. * `{qutebrowser_version}`: The currently
# running qutebrowser version.  The default value is equal to the
# unchanged user agent of QtWebKit/QtWebEngine.  Note that the value
# read from JavaScript is always the global value.
# Type: FormatString
config.set('content.headers.user_agent', 'Mozilla/5.0 ({os_info}; rv:71.0) Gecko/20100101 Firefox/71.0', 'https://*.google.com/*')

# Load images automatically in web pages.
# Type: Bool
config.set('content.images', True, 'chrome-devtools://*')

# Load images automatically in web pages.
# Type: Bool
config.set('content.images', True, 'devtools://*')

# Enable JavaScript.
# Type: Bool
config.set('content.javascript.enabled', True, 'chrome-devtools://*')

# Enable JavaScript.
# Type: Bool
config.set('content.javascript.enabled', True, 'devtools://*')

# Enable JavaScript.
# Type: Bool
config.set('content.javascript.enabled', True, 'chrome://*/*')

# Enable JavaScript.
# Type: Bool
config.set('content.javascript.enabled', True, 'qute://*/*')

# CSS border value for hints.
# Type: String
c.hints.border = '2px solid #2a212a'

# Make characters in hint strings uppercase.
# Type: Bool
c.hints.uppercase = False

# Padding (in pixels) around text for tabs.
# Type: Padding
c.tabs.padding = {'bottom': 2, 'left': 5, 'right': 5, 'top': 2}

# Search engines which can be used via the address bar. Maps a search
# engine name (such as `DEFAULT`, or `ddg`) to a URL with a `{}`
# placeholder. The placeholder will be replaced by the search term, use
# `{{` and `}}` for literal `{`/`}` signs. The search engine named
# `DEFAULT` is used when `url.auto_search` is turned on and something
# else than a URL was entered to be opened. Other search engines can be
# used by prepending the search engine name to the search term, e.g.
# `:open google qutebrowser`.
# Type: Dict
c.url.searchengines = {'DEFAULT': 'https://duckduckgo.com/?q={}', 'yt': 'https://www.youtube.com/results?search_query={}', 'aw': 'https://wiki.archlinux.org/index.php/{}', 'apkg': 'https://www.archlinux.org/packages/?sort=&q={}&maintainer=&flagged=', 'aur': 'https://aur.archlinux.org/packages/?O=0&K={}', 'google': 'https://www.google.com/search?q={}'}

text_color = '#bababa'
bg_color0 = '#1a111a'
bg_color1 = '#2a212a'
bg_color2 = '#3a313a'
bg_color3 = '#4a313a'
bg_color4 = '#1c1c1c'
fg_color0 = '#f799d7'
fg_color1 = '#d75f5f'

# Text color of the completion widget. May be a single color to use for
# all columns or a list of three colors, one for each column.
# Type: List of QtColor, or QtColor
c.colors.completion.fg = text_color

# Background color of the completion widget for odd rows.
# Type: QssColor
c.colors.completion.odd.bg = bg_color1

# Background color of the completion widget for even rows.
# Type: QssColor
c.colors.completion.even.bg = bg_color1

# Foreground color of completion widget category headers.
# Type: QtColor
c.colors.completion.category.fg = fg_color0

# Background color of the completion widget category headers.
# Type: QssColor
c.colors.completion.category.bg = bg_color2

# Top border color of the completion widget category headers.
# Type: QssColor
c.colors.completion.category.border.top = bg_color1

# Bottom border color of the completion widget category headers.
# Type: QssColor
c.colors.completion.category.border.bottom = bg_color1

# Foreground color of the selected completion item.
# Type: QtColor
c.colors.completion.item.selected.fg = fg_color0

# Background color of the selected completion item.
# Type: QssColor
c.colors.completion.item.selected.bg = bg_color2

# Top border color of the selected completion item.
# Type: QssColor
c.colors.completion.item.selected.border.top = bg_color1

# Bottom border color of the selected completion item.
# Type: QssColor
c.colors.completion.item.selected.border.bottom = bg_color1

# Foreground color of the matched text in the selected completion item.
# Type: QtColor
c.colors.completion.item.selected.match.fg = fg_color0

# Color of the scrollbar handle in the completion view.
# Type: QssColor
c.colors.completion.scrollbar.fg = fg_color0

# Color of the scrollbar in the completion view.
# Type: QssColor
c.colors.completion.scrollbar.bg = bg_color0

# Font color for hints.
# Type: QssColor
c.colors.hints.fg = text_color

# Background color for hints. Note that you can use a `rgba(...)` value
# for transparency.
# Type: QssColor
c.colors.hints.bg = bg_color1

# Font color for the matched part of hints.
# Type: QtColor
c.colors.hints.match.fg = fg_color0

# Background color of the statusbar.
# Type: QssColor
c.colors.statusbar.normal.bg = bg_color0

# Background color of the statusbar in insert mode.
# Type: QssColor
c.colors.statusbar.insert.bg = bg_color2

# Foreground color of the statusbar in private browsing mode.
# Type: QssColor
c.colors.statusbar.private.fg = text_color

# Background color of the statusbar in private browsing mode.
# Type: QssColor
c.colors.statusbar.private.bg = bg_color0

# Foreground color of the statusbar in command mode.
# Type: QssColor
c.colors.statusbar.command.fg = text_color

# Background color of the statusbar in command mode.
# Type: QssColor
c.colors.statusbar.command.bg = bg_color0

# Foreground color of the statusbar in private browsing + command mode.
# Type: QssColor
c.colors.statusbar.command.private.fg = text_color

# Background color of the statusbar in private browsing + command mode.
# Type: QssColor
c.colors.statusbar.command.private.bg = bg_color0

# Foreground color of the statusbar in caret mode.
# Type: QssColor
c.colors.statusbar.caret.fg = text_color

# Background color of the statusbar in caret mode.
# Type: QssColor
c.colors.statusbar.caret.bg = bg_color2

# Background color of the progress bar.
# Type: QssColor
c.colors.statusbar.progress.bg = fg_color1

# Default foreground color of the URL in the statusbar.
# Type: QssColor
c.colors.statusbar.url.fg = fg_color1

# Foreground color of the URL in the statusbar on error.
# Type: QssColor
c.colors.statusbar.url.error.fg = fg_color1

# Foreground color of the URL in the statusbar for hovered links.
# Type: QssColor
c.colors.statusbar.url.hover.fg = fg_color0

# Foreground color of the URL in the statusbar on successful load
# (http).
# Type: QssColor
c.colors.statusbar.url.success.http.fg = fg_color0

# Foreground color of the URL in the statusbar on successful load
# (https).
# Type: QssColor
c.colors.statusbar.url.success.https.fg = fg_color0

# Background color of the tab bar.
# Type: QssColor
c.colors.tabs.bar.bg = bg_color4

# Foreground color of unselected odd tabs.
# Type: QtColor
c.colors.tabs.odd.fg = text_color

# Background color of unselected odd tabs.
# Type: QtColor
c.colors.tabs.odd.bg = bg_color1

# Foreground color of unselected even tabs.
# Type: QtColor
c.colors.tabs.even.fg = text_color

# Background color of unselected even tabs.
# Type: QtColor
c.colors.tabs.even.bg = bg_color1

# Foreground color of selected odd tabs.
# Type: QtColor
c.colors.tabs.selected.odd.fg = fg_color0

# Background color of selected odd tabs.
# Type: QtColor
c.colors.tabs.selected.odd.bg = bg_color2

# Foreground color of selected even tabs.
# Type: QtColor
c.colors.tabs.selected.even.fg = fg_color0

# Background color of selected even tabs.
# Type: QtColor
c.colors.tabs.selected.even.bg = bg_color2

# Font used for the hints.
# Type: Font
c.fonts.hints = 'bold 13pt default_family'

# Font used in the statusbar.
# Type: Font
c.fonts.statusbar = '11pt default_family'

# Font used in the tab bar.
# Type: QtFont
c.fonts.tabs = '11pt default_family'