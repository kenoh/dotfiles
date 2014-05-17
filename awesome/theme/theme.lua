-------------------------------
--  "Zenburn" awesome theme  --
--    By Adrian C. (anrxc)   --
-------------------------------

themeFolder = "/home/kenoh/.config/awesome/theme"


-- {{{ Solarized colors
solcol = {}
solcol.base03    = "#002b36"
solcol.base02    = "#073642"
solcol.base01    = "#586e75"
solcol.base00    = "#657b83"
solcol.base0     = "#839496"
solcol.base1     = "#93a1a1"
solcol.base2     = "#eee8d5"
solcol.base3     = "#fdf6e3"
solcol.yellow    = "#b58900"
solcol.orange    = "#cb4b16"
solcol.red       = "#dc322f"
solcol.magenta   = "#d33682"
solcol.violet    = "#6c71c4"
solcol.blue      = "#268bd2"
solcol.cyan      = "#2aa198"
solcol.green     = "#859900"
-- }}}


-- Alternative icon sets and widget icons:
--  * http://awesome.naquadah.org/wiki/Nice_Icons

-- {{{ Main
theme = {}
--theme.wallpaper_cmd = { "awsetbg /usr/share/awesome/themes/zenburn/zenburn-background.png" }
-- }}}

-- {{{ Styles
theme.font      = "Terminus 8"

-- {{{ Colors
theme.fg_normal = "#999999"
theme.fg_focus  = "#111111"
theme.fg_urgent = "#ffffff"
theme.bg_normal = "#111111"
theme.bg_focus  = "#009900"
theme.bg_urgent = "#ff0000"
-- }}}

-- {{{ Borders
theme.border_width  = "2"
theme.border_normal = theme.bg_normal
theme.border_focus  = theme.bg_focus
theme.border_marked = theme.bg_urgent
-- }}}

-- {{{ Titlebars
theme.titlebar_bg_focus  = theme.border_focus
theme.titlebar_bg_normal = theme.border_normal
theme.titlebar_fg_focus  = theme.fg_focus
-- }}}

-- {{{ Others
theme.taglist_bg_focus = theme.bg_normal
theme.taglist_bg_urgent = theme.bg_urgent
theme.taglist_fg_focus = theme.bg_focus
theme.tasklist_bg_focus = theme.bg_normal
theme.tasklist_bg_urgent = theme.border_marked
theme.tasklist_fg_focus = theme.bg_focus
-- }}}

-- There are other variable sets
-- overriding the default one when
-- defined, the sets are:
-- [taglist|tasklist]_[bg|fg]_[focus|urgent]
-- titlebar_[normal|focus]
-- tooltip_[font|opacity|fg_color|bg_color|border_width|border_color]
-- Example:
--theme.taglist_bg_focus = "#CC9393"
-- }}}

-- {{{ Widgets
-- You can add as many variables as
-- you wish and access them by using
-- beautiful.variable in your rc.lua
theme.fg_widget        = "#aaaaaa"
--theme.fg_center_widget = "#88A175"
--theme.fg_end_widget    = "#FF5656"
--theme.bg_widget        = "#494B4F"
--theme.border_widget    = "#3F3F3F"
-- }}}

-- {{{ Mouse finder
theme.mouse_finder_color = "#CC9393"
-- mouse_finder_[timeout|animate_timeout|radius|factor]
-- }}}

-- {{{ Menu
-- Variables set for theming the menu:
-- menu_[bg|fg]_[normal|focus]
-- menu_[border_color|border_width]
theme.menu_height = "20"
theme.menu_width  = "192"
-- }}}

-- {{{ Icons
-- {{{ Taglist
--theme.taglist_squares_sel   = themeFolder .. "/taglist/squarefz.png"
--theme.taglist_squares_unsel = themeFolder .. "/taglist/squarez.png"
theme.taglist_squares_resize = false
-- }}}

-- {{{ Misc
theme.awesome_icon           = themeFolder .. "/awesome-icon.png"
theme.menu_submenu_icon      = "/usr/share/awesome/themes/default/submenu.png"
theme.tasklist_floating_icon = "/usr/share/awesome/themes/default/tasklist/floatingw.png"
-- }}}

-- {{{ Layout
theme.layout_tile       = themeFolder .. "/layouts/tile.png"
theme.layout_tileleft   = themeFolder .. "/layouts/tileleft.png"
theme.layout_tilebottom = themeFolder .. "/layouts/tilebottom.png"
theme.layout_tiletop    = themeFolder .. "/layouts/tiletop.png"
theme.layout_fairv      = themeFolder .. "/layouts/fairv.png"
theme.layout_fairh      = themeFolder .. "/layouts/fairh.png"
theme.layout_spiral     = themeFolder .. "/layouts/spiral.png"
theme.layout_dwindle    = themeFolder .. "/layouts/dwindle.png"
theme.layout_max        = themeFolder .. "/layouts/max.png"
theme.layout_fullscreen = themeFolder .. "/layouts/fullscreen.png"
theme.layout_magnifier  = themeFolder .. "/layouts/magnifier.png"
theme.layout_floating   = themeFolder .. "/layouts/floating.png"
-- }}}

-- {{{ Titlebar
theme.titlebar_close_button_focus  = themeFolder .. "/titlebar/close_focus.png"
theme.titlebar_close_button_normal = themeFolder .. "/titlebar/close_normal.png"

theme.titlebar_ontop_button_focus_active  = themeFolder .. "/titlebar/ontop_focus_active.png"
theme.titlebar_ontop_button_normal_active = themeFolder .. "/titlebar/ontop_normal_active.png"
theme.titlebar_ontop_button_focus_inactive  = themeFolder .. "/titlebar/ontop_focus_inactive.png"
theme.titlebar_ontop_button_normal_inactive = themeFolder .. "/titlebar/ontop_normal_inactive.png"

theme.titlebar_sticky_button_focus_active  = themeFolder .. "/titlebar/sticky_focus_active.png"
theme.titlebar_sticky_button_normal_active = themeFolder .. "/titlebar/sticky_normal_active.png"
theme.titlebar_sticky_button_focus_inactive  = themeFolder .. "/titlebar/sticky_focus_inactive.png"
theme.titlebar_sticky_button_normal_inactive = themeFolder .. "/titlebar/sticky_normal_inactive.png"

theme.titlebar_floating_button_focus_active  = themeFolder .. "/titlebar/floating_focus_active.png"
theme.titlebar_floating_button_normal_active = themeFolder .. "/titlebar/floating_normal_active.png"
theme.titlebar_floating_button_focus_inactive  = themeFolder .. "/titlebar/floating_focus_inactive.png"
theme.titlebar_floating_button_normal_inactive = themeFolder .. "/titlebar/floating_normal_inactive.png"

theme.titlebar_maximized_button_focus_active  = themeFolder .. "/titlebar/maximized_focus_active.png"
theme.titlebar_maximized_button_normal_active = themeFolder .. "/titlebar/maximized_normal_active.png"
theme.titlebar_maximized_button_focus_inactive  = themeFolder .. "/titlebar/maximized_focus_inactive.png"
theme.titlebar_maximized_button_normal_inactive = themeFolder .. "/titlebar/maximized_normal_inactive.png"
-- }}}
-- }}}

return theme
