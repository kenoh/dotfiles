import i3ipc
from pprint import pprint

last = None

def on_window_focus(i3, e):
    global last
    con = e.container
    if con.floating:
        return
    if last:
        last.command('mark prev')
    last = con
    # pprint({x: getattr(con, x) for x in dir(con)})
    # pprint(con.floating)


if __name__ == '__main__':
    i3 = i3ipc.Connection()
    i3.on('window::focus', on_window_focus)
    i3.main()
