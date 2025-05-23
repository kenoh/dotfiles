#!/usr/bin/python3
import argparse
import subprocess as sp
import sys
import time
from abc import ABC
from dataclasses import dataclass
from datetime import datetime

import evdev as ed


def log(*args):
    if VERB >= 1:
        print(datetime.now(), *args, file=sys.stderr)

def list_devs():
    return (ed.InputDevice(p) for p in ed.list_devices())

def get_lid_dev(name):
    ds = list_devs()
    for d in ds:
        if d.name == name:
            return d
    raise RuntimeError('File not matched')

def ptick(char):
    if VERB >= 2:
        print(char, end='')
        sys.stdout.flush()

class Loop(ABC):
    """Inherit this class, adding `proc_`-prefixed methods to loop over. Once all these methods return `None`, wait for given amount and start over."""
    _prefix = "proc_"
    def loop(self, block_time_s: float):
        funcs = [getattr(self, n) for n in dir(self) if n.startswith(self._prefix)]
        if funcs == []:
            raise ValueError("Provide at least one func!")
        rets = [None for _ in funcs]
        while True:
            ptick('@')
            rets = [f() for f in funcs]
            if any(rets):
                ptick('.')
                continue
            ptick('-')
            time.sleep(block_time_s)


@dataclass
class MyLoop(Loop):
    dt: float
    dev_name: str
    cmd: str
    def __post_init__(self):
        self.dev = get_lid_dev(self.dev_name)
        caps = self.dev.capabilities(True)
        assert ('EV_SW', 5) in caps
        assert caps[('EV_SW', 5)] == [('SW_LID', 0)]
        self.last_close = None
        self.last_open = None

    def proc_evdev(self):
        while e:=self.dev.read_one():
            if e.type != ed.ecodes.ecodes['EV_SW']:
                continue
            t = e.sec * 1_000_000_000 + e.usec * 1_000
            assert abs(time.time_ns() - t) < 1e9 # sanity
            log('e.value', e.value)
            match e.value:
                case 0:
                    self.last_open = t
                    self.last_close = self.last_close or self.last_open
                case 1:
                    self.last_close = t
                    self.last_open = self.last_open or self.last_close
                case _:
                    raise ValueError('Unexpected value')


    def proc_lock(self):
        if any([self.last_close, self.last_open]):
            if self.last_close > self.last_open and time.time_ns() - self.last_close > self.dt * 1_000_000_000:
                self.last_open = self.last_close = None
                log("locking..")
                sp.run(self.cmd,
                       shell=True,
                       stdout=sys.stdout,
                       stderr=sys.stderr)
                # # now flush the evdev
                # log('flushing')
                # self.proc_evdev()
                # self.last_open = self.last_close = None
                return 1


def main(dt, dev_name, cmd):
    MyLoop(dt, dev_name, cmd).loop(0.1)


if __name__ == '__main__':
    ap = argparse.ArgumentParser()
    ap.add_argument('cmd')
    ap.add_argument('--dt', default=2)
    ap.add_argument('-v', action='store_true')
    ap.add_argument('-vv', action='store_true', help='verbose with ticks')
    a = ap.parse_args()
    global VERB
    VERB = 2 if a.vv else 1 if a.v else 0
    main(dt=a.dt, dev_name='Lid Switch',
         cmd=a.cmd)
         # cmd=['bash', os.path.expanduser("~/bin/lock")])
    list_devs()
