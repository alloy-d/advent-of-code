#!/usr/bin/env python3

import tkinter
import re
from sys import stdin
from time import sleep

class Star:
    pattern = r"position=<\s*(-?\d+),\s*(-?\d+)> velocity=<\s*(-?\d+),\s*(-?\d+)>"
    def __init__(self, string):
        match = re.search(self.pattern, string)

        self.x = int(match.group(1))
        self.y = int(match.group(2))

        self.dx = int(match.group(3))
        self.dy = int(match.group(4))

    def __repr__(self):
        return f"star {self.x}+({self.dx}) {self.y}+({self.dy})"

    def position_at(self, time):
        return (self.x + time * self.dx, self.y + time * self.dy)

def visualize(stars):
    top = tkinter.Tk()
    time = 10238
    scale = 2.5 

    C = tkinter.Canvas(top, bg="#00061F", confine=False, height=600, width=800)
    C.pack()

    star_ids = {}
    for star in stars:
        (x, y) = (star.x * scale, star.y * scale)
        star_ids[star] = C.create_oval(x, y, x+2, y+2, fill="white")

    time_id = C.create_text(30, 580, fill="white", text=str(time))
    while True:
        top.update()
        sleep(0.1)
        if time == 10369:
            sleep(5)
        time += 1

        for star in stars:
            (x, y) = star.position_at(time)
            C.coords(star_ids[star], x*scale, y*scale, x*scale+2, y*scale+2)

        C.itemconfigure(time_id, text=str(time))

if __name__ == "__main__":
    stars = [Star(line) for line in stdin]
    print(stars)

    visualize(stars)
