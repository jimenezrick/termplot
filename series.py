#!/usr/bin/env python

import sys

BARS = " ▁▂▃▄▅▆▇"

class Series:
	def __init__(self, data, min_ = None, max_ = None):
		self.data = list(map(lambda elem: float(elem), data))
		self.min  = min(self.data) if min_ == None else min_
		self.max  = max(self.data) if max_ == None else max_

	def index(self):
		if self.min == self.max:
			return [0]
		else:
			step = (self.max - self.min) / len(BARS)
			return map(lambda elem: int((elem - self.min) / step), self.data)

	def plot(self):
		for i in self.index():
			l = len(BARS)
			assert i <= l
			if i == l:
				i = l - 1
			print(BARS[i], end = "")
		print()

def main(data):
	series = Series(data)
	series.plot()

if __name__ == "__main__":
	if len(sys.argv) == 1:
		print("Usage: %s n1 [n2 n3...]" % sys.argv[0], file = sys.stderr)
		exit(1)
	main(sys.argv[1:])






########################################################################
ESC   = "\033["
COLOR = "31m"

BLACK = "0m"
STOP  = ESC + BLACK
