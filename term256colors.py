#!/usr/bin/env python

####################################################################
# - ANSI escape sequence grammar:
#
# code       :: "^[[" value 'm'
# value      :: attributes ';' foreground ';' background
# attributes :: attribute  ';' attributes
# attribute  :: 00 | 01 | 03 | 04 | 05 | 07 | 22 | 23 | 24 | 25 | 27
# foreground :: 38 ';' 05 ';' color
# background :: 48 ';' 05 ';' color
# color      :: 000-255
#
# - Attribute list:
#
# reset        = 00
# bold         = 01
# italic       = 03
# underline    = 04
# blink        = 05
# reverse      = 07
# no bold      = 22
# no italic    = 23
# no underline = 24
# no blink     = 25
# no reverse   = 27
####################################################################

# ESC + '['
CSI = "\033["

RESET        = 0
BOLD         = 1
ITALIC       = 3
UNDERLINE    = 4
BLINK        = 5
REVERSE      = 7
NO_BOLD      = 22
NO_ITALIC    = 23
NO_UNDERLINE = 24
NO_BLINK     = 25
NO_REVERSE   = 27

BLACK         = 0
LIGHT_RED     = 1
LIGHT_GREEN   = 2
YELLOW        = 3
LIGHT_BLUE    = 4
LIGHT_MAGENTA = 5
LIGHT_CYAN    = 6
HIGH_WHITE    = 7
GRAY          = 8
RED           = 9
GREEN         = 10
BROWN         = 11
BLUE          = 12
MAGENTA       = 13
CYAN          = 14
WHITE         = 15

class Attr:
	def __init__(self, val):
		self.val = val

	def __str__(self):
		return str(self.val)

class Color:
	def __init__(self, val, is_fg = True):
		self.val   = val
		self.is_fg = is_fg

	def __str__(self):
		pre = "38;5;" if self.is_fg else "48;5;"
		return pre + str(self.val)

class EscSeq:
	reset = CSI + str(RESET) + "m"

	def __init__(self, text, fg = None, bg = None, attrs = [], do_reset = True):
		self.text = text
		self.fg   = Color(fg) if fg != None else None
		self.bg   = Color(bg, False) if bg != None else None
		if isinstance(attrs, int):
			attrs = [attrs]
		self.attrs    = map(Attr, attrs)
		self.do_reset = do_reset

	def __str__(self):
		fg    = str(self.fg) if self.fg != None else None
		bg    = str(self.bg) if self.bg != None else None
		attrs = ";".join(map(str, self.attrs))
		val   = [x for x in [attrs, fg, bg] if x]
		pre   = CSI + ";".join(val) + "m" if val else ""
		reset = self.reset if self.do_reset else ""
		return pre + self.text + reset

def main():
	for i in range(256):
		fg = WHITE if i != WHITE else BLACK
		print(i, EscSeq("Hello world!", fg, i, BOLD), sep = '\t')

if __name__ == "__main__":
	main()
