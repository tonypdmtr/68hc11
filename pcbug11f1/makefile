################################################################################
# Makefile for PCBug11 talker for the ASPiSYS F1 Board
# Written by Tony Papadimitriou <tonyp@acm.org>
# For use with Borland's MAKE or compatible
################################################################################

all: f1_board.map test.s19

aspisys.s19: aspisys.asm
  @asm11 aspisys.asm -m- -ls- -l- -s+ -exp+

aspisys.exp: aspisys.s19

f1_board.map: aspisys.s19
  @lua -E col.lua (1 20)(31 35) ^< aspisys.exp ^>f1_board.map
  @del aspisys.exp

b11: aspisys.s19
  @b11 aspisys

rar: aspisys.asm aspisys.bat aspisys.s19 test.asm f1.mcr test.s19 readme.txt f1_board.map makefile col.lua
  @wrar u pcbugf1 aspisys.asm aspisys.bat aspisys.s19 test.asm f1.mcr test.s19 readme.txt f1_board.map makefile col.lua

test.s19: test.asm
  @asm11 test -l- -m- -s+ -ls-
