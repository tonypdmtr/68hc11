@echo off
echo Press RESET on the F1 Board, then...
pause
rem Change PCBUG11 path below as needed by your PC's installation
pcbug11 f1_board macro=f1 port=1 baud=19200
