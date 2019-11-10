The set of files found in this archive (or directory) is meant for use with
the ASPiSYS F1 Board and PCBUG11.  (For faster PCs, you may have to get an
updated version of the PCBUG11.EXE file that has the speed bug fixed.)

Usage Instructions
------------------

1. Extract the files of this archive into some dedicated directory (or the
   PCBUG11 directory).  The included utility COL.EXE must be placed in some
   directory that is part of the path.

2. Edit the file ASPISYS.BAT so that it points to the correct COM port.  The
   port=2 parameter is for usage with COM2, change to port=1 for COM1.  You
   should also change the baud=19200 to baud=9600 if you're using a 8MHz
   crystal board.  The 19200 value is for the 16MHz version.

3. Edit the file ASPISYS.BAT so that the PCBUG11.EXE location is correct for
   your system.

4. Connect, power up, and place the F1 Board in boot mode.  (Press and hold
   BOOT, then press and release RESET, then release BOOT.)

5. Download the ASPISYS.S19 file into your F1 Board using the instructions for
   the B11.EXE utility.  Normally, the command B11 ASPISYS will do.  (If you
   haven't already done so, you may have to configure B11 for the correct COM
   port and processor speed.)

6. Once the download is complete, place the F1 Board in expanded mode by
   pressing and releasing the RESET button momentarily.

7. Make sure PCBUG11.EXE is either installed in the current directory or found
   in the PATH somewhere.

8. Give the command ASPISYS <ENTER> to run the ASPISYS.BAT batch file to
   call PCBUG11.

9. You'll now be able to use PCBUG for test running your code.  You should use
   RAM from $1060 to $7FFF for storing your variables and the program code you
   want to debug.

   NOTE: Optionally, the provided talker automatically turns OFF the SDP
         (Software Data Protection) of the external Atmel.  This enables
         PCBUG11 to modify this memory, if required.  This, however, leaves
         the loaded talker unprotected from possible corruption.  You may have
         to reload the talker between your program's crashes.  (Next time you
         use B11 to load a program, the SDP will be turned on automatically,
         if you haven't changed this option from the default).  The -dSDP
         option must be given to ASM11 if the SDP code should be enabled.  The
         default does not enable this code section making external EEPROM
         write-access impossible.

ASPiSYS Ltd.

---------------------------------------------------------------------------

NOTE: We do NOT support third-party software (such as PCBUG11).  However, this
      archive is provided in an effort to help those of you who would like to
      use PCBUG11 v3.42 with the ASPiSYS F1 Board.

      Please keep in mind that what is described here has not been tested
      extensively yet, so it may not work 100% correctly with all of PCBUG11's
      features.  The Trace command seems to work properly.  It will not update
      the register display, automatically.  But, as the PCBug
      manual says, one can define a TRACE macro as follows:

                                RD
                                DASM *

      to have the desired effect.  This has already been done for you in the
      F1.MCR macro definitions file which is included.

---------------------------------------------------------------------------
