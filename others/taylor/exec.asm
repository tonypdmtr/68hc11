; A task switcher for the MC6811 based F1.
;
; This task switcher uses RTI interrupts to switch between tasks. It has
; been written and tested on Pete Dunster's F1 Controller System.
;
; To use the task switcher, incorporate the set up code into your
; program's initialisation code.
;
; Copyright 1999 David Taylor.


; <H3>1.01 - 9 August 1999</H3>
; <UL>
; <LI> Fixed a bug in the <CODE>_addTask</CODE> routine. The routine must
; disable interrupts while it is modifying the task structures. To determine
; whether interrupts were already disabled upon entry it saves the state of the
; CCR register so it can check whether to issue a CLI when exiting. The test at
; the end was wrong.
; <P>
; <LI> <CODE>_rmTask</CODE> no longer inhibits interrupts. This routine runs in
; the calling task's frame of reference and so having it switched to while it is
; winding down does not seem dangerous.
; <P>
; <LI> Changed the <CODE>_taskSwitch</CODE> to run off the RTI instead of OC2.
; <P>
; <LI> Fixed a scheduling bug in <CODE>_taskSwitch</CODE> that would run the
; idle task when it shouldn't (if there was only one active task in the system).
; <P>
; <LI> Removed all references to the original application.
; <P>
; <LI> Optimised the code (for size at least) using BRA rather than JMP and also
; combined the test for valid and runnable task flags into smaller code in
; <CODE>_taskSwitch</CODE>.
; <P>
; <LI> Added signals - tasks can wait on one or more signals to be sent from
; other tasks.


                    #CaseOn
                    #ListOn
                    #OptRelOff

vbase               equ       $7e00

rprint              equ       $ff82
nlout               equ       $ffc4
inchr               equ       $ffcd
outchr              equ       $ffb8
outlhl              equ       $ffb2
outrhl              equ       $ffb5
outstr              equ       $ffca
out1byt             equ       $ffbb
out1bsp             equ       $ffbe
warm                equ       $ff7c

chkchr              equ       $835f

voff                equ       3
jtoc2               equ       voff*8+vbase
uvtoc2              equ       voff*8+vbase+1
jtic3               equ       voff*10+vbase
uvtic3              equ       voff*10+vbase+1
jrti                equ       voff*13+vbase
uvrti               equ       voff*13+vbase+1
jIRQ                equ       voff*14+vbase
uvIRQ               equ       voff*14+vbase+1

regbs               equ       $1000
porta               equ       regbs               ; bits 0 - 3 can be used as input, using bit 1 to interrupt
ddra                equ       regbs+$01           ; should not need to be changed from bootup
tcnt                equ       regbs+$0e
TCNT                equ       regbs+$0e
tic3                equ       regbs+$14
ti4o5               equ       regbs+$1e
TI4O5               equ       regbs+$1e
toc1                equ       regbs+$16
toc2                equ       regbs+$18
toc3                equ       regbs+$1a
toc4                equ       regbs+$1c
tctl2               equ       regbs+$21           ; set bits 0:1 to determine which edge causes an interrupt on IC3
tmsk1               equ       regbs+$22           ; set bit 0 to enable interrupts on IC3
tflg1               equ       regbs+$23
TFLG1               equ       regbs+$23
tmsk2               equ       regbs+$24
tflg2               equ       regbs+$25
pactl               equ       regbs+$26
scsr                equ       regbs+$2e
scdr                equ       regbs+$2f
adctl               equ       regbs+$30
adr1                equ       regbs+$31
adr2                equ       regbs+$32
adr3                equ       regbs+$33
adr4                equ       regbs+$34
option              equ       regbs+$39


opjmp               equ       $7e

execSP              equ       $03ff               ; the executive's stack will always start here


; zero page variables

                    org       $0000

; debugging msgs

initMsg             fcc       "Initialising interrupts"
                    fcb       $0a,$04

task1Msg            fcc       "Task 1"
                    fcb       $0a,$04

task2Msg            fcc       "Task 2"
                    fcb       $0a,$04

task3Msg            fcc       "Task 3"
                    fcb       $0a,$04

task1Flag           rmb       1
task2Flag           rmb       1


; a task definition


; 0000: 2 byte pointer to next task structure
; 0002: 1 byte status / flag register:
; bit 1 = in-use/free
; bit 2 = runnable
; bit 3 = waiting for signal
; 0003: 2 byte countdown / signal mask
; 0005: ccr
; 0006: b
; 0007: a
; 0008: x
; 000a: y
; 000c: pc
; 000e: sp
; 0010 - task stack


taskNext            equ       0
taskStatus          equ       2
taskCountdown       equ       3
taskSigMask         equ       3
taskCCR             equ       5
taskB               equ       6
taskA               equ       7
taskX               equ       8
taskY               equ       10
taskPC              equ       12
taskSP              equ       14
taskStackBase       equ       78

taskStructSize      equ       80


firstTask           rmb       2                   ; pointer to the first task structure
currTask            rmb       2                   ; pointer to the current task structure
currTaskIdx         rmb       1                   ; current task ID
idleTaskIdx         rmb       1                   ; idle task ID

maxTasks            equ       5                   ; the size of the task list
taskIdx             rmb       1                   ; used when looping through the list

tmpTaskWord         rmb       2                   ; temporary storage used by the add task routine

checkingTaskMsg     fcc       "Checking task "
ctmIdx              rmb       1
                    fcb       $0a,$04

usingTaskMsg        fcc       "Switching to task "
utmIdx              rmb       1
                    fcb       $0a,$04

usingIdleMsg        fcc       "Idle"
                    fcb       $0a,$04

;*****************************************************************
;
; Start of code
;
;*****************************************************************
                    org       $2000

;
; Set up the interrupts and so on
;
setup
                    sei                           ; block interrupts

                    ldx       #initMsg
                    jsr       outstr

                    ldaa      pactl               ; set the RTI interval to 4.096 ms
                    anda      #%11111100          ; by setting RTR[1:0] to 00
                    staa      pactl

                    ldaa      tmsk2               ; enable the RTI
                    oraa      #%01000000
                    staa      tmsk2

                    ldx       #_taskSwitch        ; set the address of the RTI handler
                    stx       uvrti               ; which is the task switcher in this case

                    ldaa      #opjmp              ; set the jump instruction for each ISR
                    staa      jrti


; set up tasks


                    ldx       #taskStructBase     ; set up the task list head pointer
                    stx       firstTask
                    stx       currTask
                    clr       currTaskIdx


; loop through the task structures setting the "next task" pointers

                    lda       #$01
                    sta       taskIdx

                    ldd       #taskStructBase
setNextPointer      addd      #taskStructSize     ; add the task structure size to get the value for
                    xgdy                          ; the next task structure and save the value D -> Y

                    lda       #maxTasks           ; check if at the end of the task list
                    cmpa      taskIdx
                    beq       doneNextPointers

                    xgdy                          ; restore the pointer value from Y -> D

                    std       0,X                 ; set the "next pointer" for the current task
                    pshb                          ; and transfer the value from D -> X via the stack
                    psha
                    pulx                          ; moving the current task along one
                    inc       taskIdx
                    jmp       setNextPointer

doneNextPointers

; add the tasks here

                    ldy       #idleLoop           ; the idle task should always be included for
                    sty       _atPC               ; when there are no other active tasks
                    jsr       _addTask

                    lda       #$00                ; set the idle task
                    sta       idleTaskIdx
                    jsr       _disableTask        ; and disable it so it will only run when explicitly called by the
; task switch algorithm.

                    ldy       #task1Main          ; set the program counter for task1
                    sty       _atPC
                    jsr       _addTask

                    ldy       #task2Main          ; set the program counter for task2
                    sty       _atPC
                    jsr       _addTask

                    ldy       #task3Main          ; set the program counter for task3
                    sty       _atPC
                    jsr       _addTask


; application specific initialisation follows

                    lda       #$01                ; set the task flags so task1 will print,
                    sta       task1Flag           ; and task2 won't.
                    lda       #$00
                    sta       task2Flag


; load the first task's stack pointer, put the PC
; onto the stack and use an RTS to jump to it

                    ldx       firstTask
                    ldd       taskSP,X            ; D -> Y -> SP
                    xgdy
                    tys
                    ldd       taskPC,X
                    pshb
                    psha

                    cli                           ; re-enable interrupts

                    rts

;****************************************************************************
; Idle loop
;****************************************************************************

; This is what runs when there are no active user tasks and also
; where an exiting task ends up.

; The next time an exiting task is switched out the registers will be saved as
; they were at this point, but the task will never be switched back in so it is
; not a problem.

idleLoop            bra       idleLoop

;****************************************************************************
; Add a task to the task list.
;****************************************************************************
; The start address of the task must be stored in _atPC before calling
; this routine.

; Algorithm

; taskIdx = 0;
; taskPtr = firstTask;

; for(taskIdx = 0; taskIdx < maxTasks; taskIdx++)
; {
; if(taskPtr->taskStatus & 0x01 != 0) // task is in use already
; continue;

; taskPtr->taskPC = _atPC;
; taskInitialStack = taskPtr + taskStackBaseOffset;
; taskPtr->taskSP = taskInitialStack;
; taskPtr->taskStatus |= 0x01;
; break;
; }


_atPC               rmb       2
_atClrInt           rmb       1                   ; if the interrupt mask was set on the way in, don't do a cli on exit

_addTask
                    tpa                           ; save the status register so it can be checked
                    staa      _atClrInt           ; on exit from this routine

                    sei                           ; disable interrupts while playing with task structures

                    clr       taskIdx
                    ldx       #taskStructBase

_checkTask
                    inc       taskIdx

                    ldaa      #maxTasks
                    cmpa      taskIdx
                    bpl       _gntIncr
                    bra       _atExit

_gntIncr
                    ldaa      taskStatus,X        ; get the task status byte and check if the
                    anda      #$01                ; task is in use
                    bne       _getNextTask        ; if it is, get the next task

                    bra       _useTaskStruct      ; otherwise, use this task struct

_getNextTask
                    ldab      #taskStructSize     ; point to the next task struct
                    abx
                    bra       _checkTask          ; check the next task structure

_useTaskStruct

; At this point, X is pointing the the base of the task structure.
; Set the task's initial program counter and stack pointer and mark
; the structure as in use.

                    ldd       _atPC               ; get the task entry point
                    std       taskPC,X            ; and store it in the task struct PC

                    stx       tmpTaskWord         ; make the task's stack start at the
                    ldd       tmpTaskWord         ; end of it's struct
                    addd      #taskStackBase
                    std       taskSP,X

                    ldaa      taskStatus,X        ; mark the structure as in use
                    ora       #%00000011          ; and the task as runnable
                    staa      taskStatus,X

_atExit
                    ldaa      _atClrInt
                    anda      #%00010000
                    bne       _atDontCli
                    cli
_atDontCli
                    rts

;****************************************************************************
; Remove a task from the list.
;****************************************************************************

; This routine is run in the context of the calling task, not the exec.
; It is similar to calling exit() in a C program.

; Before calling this routine, push the 8 bit task ID onto the stack.


_rmTask
                    pula                          ; pull the task number into A
                    jsr       _getTaskPtr         ; get the task pointer in X
                    clr       taskStatus,X        ; clear the "task in use" bit of the task status
                    jmp       idleLoop            ; run the rest of the time slice in the idle loop

;****************************************************************************
; Disable the task whose ID is passed in A.
;****************************************************************************

; This means clearing the runnable flag.

; It also assumes interrupts have been disabled, rather than disabling and
; enabling them itself.

_disableTask
                    jsr       _getTaskPtr
                    ldaa      taskStatus,X
                    anda      #%11111101
                    staa      taskStatus,X
                    rts

;****************************************************************************
; Wait on one or more signals
;****************************************************************************

; Tasks calling this routine will block until one of the signals included
; in the mask is received.

; The signal mask must be passed in X


_wfsClrInt          rmb       1

_waitForSignal
                    tpa                           ; save the status register so it can be checked
                    staa      _wfsClrInt          ; on exit from this routine

                    sei                           ; disable interrupts

                    xgdx
                    ldx       currTask            ; store the signal mask
                    std       taskSigMask,X

                    ldaa      taskStatus,X
                    anda      #%11111101          ; clear the runnable bit and set the
                    oraa      #%00000100          ; waiting for signal bit of the task
                    staa      taskStatus,X        ; status

                    ldaa      _wfsClrInt
                    anda      #%00010000
                    bne       _wfsDontCli
                    cli
_wfsDontCli


; Need to both call the task switcher and also go into some
; sort of idle loop until the signal is received.

; Use SWI to go into the task switcher?


_wfsWait            ldaa      taskStatus,X
                    anda      #%00000010
                    beq       _wfsWait

                    rts

;****************************************************************************
; Send a signal to a task
;****************************************************************************

; If the task is waiting for the signal, it will be set to runnable. The
; signal it received is stored in the task's taskSigMask area.

; A = task number, X = signal to send


_ssClrInt           rmb       1

_sendSignal
                    tab
                    tpa                           ; save the status register so it can be checked
                    staa      _ssClrInt           ; on exit from this routine

                    sei                           ; disable interrupts

                    tba
                    pshx                          ; save the signal to be sent on the stack
                    jsr       _getTaskPtr         ; set the task structure pointer to the receiving task
                    puly                          ; pull the signal back into Y
                    xgdy                          ; and then put it in D

                    anda      taskSigMask,X
                    staa      taskSigMask,X
                    andb      taskSigMask+1,X
                    stab      taskSigMask+1,X

                    ldd       taskSigMask,X
                    beq       _ssExit

                    ldaa      taskStatus,X
                    anda      #%11111011          ; set the runnable bit and clear the
                    oraa      #%00000010          ; waiting for signal bit of the task
                    staa      taskStatus,X        ; status

_ssExit
                    ldaa      _ssClrInt
                    anda      #%00010000
                    bne       _ssDontCli
                    cli
_ssDontCli
                    rts


;****************************************************************************
; Get the task structure pointer for the task ID passed in A.
;****************************************************************************

; The task structure pointer is passed back in X.

; A, B, X, Y are invalidated.

_getTaskPtr
                    tab                           ; put the task number in Y
                    clra
                    xgdy
                    iny                           ; ensure the counter works, even for tasks 0 and 1

                    ldab      #taskStructSize
                    ldx       #taskStructBase
_gtpLoop
                    dey
                    beq       _gtpExit

                    abx                           ; move X onto the next task structure
                    bra       _gtpLoop

_gtpExit
                    rts


;****************************************************************************
; Algorithm for task switch
;****************************************************************************

; When the task switch routine is run, the current task's register set is stored on
; the stack. The task switch is achieved by storing the current task's registers in
; that task's task structure, looking for the next task to run and putting the new
; task's registers onto the stack in place of those just removed.

; There are no priorities - the tasks run in list order.

; reset interrupt flag
; taskPtr = currentTask;
; transfer register set from stack to taskPtr->registers
; stack pointer = executive stack base

; while(true)
; {
; if(taskPtr->nextTask == null)
; taskPtr = firstTask;
; else
; taskPtr = taskPtr->nextTask;

; if(taskPtr == currTask)
; {
; currTask = idleTask;
; break;
; }
; if(taskPtr->taskStatus & 0x01 != 0)     // if valid task
; if(taskPtr->taskStatus & 0x02 != 0)     // if runnable
; break;
; }

; currTask = taskPtr;
; transfer taskPtr->registers to the stack
; schedule next interrupt
; return from interrupt (using new task's registers on the stack)

; The endless loop looks wrong but the idle task is always available to run.


                    org       *

_taskSwitch         ldaa      tflg2
                    oraa      #%01000000          ; set the flag so the RTI can happen again
                    staa      tflg2


; save current register set

                    ldx       currTask            ; get the base address of current task structure

                    pula
                    sta       taskCCR,X
                    pula
                    sta       taskB,X
                    pula
                    sta       taskA,X
                    puly
                    sty       taskX,X
                    puly
                    sty       taskY,X
                    puly
                    sty       taskPC,X
                    sts       taskSP,X

                    lds       #execSP             ; set the stack pointer to the exec's stack base


; going back to the first task if necessary


getNextTask         ldd       taskNext,X          ; load D with next task pointer from that structure
                    bne       haveTaskPtr         ; if pointer not null, use it
                    ldd       firstTask           ; if pointer is null, load D with first task's base address

                    clr       currTaskIdx         ; set the currTaskIdx to "-1" so the increment
                    dec       currTaskIdx         ; after haveTaskPtr will work properly

haveTaskPtr         xgdx                          ; put the next task base address in X

                    inc       currTaskIdx

                    lda       taskStatus,X        ; if the next task is valid, check if it's runnable
                    anda      #%00000011          ; otherwise check the next task
                    cmpa      #%00000011
                    beq       useTask

                    cpx       currTask            ; if made it all the way through the list and not even the
                    beq       noRunnables         ; current task is runnable, switch to the idle task
; which must always be valid (but not necessarily runnable)

                    bra       getNextTask

noRunnables
                    ldx       #usingIdleMsg
                    jsr       outstr

                    lda       idleTaskIdx
                    sta       currTaskIdx
                    jsr       _getTaskPtr
useTask
                    stx       currTask            ; and make it the current task

; lda     currTaskIdx
; adda    #$30
; sta     utmIdx
; ldx     #usingTaskMsg
; jsr     outstr

; ldx     currTask

; set the register set for the task being switched in (X points to task struct base)


                    lds       taskSP,X
                    ldy       taskPC,X
                    pshy
                    ldy       taskY,X
                    pshy
                    ldy       taskX,X
                    pshy
                    lda       taskA,X
                    psha
                    lda       taskB,X
                    psha
                    lda       taskCCR,X
                    psha

                    cli

                    rti

;****************************************************************************
; Example tasks
;****************************************************************************

; If flag is set, print msg, unset flag, set other task's flag

task1Main           ldx       #task1Msg
                    jsr       outstr

                    lda       #$02                ; signal task 2 to run
                    ldx       #%0000000000000001
                    jsr       _sendSignal
                    jmp       task1Main


; If flag is set, print msg, unset flag, set other task's flag

task2Main           ldx       #%0000000000000001
                    jsr       _waitForSignal
task2Print          ldx       #task2Msg
                    jsr       outstr
                    jmp       task2Main


; Run once only

task3Main           ldx       #task3Msg
                    jsr       outstr
                    ldaa      currTaskIdx
                    psha
                    jmp       _rmTask

                    org       $7000
taskStructBase      rmb       400
taskStructMax       rmb       1
