;*******************************************************************************
;* Language  : Motorola/Freescale/NXP 68HC11 Assembly Language (aspisys.com/ASM11)
;*******************************************************************************
;
; A task switcher for the MC68HC11F1.
;
; This task switcher uses RTI interrupts to switch between tasks. It has
; been written and tested on Pete Dunster's F1 Controller System.
;
; To use the task switcher, incorporate the set up code into your program's
; initialization code.
;
; Copyright 1999 David Taylor.
;
; 99.08.11 Adapted to ASM11 by Tony Papadimitriou <tonyp@acm.org>
;          Also, optimized a bit
; 01.06.25 Optimized further by Tony Papadimitriou <tonyp@acm.org>
;
; 1.01 - 9 August 1999
;
; Fixed a bug in the _addTask routine. The routine must disable interrupts
; while it is modifying the task structures. To determine whether interrupts
; were already disabled upon entry it saves the state of the CCR register
; so it can check whether to issue a CLI when exiting. The test at the end
; was wrong.
;
; _rmTask no longer inhibits interrupts. This routine runs in the calling
; task's frame of reference and so having it switched to while it is winding
; down does not seem dangerous.
;
; Changed the _taskSwitch to run off the RTI instead of OC2.
;
; Fixed a scheduling bug in _taskSwitch that would run the idle task when
; it shouldn't (if there was only one active task in the system).
;
; Removed all references to the original application.
;
; Optimized the code (for size at least) using BRA rather than JMP and also
; combined the test for valid and runnable task flags into smaller code in
; _taskSwitch.
;
; Added signals - tasks can wait on one or more signals to be sent from
;                 other tasks.
;*******************************************************************************

                    #CaseOn

vbase               def       $7E00

rprint              equ       $FF82
nlout               equ       $FFC4
inchr               equ       $FFCD
outchr              equ       $FFB8
outlhl              equ       $FFB2
outrhl              equ       $FFB5
outstr              equ       $FFCA
out1byt             equ       $FFBB
out1bsp             equ       $FFBE
warm                equ       $FF7C

chkchr              equ       $835F

?                   macro     name,offset
j~1~                equ       voff*~2~+vbase
uv~1~               equ       voff*~2~+vbase+1
                    endm

voff                equ       3
                    @?        toc2,8
                    @?        tic3,10
                    @?        rti,13
                    @?        IRQ,14

REGS                def       $1000
porta               equ       REGS                ; bits 0 - 3 can be used as input, using bit 1 to interrupt
ddra                equ       REGS+$01            ; should not need to be changed from bootup
tcnt                equ       REGS+$0e
TCNT                equ       REGS+$0e
tic3                equ       REGS+$14
ti4o5               equ       REGS+$1e
TI4O5               equ       REGS+$1e
toc1                equ       REGS+$16
toc2                equ       REGS+$18
toc3                equ       REGS+$1a
toc4                equ       REGS+$1c
tctl2               equ       REGS+$21            ; set bits 0:1 to determine which edge causes an interrupt on IC3
tmsk1               equ       REGS+$22            ; set bit 0 to enable interrupts on IC3
tflg1               equ       REGS+$23
TFLG1               equ       REGS+$23
tmsk2               equ       REGS+$24
tflg2               equ       REGS+$25
pactl               equ       REGS+$26
scsr                equ       REGS+$2e
scdr                equ       REGS+$2f
adctl               equ       REGS+$30
adr1                equ       REGS+$31
adr2                equ       REGS+$32
adr3                equ       REGS+$33
adr4                equ       REGS+$34
option              equ       REGS+$39


JMP_OPCODE          equ       $7E
LF                  equ       10
EOT                 equ       $04

execSP              equ       $03FF               ; the executive's stack will always start here

; zero page variables

                    #RAM
                    org       $0000

; debugging msgs

msg                 macro     String
                    mset      1,~@~
                    mstr      1
                    fcc       ~1~,LF,EOT
                    endm

initMsg             @msg      Initializing interrupts
task1Msg            @msg      Task 1
task2Msg            @msg      Task 2
task3Msg            @msg      Task 3

task1Flag           rmb       1                   ;RMB
task2Flag           rmb       1                   ;RMB

; a task definition
;
;
; 0000: 2 byte pointer to next task structure
; 0002: 1 byte status / flag register:
;       bit 1 = in-use/free
;       bit 2 = runnable
;       bit 3 = waiting for signal
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
                    fcb       LF,EOT

usingTaskMsg        fcc       "Switching to task "
utmIdx              rmb       1
                    fcb       LF,EOT

usingIdleMsg        fcc       "Idle"
                    fcb       LF,EOT

;*******************************************************************************
; Start of code
;*******************************************************************************

                    #ROM
                    org       $2000

; Set up the interrupts and so on

setup               proc
                    sei                           ; block interrupts
                    lds       #execSP             ; set the stack pointer to the exec's stack base

                    ldx       #initMsg
                    jsr       outstr

                    lda       pactl               ; set the RTI interval to 4.096 ms
                    anda      #%11111100          ; by setting RTR[1:0] to 00
                    sta       pactl

                    lda       tmsk2               ; enable the RTI
                    ora       #%01000000
                    sta       tmsk2

                    ldx       #_taskSwitch        ; set the address of the RTI handler
                    stx       uvrti               ; which is the task switcher in this case

                    lda       #JMP_OPCODE         ; set the jump instruction for each ISR
                    sta       jrti

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

                    std       ,x                  ; set the "next pointer" for the current task
                    pshd                          ; and transfer the value from D -> X via the stack
                    pulx                          ; moving the current task along one
                    inc       taskIdx
                    bra       setNextPointer

doneNextPointers

          ; add the tasks here

                    ldy       #idleLoop           ; the idle task should always be included for
                    sty       <_atPC              ; when there are no other active tasks
                    bsr       _addTask

                    clr       idleTaskIdx         ; set the idle task
                    bsr       _disableTask        ; and disable it so it will only run when explicitly called by the
                                                  ; task switch algorithm.

                    ldy       #task1Main          ; set the program counter for task1
                    sty       <_atPC
                    bsr       _addTask

                    ldy       #task2Main          ; set the program counter for task2
                    sty       <_atPC
                    bsr       _addTask

                    ldy       #task3Main          ; set the program counter for task3
                    sty       <_atPC
                    bsr       _addTask

          ; application specific initialisation follows

                    lda       #$01                ; set the task flags so task1 will print,
                    sta       task1Flag           ; and task2 won't.
                    clr       task2Flag

          ; load the first task's stack pointer, put the PC
          ; onto the stack and use an RTS to jump to it

                    ldx       firstTask
                    ldd       taskSP,X            ; D -> Y -> SP
                    xgdy
                    tys
                    ldd       taskPC,X
                    pshd

                    cli                           ; re-enable interrupts
                    rts

;*******************************************************************************
; Idle loop
;*******************************************************************************
;
; This is what runs when there are no active user tasks and also
; where an exiting task ends up.
;
; The next time an exiting task is switched out the registers will be saved as
; they were at this point, but the task will never be switched back in so it is
; not a problem.

idleLoop            bra       *

;*******************************************************************************
; Add a task to the task list.
;*******************************************************************************
; The start address of the task must be stored in _atPC before calling
; this routine.
;
; Algorithm
;
; taskIdx = 0;
; taskPtr = firstTask;
;
; for(taskIdx = 0; taskIdx < maxTasks; taskIdx++)
; {
;       if(taskPtr->taskStatus & 0x01 != 0) // task is in use already
;               continue;
;
;       taskPtr->taskPC = _atPC;
;       taskInitialStack = taskPtr + taskStackBaseOffset;
;       taskPtr->taskSP = taskInitialStack;
;       taskPtr->taskStatus |= 0x01;
;       break;
; }

                    #RAM

_atPC               rmb       2
_atClrInt           rmb       1                   ; if the interrupt mask was set on the way in, don't do a cli on exit

                    #ROM

;*******************************************************************************

_addTask            proc
                    tpa                           ; save the status register so it can be checked
                    sta       _atClrInt           ; on exit from this routine

                    sei                           ; disable interrupts while playing with task structures

                    clr       taskIdx
                    ldx       #taskStructBase

_checkTask          inc       taskIdx

                    lda       #maxTasks
                    cmpa      taskIdx
                    bmi       _atExit

                    lda       taskStatus,X        ; get the task status byte and check if the
                    anda      #$01                ; task is in use
                    beq       _useTaskStruct      ; otherwise, use this task struct

                    ldb       #taskStructSize     ; point to the next task struct
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

                    lda       taskStatus,X        ; mark the structure as in use
                    ora       #%00000011          ; and the task as runnable
                    sta       taskStatus,X

_atExit             lda       _atClrInt
                    anda      #%00010000
                    bne       _atDontCli
                    cli
_atDontCli          rts

;****************************************************************************
; Remove a task from the list.
;****************************************************************************
;
; This routine is run in the context of the calling task, not the exec.
; It is similar to calling exit() in a C program.
;
; Before calling this routine, push the 8 bit task ID onto the stack.
;

_rmTask             proc
                    pula                          ; pull the task number into A
                    bsr       _getTaskPtr         ; get the task pointer in X
                    clr       taskStatus,X        ; clear the "task in use" bit of the task status
                    bra       idleLoop            ; run the rest of the time slice in the idle loop

;****************************************************************************
; Disable the task whose ID is passed in A.
;****************************************************************************
;
; This means clearing the runnable flag.
;
; It also assumes interrupts have been disabled, rather than disabling and
; enabling them itself.

_disableTask        proc
                    bsr       _getTaskPtr
                    lda       taskStatus,X
                    anda      #%11111101
                    sta       taskStatus,X
                    rts

;****************************************************************************
; Wait on one or more signals
;****************************************************************************
;
; Tasks calling this routine will block until one of the signals included
; in the mask is received.
;
; The signal mask must be passed in X
;

                    #push
                    #RAM

_wfsClrInt          rmb       1

                    #pull

_waitForSignal      proc
                    tpa                           ; save the status register so it can be checked
                    sta       _wfsClrInt          ; on exit from this routine

                    sei                           ; disable interrupts

                    xgdx
                    ldx       currTask            ; store the signal mask
                    std       taskSigMask,X

                    lda       taskStatus,X
                    anda      #%11111101          ; clear the runnable bit and set the
                    ora       #%00000100          ; waiting for signal bit of the task
                    sta       taskStatus,X        ; status

                    lda       _wfsClrInt
                    anda      #%00010000
                    bne       _wfsDontCli
                    cli
_wfsDontCli

          ; Need to both call the task switcher and also go into some
          ; sort of idle loop until the signal is received.
          ;
          ; Use SWI to go into the task switcher?

_wfsWait            lda       taskStatus,X
                    anda      #%00000010
                    beq       _wfsWait
                    rts

;****************************************************************************
; Send a signal to a task
;****************************************************************************
;
; If the task is waiting for the signal, it will be set to runnable. The
; signal it received is stored in the task's taskSigMask area.
;
; A = task number, X = signal to send


                    #push
                    #RAM

_ssClrInt           rmb       1

                    #pull

_sendSignal         proc
                    tab
                    tpa                           ; save the status register so it can be checked
                    sta       _ssClrInt           ; on exit from this routine

                    sei                           ; disable interrupts

                    tba
                    pshx                          ; save the signal to be sent on the stack
                    bsr       _getTaskPtr         ; set the task structure pointer to the receiving task
                    puly                          ; pull the signal back into Y
                    xgdy                          ; and then put it in D

                    anda      taskSigMask,X
                    sta       taskSigMask,X
                    andb      taskSigMask+1,X
                    stb       taskSigMask+1,X

                    ldd       taskSigMask,X
                    beq       _ssExit

                    lda       taskStatus,X
                    anda      #%11111011          ; set the runnable bit and clear the
                    ora       #%00000010          ; waiting for signal bit of the task
                    sta       taskStatus,X        ; status

_ssExit             lda       _ssClrInt
                    anda      #%00010000
                    bne       :AnRTS
                    cli
                    rts

;****************************************************************************
; Get the task structure pointer for the task ID passed in A.
;****************************************************************************
;
; The task structure pointer is passed back in X.
;
; A, B, X, Y are invalidated.

_getTaskPtr         proc
                    tab                           ; put the task number in Y
                    clra
                    xgdy
                    iny                           ; ensure the counter works, even for tasks 0 and 1

                    ldb       #taskStructSize
                    ldx       #taskStructBase

Loop@@              dey
                    beq       :AnRTS

                    abx                           ; move X onto the next task structure
                    bra       Loop@@

;****************************************************************************
; Algorithm for task switch
;****************************************************************************
;
; When the task switch routine is run, the current task's register set is stored on
; the stack. The task switch is achieved by storing the current task's registers in
; that task's task structure, looking for the next task to run and putting the new
; task's registers onto the stack in place of those just removed.
;
; There are no priorities - the tasks run in list order.
;
; reset interrupt flag
; taskPtr = currentTask;
; transfer register set from stack to taskPtr->registers
; stack pointer = executive stack base
;
; while(true)
; {
;       if(taskPtr->nextTask == null)
;               taskPtr = firstTask;
;       else
;               taskPtr = taskPtr->nextTask;
;
;       if(taskPtr == currTask)
;       {
;               currTask = idleTask;
;               break;
;       }
;       if(taskPtr->taskStatus & 0x01 != 0)     // if valid task
;               if(taskPtr->taskStatus & 0x02 != 0)     // if runnable
;                       break;
; }
;
; currTask = taskPtr;
; transfer taskPtr->registers to the stack
; schedule next interrupt
; return from interrupt (using new task's registers on the stack)
;
; The endless loop looks wrong but the idle task is always available to run.
;
                    org       *

_taskSwitch         proc
                    lda       tflg2
                    ora       #%01000000          ; set the flag so the RTI can happen again
                    sta       tflg2

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

noRunnables         ldx       #usingIdleMsg
                    jsr       outstr

                    lda       idleTaskIdx
                    sta       currTaskIdx
                    bsr       _getTaskPtr

useTask             stx       currTask            ; and make it the current task

;                   lda       currTaskIdx
;                   adda      #$30
;                   sta       utmIdx
;                   ldx       #usingTaskMsg
;                   jsr       outstr
;
;                   ldx       currTask

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

;                   cli                           ;what the hell was this for?
                    rti

;*******************************************************************************
; Example tasks
;*******************************************************************************
;
; If flag is set, print msg, unset flag, set other task's flag

task1Main           proc
                    ldx       #task1Msg
                    jsr       outstr

                    lda       #$02                ; signal task 2 to run
                    ldx       #%0000000000000001
                    jsr       _sendSignal
                    bra       task1Main

;*******************************************************************************
; If flag is set, print msg, unset flag, set other task's flag

task2Main           proc
                    ldx       #%0000000000000001
                    jsr       _waitForSignal
                    ldx       #task2Msg
                    jsr       outstr
                    bra       task2Main

;*******************************************************************************
; Run once only

task3Main           proc
                    ldx       #task3Msg
                    jsr       outstr
                    lda       currTaskIdx
                    psha
                    jmp       _rmTask

;*******************************************************************************

                    #RAM
                    org       $7000

taskStructBase      rmb       400
taskStructMax       rmb       1

                    end       setup
