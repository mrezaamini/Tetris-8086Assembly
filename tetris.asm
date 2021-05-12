;Mohammadreza Amini
;TETRIS GAME
;ASSEMBLY EMU8086 

.MODEL SMALL


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                         ;
;                       MACROS                            ;
;                                                         ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   

PUSH_REGS MACRO 
    PUSH AX
    PUSH DX
    PUSH CX
    PUSH BX


ENDM   

POP_REGS MACRO 
    POP BX
    POP CX
    POP DX
    POP AX

 
ENDM 


DRAW_ROW Macro x,y,z,color  
    Local L1
    ; draws a line in row x from col y to col z
    MOV AH, 0CH
    MOV AL, color
    MOV CX, y
    MOV DX, x
L1: INT 10h
    INC CX
    CMP CX, z
    JL L1
    EndM

DRAW_COL Macro x,y,z,color 
    Local L2
    ; draws a line col y from row y to row z
    MOV AH, 0CH
    MOV AL, color
    MOV CX, x
    MOV DX, y
L2: INT 10h
    INC DX
    CMP DX, z
    JL L2
    EndM


DRAW_BLOCK MACRO X, Y, COLOR  
    LOCAL LOOP1, LOOP2  
    MOV AH, 0CH
    MOV AL, COLOR
    MOV DX, X 
LOOP1:
    MOV CX, Y
LOOP2:
    INT 10H 
    MOV BX, Y
    INC CX
    ADD BX, 10
    DEC BX
    CMP CX, BX   
    JBE LOOP2
    INC DX
    MOV BX, X
    ADD BX, 10 
    DEC BX
    CMP DX, BX
    JBE LOOP1 
 
ENDM


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                         ;
;                     DATA AND STACK                      ;
;                                                         ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 
.STACK 100H 
.DATA   
     ALEFT DB 'A: LEFT', '$' 
     WROTATE DB 'W: ROTATE', '$'
     SDOWN DB 'S: DOWN', '$'
     FDROP DB 'F: DROP','$'
     DRIGHT DB 'D: RIGHT','$'
     QQUIT DB 'Q: QUIT','$'
     
     NEXT DB '-NEXT ONES-','$'
     NUM1 DB ':1','$'
     NUM2 DB ':2','$' 
     
     BOARD DB 210 DUP(0)
     BOARDC DB 210 DUP(0)
     
     NBOARD DB 16 DUP(0)
     NBOARDC DB 16 DUP(0)
     
     NNBOARD DB 16 DUP(0)
     NNBOARDC DB 16 DUP(0)

     
     LEFT_SIDE EQU 110  
     RIGHT_SIDE EQU 211
     DOWN_SIDE EQU 200  
     UP_SIDE EQU 0 
     
     BLOCK_SIZE DW 0 
     BLOCKS_PER_SHAPE DW 4 
     
     THIS_ROW DW 0
     THIS_COL DW 0 
     
     FIRST_INDEX DW 0
     F_OFFSET DW 0
     S_OFFSET DW 0
     T_OFFSET DW 0
     
     
     
     ;;;;;;; SHAPES ;;;;;;;
     T_SHAPE DB 5,9,10,11, ;UP 
             DB 5,10,11,20, ;RIGHT
             DB 5,1,2,11, ;DOWN
             DB 5,9,10,20 ;LEFT
                
     LINE_SHAPE DB 3,1,2,3 ;HORIZONTAL
                DB 5,10,20,30 ;VERTICAL 
                DB 3,1,2,3 ;HORIZONTAL 
                DB 5,10,20,30 ;VERTICAL
                   
                   
     L_SHAPE DB 4,10,20,21 ;UP
             DB 4,1,2,10 ;RIGHT 
             DB 4,1,11,21 ;DOWN
             DB 6,8,9,10 ;LEFT 
                
     Z_SHAPE DB 4,10,11,21 ;UP
             DB 4,1,9,10 ;RIGHT
             DB 4,10,11,21 ;DOWN
             DB 4,1,9,10 ;LEFT
                
     SQUARE_SHAPE DB 4,1,10,11 ;UP
                  DB 4,1,10,11 ;RIGHT
                  DB 4,1,10,11 ;DOWN
                  DB 4,1,10,11 ;LEFT  
                  
                  
                  
     ;;;;;;;
     NT_SHAPE DB 1,3,4,5, ;UP 
             DB 1,4,5,8, ;RIGHT
             DB 1,1,2,5, ;DOWN
             DB 1,3,4,8 ;LEFT
                
     NLINE_SHAPE DB 0,1,2,3 ;HORIZONTAL
                DB 2,4,8,12 ;VERTICAL 
                DB 0,1,2,3 ;HORIZONTAL 
                DB 2,4,8,12 ;VERTICAL
                   
                   
     NL_SHAPE DB 1,4,8,9 ;UP
             DB 4,1,2,4 ;RIGHT 
             DB 4,1,5,9 ;DOWN
             DB 2,2,3,4 ;LEFT 
                
     NZ_SHAPE DB 1,4,5,9 ;UP
             DB 1,1,5,6 ;RIGHT
             DB 1,4,5,9 ;DOWN
             DB 1,1,5,6 ;LEFT
                
     NSQUARE_SHAPE DB 1,1,4,5 ;UP
                   DB 1,1,4,5 ;RIGHT
                   DB 1,1,4,5 ;DOWN
                   DB 1,1,4,5 ;LEFT
                       
     MOVING_PATTERN DB 0,0,0,0,
                    DB 0,0,0,0, 
                    DB 0,0,0,0,
                    DB 0,0,0,0
                     
                     
     COLORS DW 0,0,0,0,0 
     
     DESTINATION_ARRAY DW 0,0,0,0
     MOVING_INDEX_ARRAY DW 0,0,0,0 
     
     MYBOOL DW 1 ;0:FALSE 1:TRUE 
     MYOFFSET DW 0
     
                     
                     
     RAND_SHAPE DW 6
     RAND_COLOR DW 6 
     RAND_ORI DW 4 
     
     NRAND_SHAPE DW 6
     NRAND_COLOR DW 6 
     NRAND_ORI DW 4 
     
     NNRAND_SHAPE DW 6
     NNRAND_COLOR DW 6 
     NNRAND_ORI DW 4
     
     BLOCK_FINISHED DW 0 
     
     newC dw 0
     newindex dw 0 
     
     
     MYRAND DW 5    
     
     MOVING_SHAPE DW 0     ;0>>NOTHING 1>>T 2>>LINE 3>>L 4>>Z 5>>SQUARE   
     MOVING_INDEX DW 0     ;index of moviong object in its array  
     MOVING_ORI DW 0                                             
     MOVING_COLOR DW 0
     
     
     TIME DB 0               
     
     MY_LINE DW 0 
     
     ALLONE_LINE DW 0      ;1:line is full / 0:line is empty    
     
     SCORE DW 0
     NEWLINE DW 0 
     
       

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                         ;
;                       MAIN CODE                         ;
;                                                         ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;     
     

.CODE 
MAIN PROC FAR
    MOV AX,@DATA
    MOV DS,AX
    
    
INITIALIZATION: 
    
    MOV AH,00
    MOV AL,13H
    INT 10H
    
    MOV BX,200
    MOV CX,0
    MAKE_BORDER:
    CMP CX,10
    JE MBEND
    MOV BOARD+BX,1
    MOV BOARDC+BX,0
    INC BX
    INC CX
    JMP MAKE_BORDER
    
    MBEND:
    
    CALL PRINT_UI 
    CALL RAND_START
    CALL GET_SIZE 
    CALL NNNEW_SHAPE
    CALL NNEW_SHAPE
    CALL NEW_SHAPE 
    
    CALL NNNEW_SHAPE
    CALL NNEW_SHAPE
    
     
    CALL GET_MOVING_PATTERN
    CALL CALC_INDEX 
    CALL PRINT_MAP     
    CALL SHOW_SCORE 
    CALL NNNEW_SHAPE
    CALL NPRINT_MAP_ALL
    CALL NNPRINT_MAP_ALL
    MOV AH,2CH
    INT 21H 
    MOV [TIME],DH
                     
LOOP1:
    CMP [BLOCK_FINISHED],1
    JNE LOOC
    CALL CHECK_BOARD
    CALL SHOW_SCORE
    CALL NEW_SHAPE
    CALL NNEW_SHAPE
    CALL NNNEW_SHAPE
    CALL RAND_START 
    CALL GET_MOVING_PATTERN
    CALL CALC_INDEX
    CALL PRINT_MAP_ALL  
    CALL NPRINT_MAP_ALL
    CALL NNPRINT_MAP_ALL
    MOV [BLOCK_FINISHED],0  
    
    LOOC:
    MOV AH,2CH
    INT 21H
    CMP DH,[TIME]
    JNE MOVE_1S_DOWN
    MOV AH,01
    INT 16H 
    JZ LOOP1
    MOV AH,0
    INT 16H
    CMP AL,'A'
    JE ADETECT 
    CMP AL,'a'  
    JE ADETECT
    CMP AL,'D'
    JE DDETECT
    CMP AL,'d'
    JE DDETECT
    CMP AL,'W'
    JE WDETECT
    CMP AL,'w'
    JE WDETECT
    CMP AL,'S'
    JE SDETECT
    CMP AL,'s'
    JE SDETECT
    CMP AL,'F'
    JE FDETECT
    CMP AL,'f'
    JE FDETECT        
    CMP AL,'Q'
    JE QDETECT
    CMP AL,'q'
    JE QDETECT 
    JMP LOOP1
    ADETECT:
    CALL CHARA
    JMP LOOP1  
    DDETECT:
    CALL CHARD
    JMP LOOP1
    WDETECT:
    CALL CHARW
    JMP LOOP1
    SDETECT:
    CALL CHARS
    JMP LOOP1
    FDETECT:
    CALL CHARF
    JMP LOOP1
    QDETECT:
    CALL CHARQ
    JMP LOOP1 
    
    MOVE_1S_DOWN:
    CALL MOVE_SHAPE_DOWN
    CALL PRINT_MAP
    MOV AH,2CH
    INT 21H 
    MOV [TIME],DH
    JMP LOOP1
    
    


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                         ;
;                       PROCEDURES                        ;
;                                                         ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
  

CHARA PROC
    MOV AX,-1
    CALL MOVE_CONTROL
    CALL PRINT_MAP
RET
ENDP CHARA  


CHARD PROC
    MOV AX,1
    CALL MOVE_CONTROL
    CALL PRINT_MAP

RET
ENDP CHARD 

CHARW PROC
    MOV AX,2
    CALL MOVE_CONTROL
    CALL PRINT_MAP

RET
ENDP CHARW
   
CHARS PROC 
    CALL MOVE_SHAPE_DOWN
    CALL PRINT_MAP 
RET
ENDP CHARS 

CHARF PROC 
    ALL_DOWNL:
    CMP [BLOCK_FINISHED],1
    JE CHARFEND
    CALL MOVE_SHAPE_DOWN
    JMP ALL_DOWNL
    
    
    CHARFEND:
RET
ENDP CHARF

CHARQ PROC
    mov ax, 4c00h ; exit to operating system.
    int 21h

RET
ENDP CHARQ 
 
PRINT_BORDER PROC 
       DRAW_ROW 0,LEFT_SIDE,RIGHT_SIDE,0FH
       DRAW_ROW DOWN_SIDE,LEFT_SIDE,RIGHT_SIDE,0FH
       DRAW_COL LEFT_SIDE-1,UP_SIDE,DOWN_SIDE,0FH
       DRAW_COL RIGHT_SIDE-1,UP_SIDE,DOWN_SIDE,0FH 
            
RET
ENDP PRINT_BORDER

GET_SIZE PROC
    MOV AX, RIGHT_SIDE
    SUB AX, LEFT_SIDE
    MOV BX, 10  
    XOR DX,DX
    DIV BX
    MOV BLOCK_SIZE, AX
    XOR AX,AX
    XOR BX,BX
    
RET
ENDP GET_SIZE 

PRINT_MAP PROC 
       PUSH_REGS
       MOV BX,0  
       
CHECK_AND_CALC_LOOP:
       CMP BOARD+BX,2
       JNE NC
       MOV BOARD+BX,0
       JMP CNC
       
       NC:
       CMP BOARD+BX,1
       JNE CONTINUE_MAP 
       CNC:
       XOR DX,DX
       MOV CX,10
       MOV AX,BX
       DIV CX 
       MOV [THIS_COL], DX
       MOV CX, BLOCK_SIZE 
       MUL CX
       ADD AX,UP_SIDE
       MOV [THIS_ROW],AX
       MOV AX,[THIS_COL]
       MUL CX
       ADD AX, LEFT_SIDE
       MOV [THIS_COL], AX
       PUSH_REGS
       DRAW_BLOCK [THIS_ROW],[THIS_COL], BOARDC+BX
       POP_REGS
CONTINUE_MAP:
       INC BX
       CMP BX, 199
       JA FINISH_PRINT_MAP
       JMP CHECK_AND_CALC_LOOP
FINISH_PRINT_MAP:
       CALL PRINT_BORDER 
       POP_REGS
       RET 
ENDP PRINT_MAP  

PRINT_MAP_ALL PROC 
       PUSH_REGS
       MOV BX,0  
CHECK_AND_CALC_LOOP_ALL: 
       XOR DX,DX
       MOV CX,10
       MOV AX,BX
       DIV CX 
       MOV [THIS_COL], DX
       MOV CX, BLOCK_SIZE 
       MUL CX
       ADD AX,UP_SIDE
       MOV [THIS_ROW],AX
       MOV AX,[THIS_COL]
       MUL CX
       ADD AX, LEFT_SIDE
       MOV [THIS_COL], AX
       PUSH_REGS
       DRAW_BLOCK [THIS_ROW],[THIS_COL], BOARDC+BX
       POP_REGS
CONTINUE_MAP_ALL:
       INC BX
       CMP BX, 199
       JA FINISH_PRINT_MAP_ALL
       JMP CHECK_AND_CALC_LOOP_ALL
       
FINISH_PRINT_MAP_ALL:
       CALL PRINT_BORDER 
       POP_REGS
       RET 
ENDP PRINT_MAP_ALL


RAND_NUM PROC ;OUTPUT IN AL  0<=NUM<=4
    MOV AX,MYRAND
    MOV BX,0ABCDH
    MUL BX
    ADD AX,0BDH 
    MUL BX
    SHR AX,1
    MOV MYRAND,AX
    
    MOV BX,6
    XOR DX,DX
    DIV BX
    MOV AL,DL    
RET
ENDP RAND_NUM 


RAND_START PROC
   MOV AH, 00h  ; interrupts to get system time        
   INT 1AH      ; CX:DX now hold number of clock ticks since midnight      
   MOV MYRAND,DX

   CALL RAND_NUM    
RET
ENDP RAND_START


NEW_SHAPE PROC
    ;CALL RAND_NUM
    MOV AL,[NRAND_SHAPE]
    MOV [RAND_SHAPE],AL
    
    ;CALL RAND_NUM 
    ;CALL MAKE_COLOR
    MOV AL,[NRAND_COLOR]
    MOV [RAND_COLOR],AL
    MOV [MOVING_COLOR],AL 
    
    ;CALL RAND_NUM
    MOV AL,[NRAND_ORI]
    MOV [RAND_ORI],AL
    
    MOV BX,0 
SELECT_NS: 
    CMP RAND_SHAPE,0
    JE TNS
    CMP RAND_SHAPE,1
    JE LINENS 
    CMP RAND_SHAPE,2
    JE LNS
    CMP RAND_SHAPE,3
    JE ZNS
    CMP RAND_SHAPE,4
    JE SQNS
    
TNS:
    CALL CALC_ORI
    MOV BX,AL
    MOV [MOVING_ORI],BX
    MOV CX,BX
    ADD CX,4
    PUSH BX 
    MOV BX,T_SHAPE+BX
    XOR BH,BH
    CMP BOARD+BX,1
    JE CANTNS
    MOV [MOVING_SHAPE],1
    MOV [MOVING_INDEX],BX 
    PUSH BX
    CALL GET_MOVING_PATTERN 
    POP BX
    MOV [MOVING_INDEX_ARRAY],BX
    MOV BOARD+BX,1
    MOV DX,RAND_COLOR
    MOV BOARDC+BX,DL
    MOV AX,BX 
OTHER3TNS:
    POP BX
    INC BX
    CMP BX,CX
    JE ENDNS
    PUSH BX
    MOV BX,T_SHAPE+BX
    XOR BH,BH
    ADD BX,AX 
    CMP BOARD+BX,1
    JE CANTNS
    MOV BOARD+BX,1      
    MOV DX,RAND_COLOR
    MOV BOARDC+BX,DL 
    JMP OTHER3TNS
                     
LINENS:
    CALL CALC_ORI
    MOV BX,AL 
    MOV [MOVING_ORI],BX
    MOV CX,BX
    ADD CX,4
    PUSH BX 
    MOV BX,LINE_SHAPE+BX
    XOR BH,BH
    CMP BOARD+BX,1
    JE CANTNS
    MOV [MOVING_SHAPE],2
    MOV [MOVING_INDEX],BX
    PUSH BX
    CALL GET_MOVING_PATTERN 
    POP BX
    MOV BOARD+BX,1
    MOV DX,RAND_COLOR
    MOV BOARDC+BX,DL
    MOV AX,BX 
OTHER3LINS:
    POP BX
    INC BX
    CMP BX,CX
    JE ENDNS
    PUSH BX
    MOV BX,LINE_SHAPE+BX
    XOR BH,BH
    ADD BX,AX
    CMP BOARD+BX,1
    JE CANTNS
    MOV BOARD+BX,1 
    MOV DX,RAND_COLOR
    MOV BOARDC+BX,DL 
    JMP OTHER3LINS

LNS:
    CALL CALC_ORI
    MOV BX,AL
    MOV [MOVING_ORI],BX
    MOV CX,BX
    ADD CX,4
    PUSH BX 
    MOV BX,L_SHAPE+BX
    XOR BH,BH
    CMP BOARD+BX,1
    JE CANTNS
    MOV [MOVING_SHAPE],3
    MOV [MOVING_INDEX],BX
    PUSH BX 
    CALL GET_MOVING_PATTERN 
    POP BX
    MOV BOARD+BX,1
    MOV DX,RAND_COLOR
    MOV BOARDC+BX,DL
    MOV AX,BX 
OTHER3LNS:
    POP BX
    INC BX
    CMP BX,CX
    JE ENDNS
    PUSH BX
    MOV BX,L_SHAPE+BX
    XOR BH,BH
    ADD BX,AX 
    CMP BOARD+BX,1
    JE CANTNS
    MOV BOARD+BX,1 
    MOV DX,RAND_COLOR
    MOV BOARDC+BX,DL 
    JMP OTHER3LNS

ZNS: 
    CALL CALC_ORI
    MOV BX,AL
    MOV [MOVING_ORI],BX
    MOV CX,BX
    ADD CX,4
    PUSH BX 
    MOV BX,Z_SHAPE+BX
    XOR BH,BH
    CMP BOARD+BX,1
    JE CANTNS
    MOV [MOVING_SHAPE],4
    MOV [MOVING_INDEX],BX 
    PUSH BX
    CALL GET_MOVING_PATTERN 
    POP BX
    MOV BOARD+BX,1
    MOV DX,RAND_COLOR
    MOV BOARDC+BX,DL
    MOV AX,BX 
OTHER3ZNS:
    POP BX
    INC BX
    CMP BX,CX
    JE ENDNS
    PUSH BX
    MOV BX,Z_SHAPE+BX
    XOR BH,BH
    ADD BX,AX
    CMP BOARD+BX,1
    JE CANTNS
    MOV BOARD+BX,1 
    MOV DX,RAND_COLOR
    MOV BOARDC+BX,DL 
    JMP OTHER3ZNS

SQNS:
    CALL CALC_ORI
    MOV BX,AL
    MOV [MOVING_ORI],BX
    MOV CX,BX
    ADD CX,4
    PUSH BX 
    MOV BX,SQUARE_SHAPE+BX
    XOR BH,BH
    CMP BOARD+BX,1
    JE CANTNS
    MOV [MOVING_SHAPE],5  
    MOV [MOVING_INDEX],BX  
    PUSH BX
    CALL GET_MOVING_PATTERN 
    POP BX
    MOV BOARD+BX,1
    MOV DX,RAND_COLOR
    MOV BOARDC+BX,DL
    MOV AX,BX 
OTHER3SQNS:
    POP BX
    INC BX
    CMP BX,CX
    JE ENDNS
    PUSH BX
    MOV BX,SQUARE_SHAPE+BX
    XOR BH,BH
    ADD BX,AX
    CMP BOARD+BX,1
    JE CANTNS
    MOV BOARD+BX,1 
    MOV DX,RAND_COLOR
    MOV BOARDC+BX,DL 
    JMP OTHER3SQNS
    
CANTNS:
    mov ax, 4c00h ; exit to operating system.
    int 21h
 
ENDNS:
RET
ENDP NEW_SHAPE 


CALC_ORI PROC  ;INDEX IN AL
    
CO_START:
    CMP RAND_ORI,0
    JE UP_ORI
    CMP RAND_ORI,1
    JE RIGHT_ORI
    CMP RAND_ORI,2
    JE DOWN_ORI
    CMP RAND_ORI,3
    JE LEFT_ORI
    CMP RAND_ORI,4
    CALL RAND_NUM
    MOV RAND_ORI,AL
    JMP CO_START 
    
UP_ORI:
    MOV AL,0
    JMP CO_FINISH 
    
RIGHT_ORI:
    MOV AL,4
    JMP CO_FINISH
    
DOWN_ORI:
    MOV AL,8
    JMP CO_FINISH

LEFT_ORI:
    MOV AL,12
    JMP CO_FINISH
     
CO_FINISH:
RET
ENDP CALC_ORI 

NCALC_ORI PROC  ;INDEX IN AL
    
NCO_START:
    CMP NRAND_ORI,0
    JE NUP_ORI
    CMP NRAND_ORI,1
    JE NRIGHT_ORI
    CMP NRAND_ORI,2
    JE NDOWN_ORI
    CMP NRAND_ORI,3
    JE NLEFT_ORI
    CMP NRAND_ORI,4
    CALL RAND_NUM
    MOV NRAND_ORI,AL
    JMP NCO_START 
    
NUP_ORI:
    MOV AL,0
    JMP NCO_FINISH 
    
NRIGHT_ORI:
    MOV AL,4
    JMP NCO_FINISH
    
NDOWN_ORI:
    MOV AL,8
    JMP NCO_FINISH

NLEFT_ORI:
    MOV AL,12
    JMP NCO_FINISH
     
NCO_FINISH:
RET
ENDP NCALC_ORI 


NNCALC_ORI PROC  ;INDEX IN AL
    
NNCO_START:
    CMP NNRAND_ORI,0
    JE NNUP_ORI
    CMP NNRAND_ORI,1
    JE NNRIGHT_ORI
    CMP NNRAND_ORI,2
    JE NNDOWN_ORI
    CMP NNRAND_ORI,3
    JE NNLEFT_ORI
    CMP NNRAND_ORI,4
    CALL RAND_NUM
    MOV NNRAND_ORI,AL
    JMP NNCO_START 
    
NNUP_ORI:
    MOV AL,0
    JMP NNCO_FINISH 
    
NNRIGHT_ORI:
    MOV AL,4
    JMP NNCO_FINISH
    
NNDOWN_ORI:
    MOV AL,8
    JMP NNCO_FINISH

NNLEFT_ORI:
    MOV AL,12
    JMP NNCO_FINISH
     
NNCO_FINISH:
RET
ENDP NNCALC_ORI

MAKE_COLOR PROC ;INPUT>>AL OUTPUT>>AL  
    CMP AL,0
    JE BLUE
    CMP AL,1
    JE GREEN
    CMP AL,2
    JE YELLOW
    CMP AL,3
    JE RED
    CMP AL,4
    JE CYAN
    JMP FINISHMC
    
BLUE:
    MOV AL,9
    JMP FINISHMC
GREEN:
    MOV AL,0AH
    JMP FINISHMC
YELLOW:
    MOV AL,0EH
    JMP FINISHMC
RED:
    MOV AL,0CH
    JMP FINISHMC
CYAN:
    MOV AL,0BH
    JMP FINISHMC

FINISHMC:
    
RET
ENDP MAKE_COLOR  


MAKE_DES PROC   ;AX>>OFFSET  if offset=2 then rotate
    CMP AX,2
    JE MD_ROTATE
    MOV CX,0
    MDL:
    CMP CX,8
    JE MD_END 
    MOV BX,CX 
    MOV DX,MOVING_INDEX_ARRAY+BX
    ADD DX,AX ;DX = NEW DES
    MOV DESTINATION_ARRAY+BX,DX
    INC CX
    INC CX
    JMP MDL 
    
    MD_ROTATE:
    MOV AX,[MOVING_ORI]
    MOV DX,[MOVING_INDEX] ;DX=STARTING INDEX OF MOVING
    ADD AX,4
    CMP AX,16
    JNE MDRC
    MOV AX,0
    
    MDRC:
    MOV [MOVING_ORI],AX
    MOV BX,0
    MOV DESTINATION_ARRAY+BX,DX
      
    MOV CX,1
    
    MDRCL:
    CMP CX,4
    JE MD_END
    MOV BX,CX
    ADD BX,AX
    MOV BX,MOVING_PATTERN+BX 
    XOR BH,BH
    PUSH DX
    ADD DX,BX
    MOV BX,CX
    SHL BX,1 
    MOV DESTINATION_ARRAY+BX,DX
    POP DX
    INC CX
    JMP MDRCL
       
MD_END:    
RET
ENDP MAKE_DES


CALC_INDEX PROC
    MOV BX,0
    MOV AX,[MOVING_INDEX]
    MOV MOVING_INDEX_ARRAY+BX,AX
    MOV DX,[MOVING_ORI]
    MOV BX,[MOVING_SHAPE]
    CMP BX,1
    JE CI_T
    CMP BX,2
    JE CI_LINE
    CMP BX,3
    JE CI_L
    CMP BX,4
    JE CI_Z
    CMP BX,5
    JE CI_SQ
    JMP CI_END
  
    CI_T:
    MOV BX,1
    ADD BX,DX 
    MOV BX,T_SHAPE+BX
    XOR BH,BH
    ADD BX,AX
    MOV CX,BX
    MOV BX,2
    MOV MOVING_INDEX_ARRAY+ BX,CX 
    MOV BX,2
    ADD BX,DX
    MOV BX,T_SHAPE+BX
    XOR BH,BH
    ADD BX,AX
    MOV CX,BX
    MOV BX,4
    MOV MOVING_INDEX_ARRAY+ BX,CX 
    MOV BX,3
    ADD BX,DX
    MOV BX,T_SHAPE+BX
    XOR BH,BH
    ADD BX,AX
    MOV CX,BX
    MOV BX,6
    MOV MOVING_INDEX_ARRAY+ BX,CX 
    JMP CI_END 
   
    CI_LINE:
    MOV BX,1
    ADD BX,DX 
    MOV BX,LINE_SHAPE+BX
    XOR BH,BH
    ADD BX,AX
    MOV CX,BX
    MOV BX,2
    MOV MOVING_INDEX_ARRAY+ BX,CX 
    MOV BX,2
    ADD BX,DX
    MOV BX,LINE_SHAPE+BX
    XOR BH,BH
    ADD BX,AX
    MOV CX,BX
    MOV BX,4
    MOV MOVING_INDEX_ARRAY+ BX,CX 
    MOV BX,3
    ADD BX,DX
    MOV BX,LINE_SHAPE+BX
    XOR BH,BH
    ADD BX,AX
    MOV CX,BX
    MOV BX,6
    MOV MOVING_INDEX_ARRAY+ BX,CX 
    JMP CI_END
    
    CI_L: 
    MOV BX,1
    ADD BX,DX 
    MOV BX,L_SHAPE+BX
    XOR BH,BH
    ADD BX,AX
    MOV CX,BX
    MOV BX,2
    MOV MOVING_INDEX_ARRAY+ BX,CX 
    MOV BX,2
    ADD BX,DX
    MOV BX,L_SHAPE+BX
    XOR BH,BH
    ADD BX,AX
    MOV CX,BX
    MOV BX,4
    MOV MOVING_INDEX_ARRAY+ BX,CX 
    MOV BX,3
    ADD BX,DX
    MOV BX,L_SHAPE+BX
    XOR BH,BH
    ADD BX,AX
    MOV CX,BX
    MOV BX,6
    MOV MOVING_INDEX_ARRAY+ BX,CX 
    JMP CI_END
    CI_Z:
    MOV BX,1
    ADD BX,DX 
    MOV BX,Z_SHAPE+BX
    XOR BH,BH
    ADD BX,AX
    MOV CX,BX
    MOV BX,2
    MOV MOVING_INDEX_ARRAY+ BX,CX 
    MOV BX,2
    ADD BX,DX
    MOV BX,Z_SHAPE+BX
    XOR BH,BH
    ADD BX,AX
    MOV CX,BX
    MOV BX,4
    MOV MOVING_INDEX_ARRAY+ BX,CX 
    MOV BX,3
    ADD BX,DX
    MOV BX,Z_SHAPE+BX
    XOR BH,BH
    ADD BX,AX
    MOV CX,BX
    MOV BX,6
    MOV MOVING_INDEX_ARRAY+ BX,CX 
    JMP CI_END
    
    CI_SQ: 
    MOV BX,1
    ADD BX,DX 
    MOV BX,SQUARE_SHAPE+BX
    XOR BH,BH
    ADD BX,AX
    MOV CX,BX
    MOV BX,2
    MOV MOVING_INDEX_ARRAY+ BX,CX 
    MOV BX,2
    ADD BX,DX
    MOV BX,SQUARE_SHAPE+BX
    XOR BH,BH
    ADD BX,AX
    MOV CX,BX
    MOV BX,4
    MOV MOVING_INDEX_ARRAY+ BX,CX 
    MOV BX,3
    ADD BX,DX
    MOV BX,SQUARE_SHAPE+BX
    XOR BH,BH
    ADD BX,AX
    MOV CX,BX
    MOV BX,6
    MOV MOVING_INDEX_ARRAY+ BX,CX 
    JMP CI_END  
     
CI_END:        
RET
ENDP CALC_INDEX  

MOVE_CONTROL PROC  ;AX>>OFFSET  if offset=2 then rotate 
    MOV [MYOFFSET],AX
    CALL MAKE_DES 
    MOV CX,0
    INC CX
    MOV [MYBOOL],CX
    DEC CX
    MC_TWO: ;PUT 2 IN ALL OF MOVING 
    CMP CX,8
    JE END_MC_TWO
    MOV BX,CX
    MOV BX, MOVING_INDEX_ARRAY+BX
    MOV BOARD+BX,2
    MOV BOARDC+BX,0
    INC CX
    INC CX
    JMP MC_TWO
    END_MC_TWO:
    CALL CHECK_DES 
    MOV AX,[MYBOOL]
    CMP AX,1
    JE MC_NEW 
    MC_BEFORE:
    MOV CX,0
    MCB_L:
    CMP CX,8
    JE MC_END
    MOV BX,CX
    MOV BX,MOVING_INDEX_ARRAY+BX
    MOV BOARD+BX,1
    MOV DX,[MOVING_COLOR]
    MOV BOARDC+BX,DL
    INC CX
    INC CX
    JMP MCB_L

    MC_NEW:
    MOV CX,[MOVING_INDEX]
    MOV AX,[MYOFFSET]
    CMP AX,2
    JE ISR
    ADD AX,CX
    MOV [MOVING_INDEX],AX 
    ISR:
    MOV CX,0
    MCN_L:
    CMP CX,8
    JE MC_END 
    MOV BX,CX
    MOV DX,DESTINATION_ARRAY+BX
    MOV MOVING_INDEX_ARRAY+BX,DX
    MOV BX,DX
    MOV BOARD+BX,1 
    MOV DX,[MOVING_COLOR]
    MOV BOARDC+BX,DL
    INC CX
    INC CX
    JMP MCN_L 
         
MC_END:       
RET
ENDP MOVE_CONTROL  

CHECK_DES PROC  
    
    MOV BX,0
    CDL:
    CMP BX,8
    JE END_CDL
    PUSH BX 
    MOV CX,MOVING_INDEX_ARRAY+BX
    MOV BX,DESTINATION_ARRAY+BX
    CMP BOARD+BX,1
    JE CD_FALSE
    XOR DX,DX
    MOV AX,BX
    MOV BX,10
    DIV BX
    MOV AX,CX
    MOV CX,DX
    XOR DX,DX
    DIV BX
    SUB DX,CX
    CMP DX,-4
    JE CD_DONE
    CMP DX,-3
    JE CD_DONE
    CMP DX,-2
    JE CD_DONE
    CMP DX,-1
    JE CD_DONE 
    CMP DX,0
    JE CD_DONE  
    CMP DX,1
    JE CD_DONE   
    CMP DX,2
    JE CD_DONE
    CMP DX,3
    JE CD_DONE
    CMP DX,4
    JE CD_DONE
    JMP CD_FALSE

    CD_FALSE:
    POP BX
    MOV [MYBOOL],0
    JMP END_CDL 
    
    CD_DONE:
    POP BX
    INC BX
    INC BX 
    JMP CDL
    END_CDL:
RET
ENDP CHECK_DES 

GET_MOVING_PATTERN PROC    ; call it in newing shape 
    MOV DX,[MOVING_SHAPE]
    CMP DX,0
    JE GMP_END
    CMP DX,1
    JE GMP_T
    CMP DX,2
    JE GMP_LINE
    CMP DX,3
    JE GMP_L
    CMP DX,4
    JE GMP_Z
    CMP DX,5
    JE GMP_SQ
    
    GMP_T:
    MOV BX,0
    GMP_TL:
    CMP BX,16
    JE GMP_END
    MOV AX,T_SHAPE+BX 
    XOR AH,AH
    MOV MOVING_PATTERN+BX,AX
    INC BX
    JMP GMP_TL
    
    GMP_LINE:
    MOV BX,0
    GMP_LINEL:
    CMP BX,16
    JE GMP_END
    MOV AX,LINE_SHAPE+BX 
    XOR AH,AH
    MOV MOVING_PATTERN+BX,AX
    INC BX
    JMP GMP_LINEL
    
    GMP_L:
    MOV BX,0
    GMP_LL:
    CMP BX,16
    JE GMP_END
    MOV AX,L_SHAPE+BX 
    XOR AH,AH
    MOV MOVING_PATTERN+BX,AX
    INC BX
    JMP GMP_LL
    
    GMP_Z:
    MOV BX,0
    GMP_ZL:
    CMP BX,16
    JE GMP_END
    MOV AX,Z_SHAPE+BX 
    XOR AH,AH
    MOV MOVING_PATTERN+BX,AX
    INC BX
    JMP GMP_ZL
    
    GMP_SQ:
    MOV BX,0
    GMP_SQL:
    CMP BX,16
    JE GMP_END
    MOV AX,SQUARE_SHAPE+BX 
    XOR AH,AH
    MOV MOVING_PATTERN+BX,AX
    INC BX
    JMP GMP_SQL
    
    GMP_END:
RET
ENDP GET_MOVING_PATTERN   

MOVE_SHAPE_DOWN PROC  
    MOV AX,10
    PUSH [MOVING_INDEX]
    CALL MOVE_CONTROL
    POP AX
    CMP AX,[MOVING_INDEX]
    JE MSDFINAL
    JMP MSD_END
    MSDFINAL:
    MOV [BLOCK_FINISHED],1 
    MSD_END: 
RET
ENDP MOVE_SHAPE_DOWN
    
LINE_UP PROC  
     MOV CX,0
     LU_LOOP:
     CMP CX,10
     JE LU_END 
     PUSH CX
     MOV AX, [MY_LINE]
     MOV BX, 10
     MUL BX
     ADD AX, CX
     MOV BX, AX 
     MOV CH, BOARD + BX
     MOV CL,BOARDC+BX 
     SUB BX, 10
     MOV DH, BOARD + BX
     MOV DL, BOARDC+BX     
     MOV BOARD + BX, CH
     MOV BOARDC + BX, CL 
     ADD BX, 10
     MOV BOARD + BX, DH
     MOV BOARDC + BX, DL    
     POP CX
     INC CX
     JMP LU_LOOP 
     
     LU_END:
RET
ENDP LINE_UP  

ZERO_LINE PROC
    MOV AX,[MY_LINE] 
    MOV BX,10
    MUL BX
    MOV CX,0
    ZL_LOOP:
    CMP CX,10
    JE ZL_END
    MOV BX,CX 
    ADD BX,AX
    MOV BOARD+BX,2
    MOV BOARDC+BX,0
    INC CX
    JMP ZL_LOOP 
    ZL_END:

RET
ENDP ZERO_LINE 

LINE_FULL PROC 
    MOV AX,[MY_LINE]
    MOV BX,10
    MUL BX
    XOR CX,CX
    
    LF_LOOP:
    CMP CX,10
    JE ALL_ONE
    MOV BX,CX
    ADD BX,AX
    CMP BOARD+BX,1
    JNE NALL_ONE
    INC CX
    JMP LF_LOOP
    
    ALL_ONE:
    MOV [ALLONE_LINE],1
    JMP LF_END
    
    NALL_ONE:
    MOV [ALLONE_LINE],0
    
    LF_END:
    
RET
ENDP LINE_FULL 

CHECK_BOARD PROC 
    MOV CX,19
    CB_L1:
    CMP CX,0
    JE CB_END
    MOV [MY_LINE],CX
    PUSH CX
    CALL LINE_FULL 
    POP CX
    CMP [ALLONE_LINE],1
    JE LINE_IS_ONE
    JMP LINE_NOT_ONE
    
    LINE_IS_ONE:
    PUSH CX
    CALL ZERO_LINE
    ADD [NEWLINE],1
    POP CX
    PUSH CX
    
    SWAP_TO_BASE:
    CMP CX,0
    JE STB_END
    MOV [MY_LINE],CX
    PUSH CX
    CALL LINE_UP
    POP CX
    DEC CX
    JMP SWAP_TO_BASE
    
    STB_END:
    POP CX
    
    JMP CB_L1 
    
    LINE_NOT_ONE:
    DEC CX
    JMP CB_L1  
    
    CB_END:
    MOV AX,[NEWLINE] 
    CMP AX,1
    JE JONE
    MOV BX,20
    MUL BX
    ADD [SCORE],AX
    JMP FF
    
    JONE:
    ADD [SCORE],10
    
    FF:
    MOV [NEWLINE],0
RET
ENDP CHECK_BOARD


SHOW_SCORE PROC 
   MOV AH,2
   MOV BH,0
   MOV DH,6
   MOV DL,5
   INT 10H
   MOV AX,[SCORE] 
   mov cx,0 
   mov dx,0 
   
   CMP AX,0
   JE SHOW0
   
 PRINTLOOP: 
  
  CMP AX,0 
  JE PRINTLOOPFINAL  
  
  MOV BX,10   
  DIV BX     
  
 
  PUSH DX    
  
  INC CX    
  
  XOR DX,DX 
  JMP PRINTLOOP 
  
 PRINTLOOPFINAL: 
 
  CMP CX,0 
  JE ENDPROC
   
  POP DX 
  ADD DX,48 
  
  MOV AH,02h 
  INT 21h 
         
  DEC CX 
  JMP PRINTLOOPFINAL
  
  SHOW0:
  MOV DX,'0'
  MOV AH,2
  INT 21H
   
ENDPROC: 
    
RET
ENDP SHOW_SCORE 



PRINT_UI PROC
    DRAW_ROW 40,25,80,0FH
    DRAW_ROW 65,25,80,0FH
    DRAW_COL 25,40,65,0FH
    DRAW_COL 80,40,65,0FH 
    MOV AH,2
    MOV BH,0
    MOV DH,3
    MOV DL,28
    INT 10H
    LEA DX,NEXT
    MOV AH,09H
    INT 21H
    
    MOV AH,2
    MOV BH,0
    MOV DH,7
    MOV DL,36
    INT 10H
    LEA DX,NUM1
    MOV AH,09H
    INT 21H
    
    MOV AH,2
    MOV BH,0
    MOV DH,15
    MOV DL,36
    INT 10H
    LEA DX,NUM2
    MOV AH,09H
    INT 21H
    
    MOV AH,2
    MOV BH,0
    MOV DH,11
    MOV DL,3
    INT 10H
    LEA DX,WROTATE
    MOV AH,09H
    INT 21H
    
    MOV AH,2
    MOV BH,0
    MOV DH,13
    MOV DL,3
    INT 10H
    LEA DX,DRIGHT
    MOV AH,09H
    INT 21H
    
    MOV AH,2
    MOV BH,0
    MOV DH,15
    MOV DL,3
    INT 10H
    LEA DX,SDOWN
    MOV AH,09H
    INT 21H 
    
    MOV AH,2
    MOV BH,0
    MOV DH,17
    MOV DL,3
    INT 10H
    LEA DX,ALEFT
    MOV AH,09H
    INT 21H
    
    MOV AH,2
    MOV BH,0
    MOV DH,19
    MOV DL,3
    INT 10H
    LEA DX,FDROP
    MOV AH,09H
    INT 21H
    
    MOV AH,2
    MOV BH,0
    MOV DH,21
    MOV DL,3
    INT 10H
    LEA DX,QQUIT
    MOV AH,09H
    INT 21H
    
  
RET
ENDP PRINT_UI  


NNEW_SHAPE PROC 
    MOV BX,0
    NNZ:
    CMP BX,16
    JE ZZN
    MOV NBOARDC+BX,0
    INC BX
    JMP NNZ
    
    ZZN:
    ;CALL RAND_NUM
    MOV AL,[NNRAND_SHAPE]
    MOV [NRAND_SHAPE],AL
    
    ;CALL RAND_NUM
    MOV AL,[NNRAND_COLOR] 
    CALL MAKE_COLOR
    MOV [NRAND_COLOR],AL
     
    
    ;CALL RAND_NUM 
    MOV AL,[NNRAND_ORI]
    MOV [NRAND_ORI],AL
    
    MOV BX,0 
NSELECT_NS: 
    CMP NRAND_SHAPE,0
    JE NTNS
    CMP NRAND_SHAPE,1
    JE NLINENS 
    CMP NRAND_SHAPE,2
    JE NLNS
    CMP NRAND_SHAPE,3
    JE NZNS
    CMP NRAND_SHAPE,4
    JE NSQNS
    
NTNS:
    CALL NCALC_ORI
    MOV BX,AL
    
    MOV CX,BX
    ADD CX,4
    PUSH BX 
    MOV BX,NT_SHAPE+BX
    XOR BH,BH
    MOV NBOARD+BX,1
    MOV DX,NRAND_COLOR
    MOV NBOARDC+BX,DL
    MOV AX,BX 
NOTHER3TNS:
    POP BX
    INC BX
    CMP BX,CX
    JE NENDNS
    PUSH BX
    MOV BX,NT_SHAPE+BX
    XOR BH,BH
    ADD BX,AX 
    MOV NBOARD+BX,1      
    MOV DX,NRAND_COLOR
    MOV NBOARDC+BX,DL 
    JMP NOTHER3TNS
                     
NLINENS:
    CALL NCALC_ORI
    MOV BX,AL 
    
    MOV CX,BX
    ADD CX,4
    PUSH BX 
    MOV BX,NLINE_SHAPE+BX
    XOR BH,BH
    MOV NBOARD+BX,1
    MOV DX,NRAND_COLOR
    MOV NBOARDC+BX,DL
    MOV AX,BX 
NOTHER3LINS:
    POP BX
    INC BX
    CMP BX,CX
    JE NENDNS
    PUSH BX
    MOV BX,NLINE_SHAPE+BX
    XOR BH,BH
    ADD BX,AX
    MOV NBOARD+BX,1 
    MOV DX,NRAND_COLOR
    MOV NBOARDC+BX,DL 
    JMP NOTHER3LINS

NLNS:
    CALL NCALC_ORI
    MOV BX,AL
    
    MOV CX,BX
    ADD CX,4
    PUSH BX 
    MOV BX,NL_SHAPE+BX
    XOR BH,BH
    MOV NBOARD+BX,1
    MOV DX,NRAND_COLOR
    MOV NBOARDC+BX,DL
    MOV AX,BX 
NOTHER3LNS:
    POP BX
    INC BX
    CMP BX,CX
    JE NENDNS
    PUSH BX
    MOV BX,NL_SHAPE+BX
    XOR BH,BH
    ADD BX,AX 
    MOV NBOARD+BX,1 
    MOV DX,NRAND_COLOR
    MOV NBOARDC+BX,DL 
    JMP NOTHER3LNS

NZNS: 
    CALL NCALC_ORI
    MOV BX,AL
    
    MOV CX,BX
    ADD CX,4
    PUSH BX 
    MOV BX,NZ_SHAPE+BX
    XOR BH,BH
    MOV NBOARD+BX,1
    MOV DX,NRAND_COLOR
    MOV NBOARDC+BX,DL
    MOV AX,BX 
NOTHER3ZNS:
    POP BX
    INC BX
    CMP BX,CX
    JE NENDNS
    PUSH BX
    MOV BX,NZ_SHAPE+BX
    XOR BH,BH
    ADD BX,AX
    MOV NBOARD+BX,1 
    MOV DX,NRAND_COLOR
    MOV NBOARDC+BX,DL 
    JMP NOTHER3ZNS

NSQNS:
    CALL NCALC_ORI
    MOV BX,AL
    
    MOV CX,BX
    ADD CX,4
    PUSH BX 
    MOV BX,NSQUARE_SHAPE+BX
    XOR BH,BH
    MOV NBOARD+BX,1
    MOV DX,NRAND_COLOR
    MOV NBOARDC+BX,DL
    MOV AX,BX 
NOTHER3SQNS:
    POP BX
    INC BX
    CMP BX,CX
    JE NENDNS
    PUSH BX
    MOV BX,NSQUARE_SHAPE+BX
    XOR BH,BH
    ADD BX,AX
 
    MOV NBOARD+BX,1 
    MOV DX,NRAND_COLOR
    MOV NBOARDC+BX,DL 
    JMP NOTHER3SQNS

 
NENDNS:
RET
ENDP NNEW_SHAPE


NPRINT_MAP_ALL PROC 
       PUSH_REGS
       MOV BX,0  
NCHECK_AND_CALC_LOOP_ALL: 
       XOR DX,DX
       MOV CX,4
       MOV AX,BX
       DIV CX 
       MOV [THIS_COL], DX
       MOV CX, 10 
       MUL CX
       ADD AX,50
       MOV [THIS_ROW],AX
       MOV AX,[THIS_COL]
       MOV CX,10
       MUL CX
       ADD AX, 240
       MOV [THIS_COL], AX
       PUSH_REGS
       DRAW_BLOCK [THIS_ROW],[THIS_COL], NBOARDC+BX
       POP_REGS
NCONTINUE_MAP_ALL:
       INC BX
       CMP BX, 15
       JA NFINISH_PRINT_MAP_ALL
       JMP NCHECK_AND_CALC_LOOP_ALL
       
NFINISH_PRINT_MAP_ALL: 
       POP_REGS
       RET 
ENDP NPRINT_MAP_ALL 

NNNEW_SHAPE PROC 
    MOV BX,0
    NNNZ:
    CMP BX,16
    JE ZZNN
    MOV NNBOARDC+BX,0
    INC BX
    JMP NNNZ
    
    ZZNN:
    CALL RAND_NUM
    MOV [NNRAND_SHAPE],AL
    
    CALL RAND_NUM 
    CALL MAKE_COLOR
    MOV [NNRAND_COLOR],AL
     
    
    CALL RAND_NUM
    MOV [NNRAND_ORI],AL
    
    MOV BX,0 
NNSELECT_NS: 
    CMP NNRAND_SHAPE,0
    JE NNTNS
    CMP NNRAND_SHAPE,1
    JE NNLINENS 
    CMP NNRAND_SHAPE,2
    JE NNLNS
    CMP NNRAND_SHAPE,3
    JE NNZNS
    CMP NNRAND_SHAPE,4
    JE NNSQNS
    
NNTNS:
    CALL NNCALC_ORI
    MOV BX,AL
    
    MOV CX,BX
    ADD CX,4
    PUSH BX 
    MOV BX,NT_SHAPE+BX
    XOR BH,BH
    MOV NNBOARD+BX,1
    MOV DX,NNRAND_COLOR
    MOV NNBOARDC+BX,DL
    MOV AX,BX 
NNOTHER3TNS:
    POP BX
    INC BX
    CMP BX,CX
    JE NNENDNS
    PUSH BX
    MOV BX,NT_SHAPE+BX
    XOR BH,BH
    ADD BX,AX 
    MOV NNBOARD+BX,1      
    MOV DX,NNRAND_COLOR
    MOV NNBOARDC+BX,DL 
    JMP NNOTHER3TNS
                     
NNLINENS:
    CALL NNCALC_ORI
    MOV BX,AL 
    
    MOV CX,BX
    ADD CX,4
    PUSH BX 
    MOV BX,NLINE_SHAPE+BX
    XOR BH,BH
    MOV NNBOARD+BX,1
    MOV DX,NNRAND_COLOR
    MOV NNBOARDC+BX,DL
    MOV AX,BX 
NNOTHER3LINS:
    POP BX
    INC BX
    CMP BX,CX
    JE NNENDNS
    PUSH BX
    MOV BX,NLINE_SHAPE+BX
    XOR BH,BH
    ADD BX,AX
    MOV NNBOARD+BX,1 
    MOV DX,NNRAND_COLOR
    MOV NNBOARDC+BX,DL 
    JMP NNOTHER3LINS

NNLNS:
    CALL NNCALC_ORI
    MOV BX,AL
    
    MOV CX,BX
    ADD CX,4
    PUSH BX 
    MOV BX,NL_SHAPE+BX
    XOR BH,BH
    MOV NNBOARD+BX,1
    MOV DX,NNRAND_COLOR
    MOV NNBOARDC+BX,DL
    MOV AX,BX 
NNOTHER3LNS:
    POP BX
    INC BX
    CMP BX,CX
    JE NNENDNS
    PUSH BX
    MOV BX,NL_SHAPE+BX
    XOR BH,BH
    ADD BX,AX 
    MOV NNBOARD+BX,1 
    MOV DX,NNRAND_COLOR
    MOV NNBOARDC+BX,DL 
    JMP NNOTHER3LNS

NNZNS: 
    CALL NNCALC_ORI
    MOV BX,AL
    
    MOV CX,BX
    ADD CX,4
    PUSH BX 
    MOV BX,NZ_SHAPE+BX
    XOR BH,BH
    MOV NNBOARD+BX,1
    MOV DX,NNRAND_COLOR
    MOV NNBOARDC+BX,DL
    MOV AX,BX 
NNOTHER3ZNS:
    POP BX
    INC BX
    CMP BX,CX
    JE NNENDNS
    PUSH BX
    MOV BX,NZ_SHAPE+BX
    XOR BH,BH
    ADD BX,AX
    MOV NNBOARD+BX,1 
    MOV DX,NNRAND_COLOR
    MOV NNBOARDC+BX,DL 
    JMP NNOTHER3ZNS

NNSQNS:
    CALL NNCALC_ORI
    MOV BX,AL
    
    MOV CX,BX
    ADD CX,4
    PUSH BX 
    MOV BX,NSQUARE_SHAPE+BX
    XOR BH,BH
    MOV NNBOARD+BX,1
    MOV DX,NNRAND_COLOR
    MOV NNBOARDC+BX,DL
    MOV AX,BX 
NNOTHER3SQNS:
    POP BX
    INC BX
    CMP BX,CX
    JE NNENDNS
    PUSH BX
    MOV BX,NSQUARE_SHAPE+BX
    XOR BH,BH
    ADD BX,AX
 
    MOV NNBOARD+BX,1 
    MOV DX,NNRAND_COLOR
    MOV NNBOARDC+BX,DL 
    JMP NNOTHER3SQNS

 
NNENDNS:
RET
ENDP NNNEW_SHAPE


NNPRINT_MAP_ALL PROC 
       PUSH_REGS
       MOV BX,0  
NNCHECK_AND_CALC_LOOP_ALL: 
       XOR DX,DX
       MOV CX,4
       MOV AX,BX
       DIV CX 
       MOV [THIS_COL], DX
       MOV CX, 10 
       MUL CX
       ADD AX,110
       MOV [THIS_ROW],AX
       MOV AX,[THIS_COL]
       MOV CX,10
       MUL CX
       ADD AX, 240
       MOV [THIS_COL], AX
       PUSH_REGS
       DRAW_BLOCK [THIS_ROW],[THIS_COL], NNBOARDC+BX
       POP_REGS
NNCONTINUE_MAP_ALL:
       INC BX
       CMP BX, 15
       JA NNFINISH_PRINT_MAP_ALL
       JMP NNCHECK_AND_CALC_LOOP_ALL
       
NNFINISH_PRINT_MAP_ALL: 
       POP_REGS
       RET 
ENDP NNPRINT_MAP_ALL
     


END MAIN