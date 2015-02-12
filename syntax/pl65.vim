" Vim syntax file
" Language: PL65 for Atari 8-bit computers
" Maintainer: Michael Sternberg
" Latest Revision: 20 November 2014

if version < 600
    syntax clear
elseif exists("b:current_syntax")
    finish
endif

" Todo.
syn keyword pl65Todo TODO FIXME XXX DEBUG NOTE

" pl65CommentGroup allows adding matches for special things in comments.
syn cluster pl65CommentGroup  contains=pl65Todo

" Keywords
syn keyword pl65Command PROC FUNC INTERRUPT BODY CONST LINK INCLUDE 
syn keyword pl65Command BEGIN END LINK FORWARD MAIN

syn keyword pl65Type BYTE INT STRING POINTER BASED

syn keyword pl65Conditional IF THEN ELSE ENDIF 

syn keyword pl65State WHILE DO ENDWHILE REPEAT FOREVER UNTIL
syn keyword pl65State FOR TO STEP NEXT DOWNTO
syn keyword pl65State CASE OF ENDOF ENDCASE GOTO TRAP NOTRAP RETURN

syn keyword pl65Mneumonic BNE BEQ BMI BPL BCC BCS BVS BVC JMP JSR
syn keyword pl65Mneumonic INX DEX INY DEY PHA PLA PHP PLP ASLA LSA 
syn keyword pl65Mneumonic RORA ROLA TSX TXA TAX TYA TAY NOP BRK RTS 
syn keyword pl65Mneumonic RTI SED CLD SEC CLC SEI CLI CLV LDA STA LDX 
syn keyword pl65Mneumonic STX LDY STY CMP CPX CPY AND ORA EOR BIT ASL 
syn keyword pl65Mneumonic LSR ROL ROR INC DEC ADC SBC 

syn region pl65String start='"' end='"'

syn match pl65Comment "!.*$"
syn match pl65Comment "!.*!$"

syn match   pl65Label          display "[:]\<\w\+\>"
syn match   cexprNumber        display "\<\d\+\>"
syn match   cexprNumberHex     display "[$]\<[0123456789ABCDEFabcdef]\+\>"
syn match   cexprNumberBin     display "[%]\<[01][01][01][01][01][01][01][01]\>"

syn region  pl65CommentL start="!" skip="\\$" end="$" keepend contains=@pl65CommentGroup,@Spell

" Define the default highlighting.
" For version 5.x and earlier, only when not done already.
" For version 5.8 and later, only when and item doesn't have highlighting
" yet.
if version >= 508 || !exists("did_pl1_syn_inits")
    if version < 508
        let did_pl1_syn_inits = 1
        command -nargs=+ HiLink hi link <args>
    else
        command -nargs=+ HiLink hi def link <args>
    endif

    hi def link pl65Command         PreProc
    hi def link pl65Type            Type
    hi def link cexprNumber         Number
    hi def link pl65State           Statement
    hi def link pl65Mneumonic       Statement
    hi def link pl65Conditional     Statement
    hi def link pl65Todo            Todo
    hi def link pl65Label           Label
    hi def link cexprNumberHex      Special
    hi def link cexprNumberBin      Special
    hi def link pl65String          String
    hi def link pl65CommentL        pl65Comment
    hi def link pl65Comment         Comment
    
    delcommand HiLink
endif

let b:current_syntax = "pl65"
