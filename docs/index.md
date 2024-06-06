# LBOD
## Legacy Bios Operating system Development language
#### Written by Lilly H. St Claire

## Introduction

This document describes LBOD a programming language designed
to make the development of an Operating System (OS) in 8086
machine code (for the use of legacy bios machines) easier.
Though I can see the utility of the language in other contexts.
LBOD is designed so that it is possible to interpret generated
assembly from a human standpoint.

For the remainding of this document I will be representing
the syntax of values using Augmented Backus Naur form (ABNF)
which can be described (using itself as follows)

```ABNF
; Taken from https://www.rfc-editor.org/rfc/rfc5234
rulelist        = 1*(rule / (*c-wsp c-nl))
rule            = rulename defined-as elements c-nl
            ; continues if next line starts with whitespace
rulename        = ALPHA *(ALPHA / DIGIT / "-")
defined-as      = *c-wsp ("=" / "=/") *c-wsp
            ; basic rules definition and incremental alternatives
elements        = alternation *c-wsp
c-wsp           = WSP / (c-nl WSP)
c-nl            = comment / CRLF
            ; comment or newline
comment         = ";" *(WSP / VCHAR) CRLF
alternation     = concatenation *(*c-wsp "/" *c-wsp concatenation)
concatenation   = repetition *(1*c-wsp repetition)
repetition      = [repeat] element
repeat          = 1*DIGIT / (*DIGIT / (*DIGIT "*" *DIGIT))
element         = rulename / group / option / char-val / num-val / prose-val
group           = "(" *c-wsp alternation *c-wsp ")"
option          = "[" *c-wsp alternation *c-wsp "]"
char-val        = DQUOTE *(%x20-21 / %x23-7E) DQUOTE
            ; quoted string of SP and VCHAR without DQUOTE
num-val         = "%" (bin-val / dec-val / hex-val)
bin-val         = "b" 1*BIT [1*("." 1*BIT) / ("-" 1*BIT)]
            ; series of concatenated bit values or single ONEOF range
dec-val         = "d" 1*DIGIT [1*("." 1*DIGIT) / ("-" 1*digit)]
hex-val         = "x" 1*HEXDIG [1*("." 1*HEXDIG) / ("-" 1*HEXDIG)]
prose-val       = "<" *(%x20-3D / %x3F-7E) ">"
            ; bracket string of SP and VCHAR without angles prose description, to be used as last resort
```

where the following are defined (as apart of Core ABNF described in
[Augmented BNF for Syntax Specifiactions: ABNF](https://www.rfc-editor.org/rfc/rfc5234#appendix-B.2)).
```ABNF
ALPHA           = %x41-5A / %x61-7A
BIT             = "0" / "1"
CHAR            = %x01-7F
CR              = %x0D
CRLF            = CR LF
CTL             = %x00-1F / %x7F
DIGIT           = %x30-39
DQUOTE          = %x22
HEXDIG          = DIGIT / "A" / "B" / "C" / "D" / "E" / "F"
HTAB            = %x09
LF              = %x0A
LWSP            = *(WSP / CRLF WSP)
OCTET           = %x00-FF
SP              = %x20
VCHAR           = %x21-7E
WSP             = SP / HTAB
```

The expected compilation output will be described with a series
of symbols (which we will call agnostic assembly (aASM)) that
maps rather closely to 8086 assembly while leaving room for
optimization. The following section will describe aASM.

## aASM

```ABNF
FSP = WSP / NL
program = *(token / FSP)
token = literal / space / label / move / add / sub / mul / div / and / or / xor / not / jmp / jz / jnz
            / jg / jng / jl / jnl / push / pop / sysfn / compare / placeholder
literal = "LIT" *(VCHAR / FSP) "ELIT"
label = "@" *1ALPHA (SP / NL / HTAB)
space = "[" *FSP ("$" *2HEGDIG) / string)
            *(*FSP "," *FSP ("$" *2HEXDIG) / string) *FSP "]"
string = DQUOTE *(%x20-21 / %x23-7E) DQUOTE
move = (register / memory) *FSP "=" *FSP (register / memory / label)
add  = "add" *FSP (register / memory) *FSP (register / memory / label)
sub  = "sub" *FSP (register / memory) *FSP (register / memory / label)
mul  = "mul" *FSP (register / memory) *FSP (register / memory / label)
div  = "div" *FSP (register / memory) *FSP (register / memory / label)
and  = "and" *FSP (register / memory) *FSP (register / memory / label)
or   = "or"  *FSP (register / memory) *FSP (register / memory / label)
xor  = "xor" *FSP (register / memory) *FSP (register / memory / label)
not  = "not" *FSP (register / memory) *FSP (register / memory / label)
jmp  = "goto"           *FSP label / ("$" *HEXDIG) / register / memory
jz   = "goto ?zero"     *FSP label / ("$" *HEXDIG) / register / memory
jnz  = "goto !?zero"    *FSP label / ("$" *HEXDIG) / register / memory
jg   = "goto ?greater"  *FSP label / ("$" *HEXDIG) / register / memory
jng  = "goto !?greater" *FSP label / ("$" *HEXDIG) / register / memory
jl   = "goto ?lesser"   *FSP label / ("$" *HEXDIG) / register / memory
jnl  = "goto !?lesser"  *FSP label / ("$" *HEXDIG) / register / memory
register = "$" ("string" / "index" / "AH" / "AL" / "A" / "BH" / "BL" / "B"
            / "CH" / "CL" / "C" / "DH" / "DL" / "D" / "base" / "ia")
memory = "{" register / ("$" *HEXDIG) "}"
push = "push" *FSP register / memory / ("$" *HEXDIG) / label
pop = "pop" *FSP register
sysfn = "!sys" ("video" / "equ" / "memsiz" / "disk" / "serial" / "misc" / "kb" / "printer" / "rtclk" / "pci")
compare = "cmp" *FSP register *FSP register / ("$" *HEXDIG)
placeholder = "<" *VCHAR ">"
```

an example would be the 8086 "hello world" bootloader:

```aASM
LIT
    use16
    org 0x7C00
ELIT


$string = @msg
$AH     = $0E
$AL     = {$string}
@loop
    cmp  $AL     $0
    goto !?zero  @eloop
    !sysvideo
    add  $string $1
    goto loop
@eloop
LIT
    jmp $
ELIT

@msg
    ["Hello World!", $A, $D, $0]

LIT
    times 510-($-$$) db 0
    dw 0xAA55
ELIT
```

for completeness sake I will place here a table comparing aASM instructions
to 8086 assembly instructions:

```
i===================================i
| aASM               | 8086         |
#-----------------------------------#
| label <name>       | <name>:      |
| [ <values> ]       | db <values>  |
| LIT <val> ELIT     | <val>        |
| <a> = <b>          | mov <a>, <b> |
| add <a> <b>        | add <a>, <b> |
| sub <a> <b>        | sub <a>, <b> |
| mul <a> <b>        | push dx      |
| ...                | push ax      |
| ...                | xor dx, dx   |
| ...                | mov ax, <a>  |
| ...                | mul <b>      |
| ...                | pop ax       |
| ...                | pop dx       |
| div <a> <b>        | push dx      |
| ...                | push ax      |
| ...                | xor dx, dx   |
| ...                | mov ax, <a>  |
| ...                | div <b>      |
| ...                | pop ax       |
| ...                | pop dx       |
| and <a> <b>        | and <a>, <b> |
| or <a> <b>         | or <a>, <b>  |
| xor <a> <b>        | xor <a>, <b> |
| not <a>            | not <a>      |
| goto <a>           | jmp <a>      |
| goto ?zero <a>     | jz <a>       |
| goto !?zero <a>    | jnz <a>      |
| goto ?greater <a>  | jg <a>       |
| goto !?greater <a> | jng <a>      |
| goto ?lesser <a>   | jl <a>       |
| goto !?lesser <a>  | jnl <a>      |
| push <a>           | push <a>     |
| pop <a>            | pop <a>      |
| cmp <a> <b>        | cmp <a> <b>  |
| sysvideo           | int 0x10     |
| sysequ             | int 0x11     |
| sysmemsiz          | int 0x12     |
| sysdisk            | int 0x13     |
| sysserial          | int 0x14     |
| sysmisc            | int 0x15     |
| syskb              | int 0x16     |
| sysprinter         | int 0x17     |
| sysrtclk           | int 0x1A     |
| syspci             | int 0x1A     |
l===================================l
```

We will also (for convenience sake) include the following definition:

```
FSP = *(NL / SP / CR / HTAB)
```

it should also be noted that `placeholder` is a notational help
in order to explain code that cannot be guessed without context,
i.e an argument is passed into the statement.

## Lexical Conventions

A program is compiled as a series of files, specified in order,
that each contain a series of tokens that translate to a desired
assembly code.

Comments are blocks of text that are ignored by the compiler and
are defined with the following syntax:

```ABNF
comment = "/*" *(VCHAR / SP / CR / LF / HTAB) "*/"
```

The following keywords are reserved in LBOD:

```
i==================================i
| PROGRAM   | END      | const     |
| var       | bytes    | words     |
| quads     | doubles  | asm       |
| function  | break    | continue  |
| save      | ax       | bx        |
| cx        | dx       | si        |
| di        | bp       | al        |
| ah        | bl       | bh        |
| cl        | ch       | dl        |
| dh        | load     | int       |
| interrupt | move     | add       |
| subtract  | multiply | divide    |
| compare   | equal    | to        |
| greater   | less     | than      |
| or        | and      | negate    |
| not       | xor      | increment |
| loop      | forever  | return    |
| call      | match    |           |
l==================================l
```

The following symbols are also reserved:

```
: ( ) + -
* / ? = <
> | & - ;
[ ] , ^
```

In LBOD there are six kinds of constant:

```
i===============================================================================================================i
| name              | description                               | ABNF                                          |
#---------------------------------------------------------------------------------------------------------------#
| decimal integer   | an integer written in base 10             | dec = *DIGIT                                  |
| hex integer       | an integer written in base 16             | hex = "$" *HEXDIG                             |
| oct integer       | an integer written in base 8              | oct = "o" *%x30-37                            |
| binary integer    | an integer written in base 2              | bin = "b" *BIT                                |
| int literal       | an integer                                | ilit = dec / hex / oct / bin                  |
| string literal    | a set of ascii characters                 | str = DQUOTE *(VCHAR / SP) DQUOTE             |
| asm string        | a string literal containing assembly code | asmstr = 3*3DQUOTE *(VCHAR / CR / NL / SP)    |
| ...               | ...                                       |          3*3DQUOTE                            |
| byte literal      | a set of bytes                            | literal = (str / ilit) ["," literal]          |
l===============================================================================================================l
```

To conclude this section, in LBOD what also must be defined are
identifiers, used to distinguish various different objects from each other,
the ABNF is as follows:

```ABNF
iden = *(ALPHA / DIGIT)
```

## Program Statements

Every file must begin with a program statement, consisting of the following:

```ABNF
program = "PROGRAM" str FSP NL *primary FSP END FSP
```

The program file should place a comment (of whatever kind applicable) stating the text inside
its string at the beginning of the compiled section pertaining to that file.

## Primary Statements

Within programs exist "primary statements" of which there are three:

- declarations
- functions
- asm statements

the ABNF for this is as follows:

```ABNF
primary = declaration / function / asm
```

## Declarations

It is generally required that a program store data permanantly, this is generally done
via the use of variables. In LBOD this can be done in one of three ways, registers (which
will be spoken about once applicable), variables, and constants. Constants are the simplest
of the trio, simply being an assembler flag, associating a certain identifier with a constant
value. While variables are an amount of space in the final binary file reserved for use via
reference.

The ABNF of consts is as follows:

```ABNF
vconst = "const" FSP iden FSP ":" FSP ilit
```

The corresponding assembly will be different depending on the target assembler (even considering aASM)
simply because it is not a base function of any assembly, thusly it has been defined as the following:
An identifier that will be replaced with the value associated to it in every file following (Whether or
not it effects files prior to the current one will be considered undefined behaviour and should not be
assumed by the user and need not be enforced by the compiler developer).

The ABNF of variables is as follows:

```ABNF
vvar = "var" FSP ("bytes" / "words" / "doubles" / "quads") FSP iden FSP ":" FSP literal
```

The associated aASM is as follows (consider `var <type> <a> : <b>`):

```aASM
@<a> [ <b in bytes> ]
```

## Asm statements

As LBOD is a language deeply intwined with the underlying assembly, the need for a statement
translating its input *directly* to the assembly file.

Its ABNF is as follows:

```ABNF
asm = "asm" FSP asmstr
```

and its aASM (consider `asm <a>`):

```aASM
<a>
```

## Functions

A function in LBOD is simply an address that can be jumped to, that can also be returned from.

The ABNF of functions are as follows:

```
function = "function" FSP iden FSP "[" FPS ilit FSP "]" FSP "(" FSP *(statement / FSP) ")"
```

and the aAsm (consider `function <a> [<b>] ( <c> )`)

```
@a
    <c>
    pop $ia
    goto $ia
```

## Statements

A statement in LBOD describes any kind of instruction that is takes place inside a function.
What follows is a list of statements (and the corresponding ABNF)

#### repetition
- forever
- loop
#### compare
- compare
- match
#### control flow
- call
- continue
- break
- return
#### bitwise
- not
- and
- or
- xor
#### Arithmetic
- subtract
- add
- multiply
- divide
- negate
- increment
#### storage
- move
- load
- save
#### function
- interrupt

```ABNF
statement = forever / loop / compare / match / call / continue / break / return / not / and
        / or / xor / subtract / add / multiply / divide / negate / increment / move / load
        / save / interrupt
```

## Forever loop

It is at times, helpful to loop over something for a theoretically indefinite amount of
time, think a mainloop in which we take user input in order to pass it to a shell function.

The syntax is as follows:

```ABNF
forever = "forever" FSP "(" FSP *(statement / asm / FSP) ")"
```

the aASM is as so (consider `forever (<a>)`):

```aASM
@.F<integer>
<a>
goto @.F<integer>
@.EF<integer>
```

`<integer>` is simply a number appended to the end of the labels in order to differentiate the
different forever loops.

## Loop

While forever loops are useful, it is easy to imagine a situation in which we would want to exit
a loop conditionally. This is easily done with loop statements. A loop statement takes as an
argument a conditional statement (of the form `<a> <condition operation> <b>`) which the loop
will use to decide whether or not to terminate. The loop will run until the condition evaluates
false.

```ABNF
loop = "loop" FSP (register / memory) (("=" / "equal to") / ("<" / "lesser than") / (">" / "greater than")) (register / memory / ilit / iden) FSP "("
        FSP *(statement / asm / FSP) ")"
```

The aASM (for '=' case) is the following (consider `loop <a> = <b> ( <c> )`)

```aASM
@.L<integer>
<c>
cmp <a> <b>
goto !?zero @.L<integer>
@.EL<integer>
```

The aASM (for '>' case) is the following (consider `loop <a> = <b> ( <c> )`)

```aASM
@.L<integer>
<c>
cmp <a> <b>
goto !?greater @.L<integer>
@.EL<integer>
```


The aASM (for '<' case) is the following (consider `loop <a> = <b> ( <c> )`)

```aASM
@.L<integer>
<c>
cmp <a> <b>
goto !?lesser @.L<integer>
@.EL<integer>
```

## Registers and Memory

I feel now that it has become relevant enough to define registers and memory. A register is
analagous to those in assembly, volatile single values. There are the following registers
available to the LBOD programmer:

```
i===================================i
| IDEN | SIZE    | PURPOSE          |
#-----------------------------------#
| ax   | 2 bytes | general purpose  |
| al   | 1 byte  | general purpose  |
| ah   | 1 byte  | general purpose  |
| bx   | 2 bytes | general purpose  |
| bl   | 1 byte  | general purpose  |
| bh   | 1 byte  | general purpose  |
| cx   | 2 bytes | general purpose  |
| cl   | 1 byte  | general purpose  |
| ch   | 1 byte  | general purpose  |
| dx   | 2 bytes | general purpose  |
| dl   | 1 byte  | general purpose  |
| dh   | 1 byte  | general purpose  |
| si   | 2 bytes | string register  |
| di   | 2 bytes | pointer register |
| bp   | 2 bytes | pointer register |
l===================================l
```

Memory is also simple, it is simply here a dereferenced pointer with the following ABNF:

```ABNF
"[" register / iden "]"
```

## Compare statements

It is also clearly useful to do something conditional only once, and thusly compare statements
have been made.

```ABNF
compare = ("compare" / "?") FSP (register / memory) (("=" / "equal to") / ("<" / "lesser than") / (">" / "greater than"))
        (register / memory / ilit / iden) FSP "(" FSP *(statement / asm / FSP) ")"
```

The aASM (for '=' case) is the following (consider `compare <a> = <b> ( <c> )`)

```aASM
@.I<integer>
cmp <a> <b>
goto !?zero @.EI<integer>
<c>
@.EI<integer>
```

The aASM (for '>' case) is the following (consider `compare <a> = <b> ( <c> )`)

```aASM
@.I<integer>
cmp <a> <b>
goto !?greater @.EI<integer>
<c>
@.EI<integer>
```

The aASM (for '<' case) is the following (consider `compare <a> = <b> ( <c> )`)

```aASM
@.I<integer>
cmp <a> <b>
goto !?lesser @.EI<integer>
<c>
@.EI<integer>
```

## Match statements

A match statement is a compact way of writing compare equal statements. If for example
you need to check what character a user has inputted before printing out it as well as
extra characters (say for transforming LF and CR to CRLF).

```ABNF
match = "match" FSP (register / memory) FSP "(" FSP
        ((register / memory / ilit) FSP ":" *(statement / asm) ";") ")"
```

(consider:

```
match <a> (
    <b> : <c> ;
    <d> : <e> ;
)
```
)

```ABNF
@.M<integer>
cmp <a> <b>
goto !?zero .EM<integer>
<c>
@.EM<integer>
@.M<integer+1>
cmp <a> <d>
goto !?zero .EM<integer+1>
<e>
@.EM<integer+1>
```

## Call statement

What use are functions if we cannot call them? Not much is to be said about function calls,
the ABNF and aASM (regarding `call <a> (*<c>)`) follows:

```ABNF
call = "call" FSP iden FSP "(" FSP *(register / memory / iden / FSP) ")"
```

```aASM
<for every c:>
push <c>

push <current address>
goto <a>

<for every c:>
pop <c>
```

## Continue

A continue statement simply jumps to the beginning of the current loop.

ABNF:

```ABNF
continue = "continue"
```

aASM:

```
goto .<F / L><integer>
```

where `<F / L>` is simply whichever kind of loop is currently being ran.

## Break

A break statement is similar to a Continue statement but instead it jumps
to the end

ABNF:

```ABNF
break = "break"
```

aASM:

```
goto .E<F / L><integer>
```

where `<F / L>` is simply whichever kind of loop is currently being ran.

## Return

A return statement (assuming the stack is clean (all load/save statements in the current
function are balanced)) will return execution to whichever function called the current one.

ABNF:

```ABNF
return = "return"
```

aASM:

```aASM
pop $ia
goto $ia
```

## Not

Not is our first logical instruction. It takes the input value and flips each bit contained
within to its logical opposite.

```ABNF
not = "not" / "~" (register / memory)
```

```aASM
not <a>
```

## And

And takes its input values and bitwise ands them, and then stores it in the first result.

```ABNF
and = "and" / "&" (register / memory) (register / memory / iden / ilit)
```

```aAsm
and <a> <b>
```

## Or

Or takes its input values and bitwise ors them, and then stores it in the first result.

```ABNF
or = "or" / "|" (register / memory) (register / memory / iden / ilit)
```

```aAsm
or <a> <b>
```

## Xor

And takes its input values and bitwise ands them, and then stores it in the first result.

```ABNF
xor = ("xor" / "^") (register / memory) (register / memory / iden / ilit)
```

```aAsm
xor <a> <b>
```

## Add

Add is our first arithmetic input takes its input values and adds them, and then stores it in the
first result.

```ABNF
add = "add" / "+" (register / memory) (register / memory / iden / ilit)
```

```aAsm
add <a> <b>
```

## Subtract

Subtract takes its input values and adds them, and then stores it in the first result.

```ABNF
add = "subtract" / "-" (register / memory) (register / memory / iden / ilit)
```

```aAsm
sub <a> <b>
```

## Multiply

Multiply takes its input values and adds them, and then stores it in the first result.

```ABNF
mul = "multiply" / "*" (register / memory) (register / memory / iden / ilit)
```

```aAsm
mul <a> <b>
```

## Divide

Divide takes its input values and adds them, and then stores it in the first result.

```ABNF
divide = "divide" / "/" (register / memory) (register / memory / iden / ilit)
```

```aAsm
div <a> <b>
```

## Negate

Negate takes its input value and negates them (twos compliment) it,

```ABNF
neg = "negate" / "neg" (register / memory)
```

```aAsm
not <a>
add <a> 1
```

## Increment

Increment takes its input value and negates them (twos compliment) it,

```ABNF
increment = "increment" / "inc" (register / memory)
```

```aAsm
add <a> 1
```

## Move

Move stores its second input in its first input

```ABNF
move = "move" / "=" (register / memory) (register / memory / iden / ilit)
```

```aAsm
<a> = <b>
```

## Load

Load pops from the stack and stores it to its arguments in reverse order (the order that
would be passed into save should too be passed into load)

its register inputs must only be a width of two bytes.

```ABNF
load = "load" FSP "(" FSP register FSP ")"
```

considering `load (*<c>)`

```aASM
<for each c starting with the final c>
pop c
```

## Save

Save pops from the stack and stores it to its arguments in forwards order (the order that
would be passed into load should too be passed into load)

its register inputs must only be a width of two bytes.

```ABNF
save = "save" FSP "(" FSP register FSP ")"
```

considering `save (*<c>)`

```aASM
<for each c>
push c
```

## Interrupts

Interrupts map directly to BIOS interrupt calls. See [Wikipedia](https://en.wikipedia.org/wiki/BIOS_interrupt_call).
The following interrupts are allowed by LBOD:

```
i==================i
| System     | int |
#------------------#
| sysvideo   | $10 |
| sysequ     | $11 |
| sysmemsiz  | $12 |
| sysdisk    | $13 |
| sysserial  | $14 |
| sysmisc    | $15 |
| syskb      | $16 |
| sysprinter | $17 |
| sysrtclk   | $1A |
| syspci     | $1A |
l==================l
```

```ABNF
interrupt = "interrupt" / "int" ilit
```

