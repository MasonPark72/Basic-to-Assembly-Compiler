;BASIC to Assembly Comppiler
;Prompts user for file name, then compiles the BASIC code to assembly
;file must have .b extension
;Mason Park

.model small
.stack 100h
.data
	promptMsg db 'Enter a filename: $'
	exitMsg db 0dh, 0ah, 'Program compiled successfully!$'
	failMsg db 0dh, 0ah, 'File not found!$'
	crlf db 0dh, 0ah, '$'
	filename db 256 dup(0)
	sourceLine db 256 dup(0)
	outputLine db 256 dup(0)
	tempLine db 256 dup(0)
	sourceFile dw ?
	outputFile dw ?
	tempFile dw ?
	filePos dw 0
	pos1 dw ?
	pos2 dw ?
	pos3 dw ?
	varNum dw 0
	
	tasm db 'tasm '
	tlink db 'tlink '
	
	oInitial1 db '.model small', 0dh, 0ah 
	oInitial2 db '.stack 100h', 0dh, 0ah 
	oInitial3 db '.data', 0dh, 0ah
	oInitial4 db 'crlf db 0dh, 0ah, "$"', 0dh, 0ah
	tInitial1 db '.code', 0dh, 0ah
	tInitial2 db 'mov ax, @data', 0dh, 0ah
	tInitial3 db 'mov ds, ax', 0dh, 0ah
	tInitial4 db 'printStr macro X', 0dh, 0ah
	tInitial5 db 'mov dx, offset X', 0dh, 0ah
	tInitial6 db 'mov ah, 09h', 0dh, 0ah
	tInitial7 db 'int 21h', 0dh, 0ah
	tInitial8 db 'endm', 0dh, 0ah
	
	let_ db 'LET$'
		let1 db 'mov '
		let2 db ' dw ?', 0dh, 0ah
	print_ db 'PRINT$'
		print1 db 'printStr '
		print2 db 'printStr crlf', 0dh, 0ah
		print3 db ' db "'
	end_ db 'END$'
		end1 db 'mov ah, 4ch', 0dh, 0ah
		end2 db 'int 21h', 0dh, 0ah
		end3 db 'end', 0dh, 0ah
	read_ db 'READ$'
	data_ db 'DATA$'
	goto_ db 'GOTO$'
		goto1 db 'jmp '
	if_ db 'IF$'
		if3 db 'cmp '
		if4 db 'jne N'
	for_ db 'FOR$'
	next_ db 'NEXT$'
	gosub_ db 'GOSUB$'
	return_ db 'RETURN$'
	def_ db 'DEF$'
	dim_ db 'DIM$'
	rem_ db 'REM$'
	stop_ db 'STOP$'
	
.code
	main proc
		mov ax, @data
		mov ds, ax
		
		;------------misc. macros------------
		
		;PURPOSE: pushes registers onto stack
		;IN:
		;	regs: registers to be pushed
		pushReg macro regs
			irp reg, <regs>
				push reg
			endm
		endm
		
		;PURPOSE: pops registers off of stack
		;OUT:
		;	regs: registers to be popped
		popReg macro regs
			irp reg, <regs>
				pop reg
			endm
		endm
		
		;PURPOSE: runs all combination of operations with operands
		;IN:
		;	operations: list of operations to run
		;	operands: list of operands to run on
		combo macro operations, operands
			irp operation, <operations>
				irp operand, <operands>
					operation operand
				endm
			endm
		endm
		
		;PURPOSE: runs all combination of operations with operands
		;IN:
		;	operations: list of operations to run
		;	operands1: list of first operands to run on
		;	operands2: list of second operands to run on
		combo2 macro operations, operands1, operands2
			irp operation, <operations>
				irp operand1, <operands1>
					irp operand2, <operands2>
						operation operand1, operand2
					endm
				endm
			endm
		endm
		
		;------------debug macros------------
		
		;PURPOSE: prints the string stored at X
		;IN:
		;	text: text to be printed
		printStr macro text
			pushReg <ax,dx>
			mov dx, offset text				;loads first byte of string
			mov ah, 09h						;DOS API: print the string at dx
			int 21h
			popReg <dx,ax>
		endm
		
		;------------file I/O macros------------
		
		;PURPOSE: reads characters from a file
		;IN:
		;	cx: number of characters to read
		;	file: file handle
		;OUT:
		;	ax: number of characters read
		;	dest: string that holds result
		readStr macro dest, file
			pushReg <bx,dx>
			mov ah, 3fh						;DOS API: read from file
			mov bx, file					;	file handle: file
											;	number of bytes to read: cx
			mov dx, offset dest				;	destination address: dest
			int 21h
			popReg <dx,bx>
		endm
		
		;PURPOSE: sets the current position in the file
		;IN:
		;	file: file handle
		;	pos: position, from the beginning
		setFilePos macro file, pos
			pushReg <ax,bx,cx,dx>
			mov ah, 42h						;DOS API: set file position
			xor al, al						;	offset: from beginning
			mov bx, file					;	file handle: file
			xor cx, cx						;	segment: 0
			mov dx, pos						;	offset: pos
			int 21h
			popReg <dx, cx, bx, ax>
		endm
		
		;PURPOSE: reads one line
		;IN:
		;	file: file handle
		;OUT:
		;	ax: number of characters read
		;	dest: string that holds result
		readLine macro dest, file
			pushReg <bx,cx,dx,si>
			mov cx, 256
			readStr	dest, file				;reads 256 characters
			setFilePos file, filePos		;return position to beginning
			findLen dest, 0ah				;finds the length of the first line, stores in cx
			readStr	dest, file				;reads one line from the file
			mov si, ax
			mov dest[si], 0ah				;adds a line feed to the end of the result, necessary for last line of file
			add filePos, cx					;sets filePos to the beginning of the next line
			popReg <si,dx,cx,bx>
		endm
		
		;PURPOSE: writes one line
		;IN:
		;	output: string to be written
		;	file: file handle
		writeLine macro output, file
			pushReg <ax,bx,cx,dx>
			findLen output, 0ah				;finds the length of the output string, stores in cx
			mov ah, 40h						;DOS API: write to file
			mov bx, file					;	file handle: file
											;	number of bytes to write: cx
			mov dx, offset output			;	output string: output
			int 21h
			popReg <dx,cx,bx,ax>
		endm
		
		;------------string macros------------
		
		;PURPOSE: finds the next instance of the given character
		;IN:
		;	string: string to search
		;	char: character to search for
		;	pos: position to start the search from
		;OUT:
		;	pos: can be register, position of next instance of char, 256 if char not found
		findChar macro string, char, pos
			local findLoop, findLoopEnd
			push si
		findLoop:
			mov si, pos
			cmp string[si], char
			je findLoopEnd					;exits loop if pos holds the character
			cmp pos, 256
			je findLoopEnd					;exits loop if pos is the end of the string
			inc pos
			jmp findLoop					;jumps to the beginning of the loop
		findLoopEnd:
			pop si
		endm
		
		;PURPOSE: finds the length of a string, ending at the first line feed
		;IN:
		;	string: string to find the length of
		;	termChar: Character that terminates the string
		;OUT:
		;	cx: length of the string
		findLen macro string, termChar
			xor cx, cx
			findChar string, termChar, cx	;finds the first line feed, stores in cx
			inc cx							;sets cx to the length of the string instead of the index of the line feed
		endm
		
		;PURPOSE: compares two strings, starting at a given position
		;IN:
		;	str1: first string
		;	str1Pos: start index of string 1
		;	str2: second string
		;	str2Pos: start index of string 2
		;	len: number of characters to compare
		;OUT:
		;	zf: sets if strings are equal, resets if not
		cmpString macro str1, str1Pos, str2, str2Pos
			local cmpLoop, endCmp
			pushReg <ax,bx,cx,dx,si,di>
			xor cx, cx
			mov si, str1Pos
			mov di, str2Pos					;initialize cx, si, and di
		cmpLoop:
			mov ah, str1[si]				;sets ah to str1[pos]
			mov al, str2[di]				;sets al to str2[pos]
			cmp ah, '$'
			jz endCmp
			cmp al, '$'
			jz endCmp						;exit if either character is $
			cmp ah, al						;compares both strings to the current character
			jnz endCmp						;exits if both strings aren't equal
			combo inc <cx, si, di>			;increment current position
			jmp cmpLoop
		endCmp:
			popReg <di,si,dx,cx,bx,ax>
		endm
		
		;PURPOSE: copies string 2 into string 1
		;IN:
		;	str1: source string
		;	str1Pos: start index of string 1
		;	str2: destination string
		;	str2Pos: start index of string 2
		;	len: number of characters to copy, can be bx
		movString macro str1, str1Pos, str2, str2Pos, len
			local copyLoop
			pushReg <ax,cx,dx,si,di>
			xor cx, cx
			mov si, str1Pos
			mov di, str2Pos					;initialize cx, si, and di
		copyLoop:
			mov al, str2[di]
			mov str1[si], al				;copy character at current pos
			combo inc <cx, si, di>			;increment pos
			cmp cx, len
			jl copyLoop						;jumps to the start of the copy loop if the character limit hasn't been reached
			popReg <di,si,dx,cx,ax>
		endm
		
		;PURPOSE: clears str, by replacing every character with 0
		;IN:
		;	len: length of str
		;OUT:
		;	str: is empty, all characters are null
		clearLine macro str, len
			local loopStart
			pushReg <si>
			xor si, si
		loopStart:
			mov str[si], 0
			inc si
			cmp si, len
			jnz loopStart
			popReg <si>
		endm
		
		;PURPOSE: replaces all instances of fStr with rStr
		;IN:
		;	fStr: string to find, must end in $
		;	rStr: string to replace all instances of fStr with, must end in $
		;OUT:
		;	str: all instances of fStr in str are replaced with rStr, must end in 0
		;	tempLine: is overwritten
		;	pos1: is overwritten
		;	pos2: is overwritten
		;	pos3: is overwritten
		replace macro str, fStr, rStr
			local loopStart, hit, endLoop
			pushReg <ax,bx,cx,dx,si,di>
			mov pos1, 0						;position of each instance of fStr, or the end of the string
			mov pos2, 0						;position of the start of each search in str
			mov pos3, 0						;position of write to tempLine
		loopStart:
			cmpString str, pos1, fStr, 0
			jz hit							;jump to hit if fStr is found
			inc pos1
			mov si, pos1
			cmp str[si], 0
			jnz loopStart					;repeat until 0 character is found
			jmp endLoop
		hit:
			mov bx, pos1
			sub bx, pos2					;finds length between beginning of current search and end of current search
			movString tempLine, pos3, str, pos2, bx;writes all characters before the occurrence of fStr
			add pos3, bx					;updates tempLine write pos
			findLen fStr, '$'
			add cx, pos1
			dec cx
			mov pos1, cx
			mov pos2, cx					;sets the beginning points of the next search to after occurrence of fStr
			findLen rStr, '$'
			dec cx
			mov bx, cx
			movString tempLine, pos3, rStr, 0, bx;writes rStr into tempLine
			add pos3, bx					;updates tempLine write pos
			jmp loopStart					;resume search
		endLoop:
			mov bx, pos1
			sub bx, pos2					;finds length of string after last occurrence of fStr
			movString tempLine, pos3, str, pos2, bx;writes to tempLine
			add bx, pos3					;updates bx to hold length of tempLine
			clearLine str, 256
			movString str, 0, tempLine, 0, bx;copies tempLine back into str
			popReg <di,si,dx,cx,bx,ax>
		endm
		
		;PURPOSE: splits sStr into substrings delimited by separator, and stores the one at index into dStr
		;IN:
		;	sStr: source string that will be split
		;	separator: string that delimits each substring, must end in $
		;	index: index of the substring to store into dStr
		;OUT:
		;	dStr: contains the substring located at index
		split macro dStr, sStr, separator, index
			local loopStart, hit, endLoop, miss
			pushReg <ax,bx,cx,dx,si,di>
			mov pos1, 0						;position of each instance of separator, or the end of the string
			mov pos2, 0						;position of the start of each search in sStr
			xor cx, cx						;current index
		loopStart:
			cmpString sStr, pos1, separator, 0
			jz hit							;jump to hit if separator is found
			inc pos1
			mov si, pos1
			cmp sStr[si], 0
			jnz loopStart					;repeat until 0 character is found
			jmp endLoop
		hit:
			cmp cx, index
			jz endLoop						;jumps to the end loop 
			push cx
			findLen separator, '$'
			add cx, pos1
			dec cx
			mov pos1, cx
			mov pos2, cx					;sets the beginning points of the next search to after the separator
			pop cx
			inc cx
			jmp loopStart
		endLoop:
			cmp cx, index
			jnz miss
			mov bx, pos1
			sub bx, pos2					;finds length of substring
			clearLine dStr, 256
			movString dStr, 0, sStr, pos2, bx
			popReg <di,si,dx,cx,bx,ax>
		miss:
		endm
		
		;PURPOSE: Generates a variable name
		;IN:
		;	pos: position to write to
		;	varNum: unique number that corresponds to each variable
		;OUT:
		;	cx: number of characters written
		;	string: variable name corresponding to varNum is written
		genVarName macro string, pos
			local genStart
			pushReg <ax,bx,dx,si>
			mov ax, varNum
			mov bl, 26						;divisor is 26 so that remainder is a letter
			xor cx, cx
			mov si, pos
			mov string[si], '$'				;first character is $
			combo inc <cx, si>
		genStart:
			div bl
			add ah, 'a'
			mov string[si], ah				;writes the remainder af ax/26 to string
			combo inc <cx, si>
			xor ah, ah						;clears ah, ax now equals al
			cmp ax, 0						;tests if the quotient is 0
			jnz genStart
			inc varNum
			popReg <si,dx,bx,ax>
		endm
		;------------end macros------------
		
		
		
		;read user input for file name
		printStr promptMsg
		mov ah, 0ah							;DOS API: buffered input
		mov dx, offset filename				;	result: filename
		mov	filename[0], 254				;	max length: 254
		int 21h
		xor ah, ah
		mov al, filename[1]
		add al, 2
		mov si, ax
		mov filename[si], 0					;add zero at end to convert to ASCIZ file name
		
		
		;open file
		mov ah, 3dh							;DOS API: open file
		mov al, 0h							;	access level: read-only
		mov dx, offset filename+2			;	file location: filename
		int 21h
		jnc fileOpenSuccess					;jumps if the file is opened successfully (necessary due to limited conditional jumps range)
		printStr failMsg
		jmp ending
	fileOpenSuccess:
		mov sourceFile, ax					;stores file handle
		
		
		;create output file
		mov filename[si-1], 'a'
		mov filename[si], 's'
		mov filename[si+1], 'm'
		mov filename[si+2], 0				;converts [file].b to [file].asm
		mov ah, 3ch							;DOS API: create file
		xor cx, cx							;	file attributes: none
		int 21h
		mov outputFile, ax					;stores file handle
		combo2 writeLine <oInitial1, oInitial2, oInitial3, oInitial4>, outputFile
		
		;create temp file, which holds the .code segment and is appended to the output file after the sourceFile is done being processed
		mov filename[si-1], 't'
		mov filename[si], 'm'
		mov filename[si+1], 'p'
		mov filename[si+2], 0				;converts [file].asm to [file].tmp
		mov ah, 3ch							;DOS API create file
		xor cx, cx							;	file attributes: none
		int 21h
		mov tempFile, ax					;stores file handle
		combo2 writeLine <tInitial1, tInitial2, tInitial3, tInitial4, tInitial5, tInitial6, tInitial7, tInitial8>, tempFile
		
		
		;translate sourceFile line by line
	readLoop:
		readLine sourceLine, sourceFile		;reads a line from sourceFile and stores in sourceLine
		cmp ax, 0							;ax = characters read
		je endRead							;stops reading if line is empty
		call PARSE							;parse the line of code and write to file
		jmp readLoop
	endRead:
		
		
		;appends tempFile to outputFile
		mov filePos, 0						;resets filePos to zero
		setFilePos tempFile, 0				;sets the file position of tempFile to 0
	appendLoop:
		readLine sourceLine, tempFile		;reads a line of tempFile and stores in sourceLine
		cmp ax, 0							;ax = characters read
		je endAppend						;stops reading if line is empty
		writeLine sourceLine, outputFile	;writes outputLine to outputFile
		jmp appendLoop
	endAppend:
		
		;closes and deletes tempFile
		mov ah, 3eh							;DOS API close file
		mov bx, tempFile					;	file handle: tempFile
		int 21h
		mov ah, 41h							;DOS API: delete file
		mov dx, offset filename	+ 2			;	file name: filename
		int 21h
		
		;closes sourceFile and outputFile
		mov ah, 3eh							;DOS API: close file
		mov bx, sourceFile					;	file handle: sourceFile
		int 21h
		mov ah, 3eh							;DOS API: close file
		mov bx, outputFile					;	file handle: outputFile
		int 21h
		
		;calls tasm and tlink (not currently working)
;		mov filename[si-2], 0ah				;sets a carriage return at the end of the filename, which terminates the command execution
;		mov ax, 4b00h						;DOS API: load and execute program
;		mov bx, offset filename+2			;	parameter offset: filename
;		mov dx, offset tasm					;	program name: tasm
;		int 21h
;		
;		mov ax, 4b00h						;DOS API: load and execute program
;		mov bx, offset filename+2			;	parameter: filename
;		mov dx, offset tlink				;	program name tlink
;		int 21h

		;calls tasm and tlink using command line (not currently working)
;		movString outputLine, 1, tasm, 0, 5
;		xor bh, bh
;		mov bl, filename[1]
;		sub bx, 2
;		movString outputLine, 6, filename, 2, bx
;		add bx, 6
;		mov si, bx
;		mov outputLine[si], 0dh
;		mov outputLine[0], bl
;		mov si, offset outputLine
;		int 2eh
		
		printStr exitMsg
	ending:
		mov ah, 4ch							;loads return control to DOS into DOS API
		int 21h								;interrupts
	endp
	
	 
;Receives line of BASIC code and generates equivalent code in assembly
	PARSE proc
		push ax
		push bx
		push cx
		push dx
		push si
		
		
		;create a label with the line number for goto instructions
		mov outputLine[0], 'L'				;Moves an L to the beginning of the label
		mov pos1, 0							;clears pos1
		findChar sourceLine, ' ', pos1		;finds the first space
		inc pos1							;increments cx to account for the L at the beginning
		movString outputLine, 1, sourceLine, 0, pos1;copies the line number into the output
		mov si, pos1						;moves cx into si
		mov outputLine[si], ':'				;moves a : to the end of the label
		inc pos1							;increments pos1 to account for the :
		movString outputLine, pos1, crlf, 0, 2;appends a crlf to the outputLine
		writeLine outputLine, tempFile		;write to file
		
		
		;determine instruction
		dec pos1							;decrements pos1 so that it stores the index of the character after the space
		
		checkInstruction macro instruction, lbl;jumps to label Y if the instruction stored in sourceLine at pos1 is X, X1 holds the length of the instruction
			local notX
			cmpString sourceLine, pos1, instruction, 0
			jne notX
			jmp lbl							;uses nonconditional jumps to avoid conditional jump memory limitation
		notX:
		endm
		
		checkInstruction let_, I0
		checkInstruction print_, I1
		checkInstruction end_, I2
		checkInstruction read_, I3
		checkInstruction data_, I4
		checkInstruction goto_, I5
		checkInstruction if_, I6
		checkInstruction for_, I7
		checkInstruction next_, I8
		checkInstruction gosub_, I9
		checkInstruction return_, I10
		checkInstruction def_, I11
		checkInstruction dim_, I12
		checkInstruction rem_, I13
		checkInstruction stop_, I14
		
		jmp lineComplete					;skips this output if no instructions are matched
		
		
		;generate assembly output
	I0:
		;------------LET------------
		;Syntax: LET [Variable] = [Number]
		
		;generate the .code segment
		movString outputLine, 0, let1, 0, 4	;moves 'mov ' to the outputLine
		mov pos1, 0							;clears pos1
		findChar sourceLine, ' ', pos1		;finds the first space
		inc pos1							;increments pos1, now points to beginning of LET command
		findChar sourceLine, ' ', pos1;finds the second space
		inc pos1						;increments pos1, now points to the beginning of the variable name
		mov ax, pos1
		mov pos2, ax					;copies pos1 into pos2
		findChar sourceLine, ' ', pos2;finds the third space
		mov bx, pos2
		sub bx, pos1					;subtracts pos1 from pos2, stores in bx
		movString outputLine, 4, sourceLine, pos1, bx;moves the variable name into outputLine
		
		mov si, bx
		add si, 4							;sets si to point to the index after 'mov [var]
		mov outputLine[si], ','
		mov outputLine[si+1], ' '			;adds ', ' to the outputLine
		add pos2, 3					;sets pos2 to point to the start index of the number
		findChar sourceLine, 0ah, pos1;finds the line feed
		mov bx, pos1
		sub bx, pos2					;subtracts pos2 from pos1, stores in bx
		mov dx, si							;moves si into dx so that it can be used in movString
		add dx, 2							;adds 2 to dx so that it points to the index after the end of 'mov [var], '
		movString outputLine, dx, sourceLine, pos2, bx;moves the variable into the end of the outputLine
		add dx, bx
		mov si, dx							;adds bx to dx and stores in si
		mov outputLine[si], 0dh				;appends carriage return
		mov outputLine[si+1], 0ah			;appends line feed
		writeLine outputLine, tempFile		;writes to tempfile
		
		;generate the .data segment
		mov pos1, 0					;clears pos1
		findChar sourceLine, ' ', pos1;finds the first space
		inc pos1						;increments pos1, now points to beginning of LET command
		findChar sourceLine, ' ', pos1;finds the second space
		inc pos1						;increments pos1, now points to the beginning of the variable name
		mov ax, pos1
		mov pos2, ax					;copies pos1 into pos2
		findChar sourceLine, ' ', pos2;finds the third space
		mov bx, pos2
		sub bx, pos1					;subtracts pos1 from pos2, stores in bx
		movString outputLine, 0, sourceLine, pos1, bx;moves the variable name into outputLine
		movString outputLine, bx, let2, 0, 7;appends the rest of the variable declaration
		writeLine outputLine, outputFile	;writes to outputFile
		
		jmp lineComplete
	I1:
		;------------PRINT------------
		;Syntax: PRINT "[String to be printed]"
		
		;generate the .code segment
		movString outputLine, 0, print1, 0, 9;moves 'printStr ' into outputLine
		genVarName outputLine, 9			;generates a variable name into the instruction
		mov si, 9							;sets si to 9
		add si, cx							;adds cx to si, which now indexes the place after the variable
		mov outputLine[si], 0ah				;appends a line feed
		writeLine outputLine, tempFile		;write to tempFile
		writeLine print2, tempFile			;prints crlf
		
		;generate the .data segment
		dec varNum							;decrements varNum so that it creates a duplicate variable in the .data section
		mov pos1, cx					;stores cx into pos1
		genVarName outputLine, 0			;generates a variable name for .data
		movString outputLine, pos1, print3, 0, 5;appends 'db "' to outputLine
		
		add cx, 5							;adds 5 to cx, which now points to the next position in the outputLine
		mov pos1, 0					;clears pos1
		findChar sourceLine, '"', pos1;finds the first "
		inc pos1						;increments pos1 so that it points to the first character of the string
		mov ax, pos1					;move pos1 into ax
		mov pos2, ax					;moves ax into pos2
		findChar sourceLine, '"', pos2;finds the second "
		mov bx, pos2					;moves pos2 into bx
		sub bx, pos1					;subtracts pos1 from bx, which now holds the length of the printed string
		mov pos2, cx					;moves cx into pos2
		movString outputLine, pos2, sourceLine, pos1, bx;writes printed string to outputLine
		
		add cx, bx							;adds bx to cx, which now points to the end of the string
		mov si, cx							;moves cx into si
		mov outputLine[si], '$'				;appends $
		mov outputLine[si+1], '"'			;appends "
		mov outputLine[si+2], 0dh			;appends carriage return
		mov outputLine[si+3], 0ah			;appends line feed
		writeLine outputLine, outputFile	;write to outputFile
		
		jmp lineComplete
		
	I2:
		;------------END------------
		;Syntax: END
		writeLine end1, tempFile
		writeLine end2, tempFile
		writeLine end3, tempFile
		
		jmp lineComplete
		
	I3:
		;TODO: implement READ
		
		jmp lineComplete
		
	I4:
		;TODO: implement DATA
		
		jmp lineComplete
		
	I5:
		;------------GOTO------------
		;Syntax: GOTO [Line number]
		movString outputLine, 0, goto1, 0, 4;moves 'jmp ' into outputLine
		mov outputLine[4], 'L'				;moves L into outputLine
		mov pos1, 0					;clears pos1
		findChar sourceLine, ' ', pos1;finds the index of the first space (after the line number)
		inc pos1						;increments the position to point to the beginning of the command
		findChar sourceLine, ' ', pos1;finds the index of the second space (after the command name)
		inc pos1						;increments the position to point to the beginning of the line number
		mov pos2, 0					;clears pos2
		findChar sourceLine, 0ah, pos2;finds the line feed
		mov bx, pos2					;moves pos2 into bx
		sub bx, pos1					;subtracts pos1
		movString outputLine, 5, sourceLine, pos1, bx;moves the line number into the outputLine
		
		add bx, 5							;adds 5 to bx so that it points to the end of the line
		mov si, bx							;moves bx into si
		mov outputLine[si], 0ah				;appends line feed
		writeLine outputLine, tempFile		;write to tempFile
		
		jmp lineComplete
		
	I6:
		;------------IF------------
		;Syntax: IF [Variable] = [Number] THEN [Line Number]
		
		;generate the compare statement
		movString outputLine, 0, if3, 0, 4	;moves 'cmp ' to the outputLine
		mov pos1, 0					;clears pos1
		findChar sourceLine, ' ', pos1;finds the first space
		inc pos1						;increments pos1, now points to beginning of IF command
		findChar sourceLine, ' ', pos1;finds the second space
		inc pos1						;increments pos1, now points to the beginning of the variable name
		mov ax, pos1
		mov pos2, ax					;copies pos1 into pos2
		findChar sourceLine, ' ', pos2;finds the third space
		mov bx, pos2
		sub bx, pos1					;subtracts pos1 from pos2, stores in bx
		movString outputLine, 4, sourceLine, pos1, bx;moves the variable name into outputLine
		
		mov si, bx
		add si, 4							;sets si to point to the index after 'if [var]
		mov outputLine[si], ','
		mov outputLine[si+1], ' '			;adds ', ' to the outputLine
		add pos2, 3					;sets pos2 to point to the start index of the number
		mov ax, pos2
		mov pos1, ax					;copies pos2 into pos2, which now also points to the number
		findChar sourceLine, ' ', pos1;sets pos1 to the space after the number
		mov bx, pos1
		sub bx, pos2					;subtracts pos2 from pos1, stores in bx
		mov dx, si							;moves si into dx so that it can be used in movString
		add dx, 2							;adds 2 to dx so that it points to the index after the end of 'if [var], '
		movString outputLine, dx, sourceLine, pos2, bx;moves the variable into the end of the outputLine
		add dx, bx
		mov si, dx							;adds bx to dx and stores in si
		mov outputLine[si], 0dh				;appends carriage return
		mov outputLine[si+1], 0ah			;appends line feed
		writeLine outputLine, tempFile		;writes to tempfile
		
		;generate the jne statement
		movString outputLine, 0, if4, 0, 5	;moves 'jne N' into the outputLine
		mov pos1, 0					;clears pos1
		findChar sourceLine, ' ', pos1;sets pos1 to the space after the line number
		movString outputLine, 5, sourceLine, 0, pos1;adds the line number to complete the jump statement: 'jne N[line number]'
		mov si, pos1					;moves pos1 into si
		add si, 5							;adds 5 to si, which sets it to point to the end of the command
		mov outputLine[si], 0dh				;appends carriage return
		mov outputLine[si+1], 0ah			;appends line feed
		writeLine outputLine, tempFile		;writes to tempFile
		
		;generate the goto statement
		movString outputLine, 0, goto1, 0, 4;moves 'jmp ' into outputLine
		mov outputLine[4], 'L'				;moves L into outputLine
		mov pos1, 0					;clears pos1
		findChar sourceLine, ' ', pos1;finds the index of the first space (after the line number)
		inc pos1						;increments the position to point to the beginning of the command
		findChar sourceLine, ' ', pos1;finds the index of the second space (after the command name)
		inc pos1						;increments the position to point to the variable
		findChar sourceLine, ' ', pos1;finds the third space (after the variable)
		add pos1, 3					;adds 3 to pos1, pointing it to the comparison number
		findChar sourceLine, ' ', pos1;finds fifth space (after the number)
		add pos1, 6					;add 6 to pos1, pointing it to the start of the line number
		mov pos2, 0					;clears pos2
		findChar sourceLine, 0ah, pos2;finds the line feed
		mov bx, pos2					;moves pos2 into bx
		sub bx, pos1					;subtracts pos1
		movString outputLine, 5, sourceLine, pos1, bx;moves the line number into the outputLine
		
		add bx, 5							;adds 5 to bx so that it points to the end of the line
		mov si, bx							;moves bx into si
		mov outputLine[si], 0ah				;appends line feed
		writeLine outputLine, tempFile		;write to tempFile
		
		;generate the path not taken label
		mov outputLine[0], 'N'				;Moves an N to the beginning of the label
		mov pos1, 0					;clears pos1
		findChar sourceLine, ' ', pos1;finds the first space
		inc pos1						;increments cx to account for the L at the beginning
		movString outputLine, 1, sourceLine, 0, pos1;copies the line number into the output
		mov si, pos1					;moves cx into si
		mov outputLine[si], ':'				;moves a : to the end of the label
		inc pos1						;increments pos1 to account for the :
		movString outputLine, pos1, crlf, 0, 2;appends a crlf to the outputLine
		writeLine outputLine, tempFile		;write to file
		jmp lineComplete
		
	I7:
		;TODO: implement FOR
		;Syntax: FOR [variable] = [start number] TO [end number]
		
		jmp lineComplete
		
	I8:
		;TODO: implement NEXT
		;Syntax: NEXT [Variable]
		
		jmp lineComplete
		
	I9:
		;TODO: implement GOSUB
		
		jmp lineComplete
		
	I10:
		;TODO: implement RETURN
		
		jmp lineComplete
		
	I11:
		;TODO: implement DEF
		
		jmp lineComplete
		
	I12:
		;TODO: implement DIM
		
		jmp lineComplete
		
	I13:
		;------------REM------------
		;Syntax: REM [comment]
		jmp lineComplete
		
	I14:
		;------------STOP------------
		;Syntax: STOP
		writeLine end1, tempFile
		writeLine end2, tempFile
		writeLine end3, tempFile
		jmp lineComplete
		
		
	lineComplete:
		pop si
		pop dx
		pop cx
		pop bx
		pop ax
		ret
	endp
	
	end main