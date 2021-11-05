clean:
	rm src/Patrus/Lexer.hs docs/*.html

lexer:
	alex src/Patrus/Lexer.x

literate:
	pandoc -s LoxLanguageNotes.md -o docs/LoxLanguageNotes.html

all: lexer literate