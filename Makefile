clean:
	rm src/Patrus/Lexer.hs

Lexer:
	alex src/Patrus/Lexer.x

all: Lexer
