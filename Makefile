clean:
	rm src/Patrus/Lexer.hs docs/Patrus.html

Lexer:
	alex src/Patrus/Lexer.x

Literate:
	pandoc -s Patrus.lhs -o docs/Patrus.html

all: Lexer
