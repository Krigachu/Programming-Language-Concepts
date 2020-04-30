# 
# Rules for compiling and linking the typechecker/evaluator
#
# Type
#   make         to rebuild the executable files
#   make clean   to remove all intermediate and temporary files
#   

# Files that need to be generated from other files
DEPEND += Lexing.hs Parsing.hs Main.hs 
#ToyEval.hs

# When "make" is invoked with no arguments, we build an executable 
#  after building everything that it depends on
all: $(DEPEND) Mainboi
#Toy

# Build an executable for Toy interpreter
#Toy: $(DEPEND) Toy.hs
#	ghc Toy.hs

# Build an executable for interactive mode
Mainboi: $(DEPEND) Main.hs
	ghc Main.hs

# Generate ML files from a parser definition file
Parsing.hs : Parsing.y
	@rm -f Parsing.hs
	happy Parsing.y
	@chmod -w Parsing.hs

# Generate ML files from a lexer definition file
Lexing.hs : Lexing.x
	@rm -f Lexing.hs
	alex Lexing.x
	@chmod -w Lexing.hs

# Clean up the directory
clean::
	rm -rf Lexing.hs Parsing.hs *.hi *.o *.info


