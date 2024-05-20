# Compiler and flags
GHC = ghc
GHC_FLAGS = -O2

# Source file and executable name
SRC = main.hs
NAME = why

all: $(NAME)

$(NAME): $(SRC)
	$(GHC) $(GHC_FLAGS) -o $@ $<

clean:
	rm -f $(NAME)

fclean: clean
	rm -f *.hi *.o

re: fclean all

# $@ : produit (ou but) de la règle
# $< : nom de la première dépendance (ou source)
# $? : toutes les dépendances plus récentes que le but
# $^ : toutes les dépendances
# $+ : idem mais chaque dépendance apparait autant de fois qu'elle est cité et l'ordre d'apparition est conservé.
