PACKAGES=xml-conduit pandoc text containers xml-hamlet
SH=nix-shell -p "haskellPackages.ghcWithPackages (p: with p; [ $(PACKAGES) ] )"
SRC=ljxml2md.hs

all: run

run:
	$(SH) --run "runghc $(SRC)"

ghcid:
	$(SH) -p ghcid --run "ghcid $(SRC)"

ghci:
	$(SH) --run "ghci $(SRC)"

shell:
	$(SH)

