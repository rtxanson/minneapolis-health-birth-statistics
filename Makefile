BIRTHS := ./parser/births/dist/build/Births/births

PDFS := $(wildcard pdfs/*.pdf)
CSVS := $(addprefix csvs/, $(notdir $(PDFS:%.pdf=%.csv)))
TXTS := $(PDFS:%.pdf=%.txt)

# First ...
pdfs/%.txt: pdfs/%.pdf
	pdftotext -enc UTF-8 -table $^

# Then ... 
.DELETE_ON_ERROR:
csvs/%.csv: pdfs/%.txt
	@-$(BIRTHS) $^ > $@ && echo "processed $@"

parser/births/dist/build/Births/births:
	cd parser/births/ && make all

parser: parser/births/dist/build/Births/births

.PHONY: init
init:
	@-mkdir -p csvs
	@-mkdir -p tmp

done:
	@echo
	@echo "Don't check in the stuff in csvs/ because it needs some manual processing."
	@echo

.PHONY: all
all: init \
	 parser \
	 $(TXTS) \
	 $(CSVS) \
	 done
