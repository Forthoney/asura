TESTER := bin/tester

DBG_FLAGS := -const 'Exn.keepHistory true'

SOURCE := src/*.sml src/*.mlb
TESTS := tests/*.sml tests/*.mlb

all: $(TESTER)

.PHONY: test

$(TESTER): $(SOURCE) $(TESTS)
	mlton $(MLTON_FLAGS) $(DBG_FLAGS) -output $@ tests/asura.test.mlb

test: $(TESTER)
	$(TESTER)

clean:
	rm -rf bin/*
