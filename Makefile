REBAR = ./rebar

compile:
	$(REBAR) compile

clean:
	$(REBAR) clean

AFL_FUZZ ?= afl-fuzz
AFL_CC ?= afl-gcc
AFL_CXX ?= afl-g++
FUZZ_SKELETON ?= fuzz_skeleton
FUZZ_TERM ?= fuzz/web_safe_base64_encode.term

fuzz:
	$(REBAR) clean
	CC=$(AFL_CC) CXX=$(AFL_CXX) $(REBAR) compile
	$(AFL_FUZZ) $(AFL_FLAGS) -i fuzz/samples -o fuzz/findings -- \
	  $(FUZZ_SKELETON) priv/erl_string_encoders_nif.so $(FUZZ_TERM)

.PHONY: compile clean fuzz
