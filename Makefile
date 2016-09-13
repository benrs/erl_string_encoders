REBAR = ./rebar

compile:
	$(REBAR) compile

clean:
	$(REBAR) clean

.PHONY: compile clean
