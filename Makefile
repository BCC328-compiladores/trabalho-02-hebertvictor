# Makefile at 	<trabalho-01-hebertvictor/>
# -----------------------------------------

.PHONY: prebuild run-docker rebuild-docker



# for instance, while we don't integrate the 
# alex/happy compilation into cabal Makefile will be the way...

HV_ALEX_LEXER			:= src/Frontend/Lexer.x
HV_ALEX_LEXER_HS		:= src/Frontend/Lexer.hs

HV_HAPPY_SNALYSER		:= src/Frontend/Parser.y
HV_HAPPY_SNALYSER_HS	:= src/Frontend/Parser.hs

ALEX					:= alex
HAPPY					:= happy


prebuild: $(HV_ALEX_LEXER_HS) $(HV_HAPPY_SNALYSER_HS)


$(HV_ALEX_LEXER_HS): $(HV_ALEX_LEXER)
	$(ALEX) $^ -o $@


$(HV_HAPPY_SNALYSER_HS): $(HV_HAPPY_SNALYSER)
	$(HAPPY) -i $^ -o $@ --ghc

# runnint the docker
run-docker: $(VT_LEXER_HS) $(VT_PARSER_HS)
	docker-compose up -d
	docker-compose exec sl bash

# docker compose down -> stops and removes old containers before rebuilding
rebuild-docker:
	docker compose down
	docker compose build
	docker compose up -d
	docker compose exec sl bash
