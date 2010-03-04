DIR=../../lib/ecube-4.0/ebin
REL=ecube
PCAP=~/git/epcap/ebin
CURVE=~/git/gl/maxBezier.erl

all: $(REL).script $(REL).tar.gz

%.script: %.rel
	@echo Making script and boot files
	erl -pa $(DIR) -noshell -eval 'ok = systools:make_script("$(REL)", []), init:stop().'
#	erl -pa $(DIR) -noshell -eval 'ok = systools:make_script("$(REL)", [local]), init:stop().'

%.tar.gz: %.rel
	@echo Making tar file
	erl -pa $(DIR) -noshell -eval 'ok = systools:make_tar("$(REL)", []), init:stop().'

#PLAYLIST=test.pls
#PLAYLIST=kraftwerk.pls
PLAYLIST=lament.pls
#PLAYLIST=moonlight.pls

run:
	@erl -pa $(DIR) -pa $(PCAP) -pa $(CURVE) -boot ecube -config sys -ecube playlist $(PLAYLIST)
#	@erl -pa $(DIR) -pa $(PCAP) -pa $(CURVE) -boot ecube -config sys
#	@erl -pa $(DIR) -pa $(PCAP) -pa $(CURVE) -boot ecube -config small

clean:
	@rm -f erl_crash.dump *~ *.tar.gz *.script *.boot
