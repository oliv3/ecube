DIR=../../lib/ecube-4.0/ebin
REL=ecube
PCAP=~/git/epcap/ebin
EFLAGS=+K true +A 2 -noshell -pa $(DIR) -pa $(PCAP) -boot ecube


all: $(REL).script $(REL).tar.gz


%.script: %.rel
	@echo Making script and boot files
	erl -pa $(DIR) -noshell -eval 'ok = systools:make_script("$(REL)", []), init:stop().'
#	erl -pa $(DIR) -noshell -eval 'ok = systools:make_script("$(REL)", [local]), init:stop().'


%.tar.gz: %.rel
	@echo Making tar file
	erl -pa $(DIR) -noshell -eval 'ok = systools:make_tar("$(REL)", []), init:stop().'


run:
	@erl $(EFLAGS) -config sys
#	@erl $(EFLAGS) -config small

clean:
	@rm -f erl_crash.dump *~ *.tar.gz *.script *.boot
