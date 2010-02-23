%%%-------------------------------------------------------------------
%%% File    : ec_pcap.erl
%%% Author  : Olivier <olivier@biniou.info>
%%% Description : pcap plugin
%%%-------------------------------------------------------------------
-module(ec_pcap).

-author('olivier@biniou.info').
-vsn("1.0").

-include("ec.hrl").
-include("ec_ps.hrl").
-include("epcap_net.hrl").

%% -define(IF, "any"). %% buggy
-define(IF, "eth0").
%% -define(IF, "wlan0").

%% API
-export([start_link/0]).

%% Callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

-record(state, {pkts=0, bytes=0}).

%%====================================================================
%% API
%%====================================================================
start_link() ->
    proc_lib:start_link(?MODULE, init, [self()]).


%%====================================================================
%% Callbacks
%%====================================================================
init(Parent) ->
    %% process_flag(trap_exit, true),

    Self = self(),
    ?D_REGISTER(?SERVER, Self), %% not needed
    %% Debug = sys:debug_options([]),

    %% XXX code:*dir
    %% FIXME: interface 'any' -> crash proto(0)
    PcapOpts = [{interface, ?IF}, {filter, "tcp or udp"}, {chroot, "priv/tmp"}, {snaplen, 68}],
    epcap:start(Self, PcapOpts),
    proc_lib:init_ack(Parent, {ok, Self}),
    loop(#state{}).


loop(#state{pkts=Pkt, bytes=Bytes} = State) ->
    receive	
	[{pkthdr, _Info}, {packet, Packet}] = _P ->
	    case epcap_net:decapsulate(Packet) of
		[_Ether, #ipv4{saddr=Saddr, daddr=Daddr}, Hdr, Payload] ->
		    %% out(Res),
		    %% ?D_F("Res= ~p", [Res]),
		    NBytes = Bytes + byte_size(Payload),
		    NPkt = Pkt + 1,
		    %% error_logger:info_msg("~p packets / ~p bytes~n", [NPkt, NBytes]),
		    %% ?D_F("create whatever= ~p ~p ~p ~p", [IP#ipv4.saddr, port(sport, Hdr), IP#ipv4.daddr, port(dport, Hdr)]),
		    create(Saddr, port(sport, Hdr), Daddr, port(dport, Hdr)),
		    loop(#state{pkts=NPkt, bytes=NBytes});
		
		_Other ->
		    ?D_UNHANDLED(_Other),
		    loop(State)
	    end;

	_Other ->
	    %% ?D_UNHANDLED(_Other),
	    loop(State)
    end.


out([_Ether, IP, Hdr, Payload]) ->
    error_logger:info_report([
			      {source_address, IP#ipv4.saddr},
			      {source_port, port(sport, Hdr)},
			      
			      {destination_address, IP#ipv4.daddr},
			      {destination_port, port(dport, Hdr)},
			      
			      {payload, epcap_net:payload(Payload)}
			     ]).

header(#tcp{} = Hdr) ->
    [{flags, epcap_net:tcp_flags(Hdr)},
        {seq, Hdr#tcp.seqno},
        {ack, Hdr#tcp.ackno},
        {win, Hdr#tcp.win}];
header(#udp{} = Hdr) ->
    [{ulen, Hdr#udp.ulen}];
header(#icmp{} = Hdr) ->
    [{type, Hdr#icmp.type},
        {code, Hdr#icmp.code}];
header(Packet) ->
    Packet.


port(sport, #tcp{sport = SPort}) -> SPort;
port(sport, #udp{sport = SPort}) -> SPort;
port(dport, #tcp{dport = DPort}) -> DPort;
port(dport, #udp{dport = DPort}) -> DPort;
port(_,_) -> "".


to_ascii(Packet) ->
    [epcap_net:to_ascii(C) || C <- binary_to_list(Packet)].


-define(MAXP, 2#11111111111).
-define(PAD, 1:1).


rescale(Val) ->
    (Val / ?MAXP) * 2.0 - 1.0.


create({A0,B0,C0,D0}=_SrcIP, SrcPort, {A1,B1,C1,D1}=_DstIP, DstPort) ->
    %% ?D_F("create(~p:~p => ~p:~p)", [_SrcIP, SrcPort, _DstIP, DstPort]),
    PadSrc = <<A0,B0,C0,D0,?PAD>>,
    PadDst = <<A1,B1,C1,D1,?PAD>>,
    <<Xs0:11,Ys0:11,Zs0:11>> = PadSrc,
    <<Xd0:11,Yd0:11,Zd0:11>> = PadDst,
    Xs1 = rescale(Xs0),
    Ys1 = rescale(Ys0),
    Zs1 = rescale(Zs0),
    Xd1 = rescale(Xd0),
    Yd1 = rescale(Yd0),
    Zd1 = rescale(Zd0),

    Vx = (Xd1-Xs1) / ?TTL,
    Vy = (Yd1-Ys1) / ?TTL,
    Vz = (Zd1-Zs1) / ?TTL,

    R = (SrcPort rem 128) + 128,
    B = (DstPort rem 128) + 128,
    G = ((SrcPort*DstPort) rem 128) + 128,
    Col = {R, G, B},

    ec_ps:add(#part{pos={Xs1,Ys1,Zs1}, vel={Vx,Vy,Vz}, col=Col}).
