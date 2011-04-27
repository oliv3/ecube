%% -*- erlang -*-

{application, ecube,
	[
	 {description, "Le Biniou powered by wxErlang"},
	 {vsn, "4.0"},
	 {modules, [ec_app, ec, ec_sup, ec_gui, ec_cf,
	  ec_win, ec_gl, ec_demo, ec_vol, ec_tex, ec_objs, ec_cube, ec_biniou,
	  wx_glfont, ec_osd, ec_pt3d, ec_ps]},
	 {registered, [ec_sup, ec_gui, ec_cf,
	  ec_win, ec_gl, ec_demo, ec_vol, ec_tex, ec_osd, ec_m3d, ec_pcap, ec_pt3d, ec_ps]},
	 {applications, [kernel, stdlib]},
	 {env, [
%%	       {fps, 60},
	       {fps, 30},
	       {tex_size, 256},
	       {size, {640, 480}},
	       %%	       {n, 2},
	       {n, 1},
%%	       {biniou_size, {256, 256}},
	       {biniou_size, {640, 480}},
	       {biniou_fps, 25}
	       ]},
	 {mod, {ec_app, []}}
	]}.
