%% -*- erlang -*-

{application, ecube,
	[
	 {description, "Le Biniou powered by wxErlang"},
	 {vsn, "4.0"},
	 {modules, [ec_app, ec, ec_sup, ec_gui, ec_cf,
		    ec_win, ec_gl, ec_demo, ec_vol, ec_tex,
		    ec_objs, ec_cube, ec_biniou, ec_rec,
		    wx_glfont, ec_osd, ec_crystal, ec_ps, ec_base]},
	 {registered, [ec_sup, ec_gui, ec_cf, ec_rec,
		       ec_win, ec_gl, ec_demo, ec_vol, ec_tex, ec_osd,
		       ec_crystal, ec_ps, ec_base]},
	 {applications, [kernel, stdlib]},
	 {env, [
%%	       {fps, 60},
	       {fps, 25},
	       {tex_size, 256},
	       {size, {640, 480}},
	       %%	       {n, 2},
	       {n, 1},
%%	       {biniou_size, {256, 256}},
	       {biniou_size, {320, 240}},
	       {biniou_fps, 20}
	       ]},
	 {mod, {ec_app, []}}
	]}.
