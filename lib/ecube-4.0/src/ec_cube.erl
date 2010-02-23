-module(ec_cube).
-author('olivier@biniou.info').
-vsn("3.0").

-include("ec.hrl").

-export([draw/3]).

-define(ROT_STEP, 0.31416).


draw(TexSize, Splash, Texs) ->
    NewTexs = refresh_textures(TexSize, Splash, Texs, 0, tuple_size(Texs), []),
    draw_objects(NewTexs),
    spin(ec_cf:opt(?O_SPIN)),
    NewTexs.


draw_objects(Texs) ->
    Vol = case ec_cf:opt(?O_MUTE) of
	      true -> ?DEFAULT_VOLUME;
	      false -> ec_vol:get()
	  end,
    case ec_cf:opt(?O_PLANE) of
	true -> ec_objs:plane(Vol);
	false -> ok
    end,
    case ec_cf:opt(?O_AXES) of
	true -> ec_objs:axes(Vol);
	false -> ok
    end,
    case ec_cf:opt(?O_EDGES) of
	true -> ec_objs:cube(Vol);
	false -> ok
    end,
    case ec_cf:opt(?O_TEXT) of
	true -> ec_objs:textured(Vol, Texs);
	false -> ok
    end.


spin(true) ->
    {OldRX, OldRY, OldRZ} = ec_cf:rot(),
    NewRX = OldRX, %% + 1 * ?ROT_STEP,
    NewRY = OldRY - 2 * ?ROT_STEP,
    NewRZ = OldRZ, %% + 3 * ?ROT_STEP,
    ec_cf:rot({NewRX, NewRY, NewRZ});
spin(false) ->
    ok.


refresh_textures(_TexSize, _Splash, _Texs, Max, Max, Acc) ->
    list_to_tuple(lists:reverse(Acc));
refresh_textures(TexSize, Splash, Texs, N0, Max, Acc) ->
    N = N0 + 1,
    OldTex = element(N, Texs),
    NewTex = ec_tex:get(N),
    Item = case NewTex of
	       unchanged ->
		   OldTex;

	       {data, Data} ->
		   make_texture(TexSize, Splash, N, OldTex, Data)
	   end,
    refresh_textures(TexSize, Splash, Texs, N0+1, Max, [Item|Acc]).


make_texture(TexSize, Splash, N, TexId, Data) ->
    if
	TexId =/= undefined andalso TexId =/= Splash ->
	    ec_gui:unregister_texture(self(), TexId);

	true ->
	    ok
    end,
    case ec_tex:process(N, Data) of
	undefined ->
	    Splash;
	Img ->
	    make(Img, TexSize)
    end.


make(Img, TexSize) ->
    [TexId] = gl:genTextures(1),
    gl:bindTexture(?GL_TEXTURE_2D, TexId),
    %% Those two calls not used since we map the texture in [0.0 .. 1.0]
    %% gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_WRAP_S, ?GL_REPEAT),
    %% gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_WRAP_T, ?GL_REPEAT),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_MAG_FILTER, ?GL_LINEAR),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_MIN_FILTER, ?GL_LINEAR),

    Mem = if
	      Img#img.w == TexSize, Img#img.h == TexSize ->
		  Img#img.data;

	      true ->
		  Img0 = wxImage:new(Img#img.w, Img#img.h, Img#img.data),
		  Img1 = wxImage:scale(Img0, TexSize, TexSize),
		  wxImage:getData(Img1)
	  end,
    gl:texImage2D(?GL_TEXTURE_2D, 0, Img#img.pf, TexSize, TexSize,
		  0, Img#img.pf, Img#img.pt, Mem),
    ec_gui:register_texture(self(), TexId),
    TexId.
