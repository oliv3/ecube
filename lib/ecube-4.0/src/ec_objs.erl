-module(ec_objs).
-author('olivier@biniou.info').
-vsn("3.0").

-include_lib("wx/include/gl.hrl"). 

-export([axes/1, plane/1, cube/1, textured/2]).


-define(ZERO, 0.0).
-define(ONE,  1.0).
-define(MAXP, 1.5).
-define(MAXA, 2.0).
-define(MC,   64).

-define(CUBE, {{ ?ONE,  ?ONE, -?ONE},   %1
	       { ?ONE, -?ONE, -?ONE},   %2
	       {-?ONE, -?ONE, -?ONE},
	       {-?ONE,  ?ONE, -?ONE},   %4
	       {-?ONE,  ?ONE,  ?ONE},
	       { ?ONE,  ?ONE,  ?ONE},   %6
	       { ?ONE, -?ONE,  ?ONE},
	       {-?ONE, -?ONE,  ?ONE}}). %8

-define(E(X), gl:vertex3fv(element(X, ?CUBE))).


set_model_view(V) ->
    gl:matrixMode(?GL_MODELVIEW),
    gl:loadIdentity(),
    gl:scalef(V, V, V).


cube(V) ->
    set_model_view(V),
    gl:lineWidth(2),
    gl:'begin'(?GL_LINES),
    gl:color3f(?ONE, ?ONE, ?ONE),
    ?E(1), ?E(2), ?E(2), ?E(3), ?E(3), ?E(4),
    ?E(4), ?E(5), ?E(5), ?E(8), ?E(8), ?E(3),
    ?E(1), ?E(6), ?E(6), ?E(7), ?E(7), ?E(2),
    ?E(6), ?E(5), ?E(5), ?E(8), ?E(8), ?E(7),
    ?E(6), ?E(1), ?E(1), ?E(4), ?E(4), ?E(5),
    ?E(7), ?E(2), ?E(3), ?E(8),
    gl:'end'().


axes(V) ->
    set_model_view(V),
    gl:lineWidth(5),
    gl:'begin'(?GL_LINES),
    gl:color3ub(0, ?MC, ?MC),
    gl:vertex3f(-?MAXA, 0, 0),
    gl:color3ub(255, ?MC, ?MC),
    gl:vertex3f(?MAXA, 0, 0),
    gl:color3ub(?MC, 0, ?MC),
    gl:vertex3f(0, -?MAXA, 0),
    gl:color3ub(?MC, 255, ?MC),
    gl:vertex3f(0, ?MAXA, 0),
    gl:color3ub(?MC, ?MC, 0),
    gl:vertex3f(0, 0, -?MAXA),
    gl:color3ub(?MC, ?MC, 255),
    gl:vertex3f(0, 0, ?MAXA),
    gl:'end'().


plane(V) ->
    set_model_view(V),
    gl:'begin'(?GL_QUADS),
    gl:color3ub(0, 0, 255),
    gl:vertex3f(-?MAXP, ?ZERO, -?MAXP),
    gl:color3ub(0, 255, 0),
    gl:vertex3f(-?MAXP, ?ZERO, +?MAXP),
    gl:color3ub(255, 0, 0),
    gl:vertex3f(+?MAXP, ?ZERO, +?MAXP),
    gl:color3ub(255, 255, 0),
    gl:vertex3f(+?MAXP, ?ZERO, -?MAXP),
    gl:'end'().


%% Biniou-textured cube

-define(FACES, [1,2,3,4,5,6]).

start(TexId) ->
    gl:enable(?GL_TEXTURE_2D),
    gl:bindTexture(?GL_TEXTURE_2D, TexId),
    gl:'begin'(?GL_QUADS).


finish() ->
    gl:'end'(),
    gl:disable(?GL_TEXTURE_2D).


textured(V, Texs) ->
    NTS = tuple_size(Texs),
    set_model_view(V),
    [tex_face6(F, NTS, Texs) || F <- ?FACES].


tex_face6(Face, NTex, Texs) ->
    Pos = face2texpos(Face, NTex),
    TexId = element(Pos, Texs),
    tex_face6b(Face, TexId).


t1() ->
    gl:texCoord2f(?ZERO, ?ZERO).
t2() ->
    gl:texCoord2f(?ZERO, ?ONE).
t3() ->
    gl:texCoord2f(?ONE, ?ONE).
t4() ->
    gl:texCoord2f(?ONE, ?ZERO).


tex_face6b(_N, undefined) ->
    ok;
tex_face6b(1, TexId) ->
    start(TexId),
    t1(), ?E(8),
    t2(), ?E(5),
    t3(), ?E(6),
    t4(), ?E(7),
    finish();
tex_face6b(2, TexId) ->
    start(TexId),
    t1(), ?E(2),
    t2(), ?E(1),
    t3(), ?E(4),
    t4(), ?E(3),
    finish();
tex_face6b(3, TexId) ->
    start(TexId),
    t1(), ?E(3),
    t2(), ?E(4),
    t3(), ?E(5),
    t4(), ?E(8),
    finish();
tex_face6b(4, TexId) ->
    start(TexId),
    t1(), ?E(7),
    t2(), ?E(6),
    t3(), ?E(1),
    t4(), ?E(2),
    finish();
tex_face6b(5, TexId) ->
    start(TexId),
    t1(), ?E(5),
    t2(), ?E(4),
    t3(), ?E(1),
    t4(), ?E(6),
    finish();
tex_face6b(6, TexId) ->
    start(TexId),
    t1(), ?E(3),
    t2(), ?E(8),
    t3(), ?E(7),
    t4(), ?E(2),
    finish().    


%% Textures positioning
%% 1 texture
face2texpos(_, 1) ->
    1;
%% 2 textures, render them in 2 stripes
face2texpos(1, 2) ->
    1;
face2texpos(2, 2) ->
    1;
face2texpos(3, 2) ->
    1;
face2texpos(_N, 2) ->
    2;
%% 3 textures, render them on opposite sides
face2texpos(1, 3) ->
    1;
face2texpos(2, 3) ->
    1;
face2texpos(3, 3) ->
    2;
face2texpos(4, 3) ->
    2;
face2texpos(_N, 3) ->
    3;
%% Other cases
face2texpos(N, NbTextures) ->
    (N rem NbTextures) + 1.
