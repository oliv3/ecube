%% Includes
-include_lib("wx/include/wx.hrl"). 
-include_lib("wx/include/gl.hrl"). 

%% Options
-include("ec_options.hrl").

%% Debug
-include("ec_debug.hrl").

%% Application name
-define(APPNAME, ecube).

%% Older versions
-define(V1, "1.0").
-define(V2, "2.0").

%% OTP supervisors timeout
-define(TIMEOUT, 5000).

%% Default splash picture
-define(SPLASH, "splash.png").

%% Default volume
-define(DEFAULT_VOLUME, 1.0).

%% Image
-record(img, {w, h, data,
	      pf = ?GL_RGB,          %% pixel format
	      pt = ?GL_UNSIGNED_BYTE %% pixel type
	     }).
