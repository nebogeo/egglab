// Project Nightjar Copyright (C) 2014 Dave Griffiths
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
///////////////////////////////////////////////////////////////////////////

// taken from the naked on pluto codebase

function fb_interface(appid)
{  
    this.accessToken=false;
    this.uid=false;
    this.me=null;
    
    // called automatically from login
    this.get_user=function() {
        var fb=this;
        FB.api('/me', function(response) {
            fb.me = response;
	    console.log(fb.me);
        });
    };

    // login or check login status
    this.login=function()
    {
        var fb=this;
        console.log("attempting login");

	console.log(FB);

        console.log(FB.getLoginStatus(function(response) {
            console.log(response);
            if (response.status=="connected") {
                console.log("logged in already...");
		fb.uid = response.authResponse.userID;
		fb.accessToken = response.authResponse.accessToken;
                fb.get_user();
	    }
	    else
	    {
                console.log("trying to log in...");
		FB.login(function(response) {
                    console.log("logging in...");
		    if (response.authResponse) {
			fb.accessToken = response.authResponse.accessToken;
                        fb.get_user();
		    }
		}, {
                    scope:'user_about_me'
                });
	    }
	}));
    };
    
    this.request_callback = function(request, to) {
        console.log(to);
    };
    
    this.request = function(message) {
        FB.ui(
            {method: 'apprequests',
             message: message
            },
            this.request_callback);
    };

    var fb=this;

    this.startup=function() {
	// startup init
	if (appid!="")
	{
            $(document).ready(function() {
		console.log("facebook init");
		FB.init({appId: appid, status: true, xfbml: true});
		fb.login();
	    });
	}
    };
    
    window.setTimeout(this.startup, 1000);
}
