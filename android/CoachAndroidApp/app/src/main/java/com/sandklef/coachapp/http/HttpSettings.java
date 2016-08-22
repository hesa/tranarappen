/******************************************************************************
 *      Copyright (c) 2015 - 2016 Henrik Sandklef
 *
 *  This file is part of Coach Assistant
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 ******************************************************************************/

package com.sandklef.coachapp.http;

public class HttpSettings {

    public static final String PATH_SEPARATOR = "/";
    public static final String CLUB_PATH      = "clubs"  + PATH_SEPARATOR;
    public static final String VIDEO_URL_PATH = "videos" + PATH_SEPARATOR;
    public static final String LOGIN_PATH     = "login" ;
    public static final String UUID_PATH      = "uuid"   + PATH_SEPARATOR;
    public static final String UPLOAD_PATH    = "upload";
    public static final String COMPOSITE_PATH = "composite";
    public static final String DOWNLOAD_PATH  = "download";
    public static final String CONTENT_STATUS = "Content-Type";
    public static final String USER_PATH      = "user-info" ;
    public static final String API_VERSION    = "0.0.0" + PATH_SEPARATOR;

    public static final String HTTP_POST      = "POST";
    public static final String HTTP_GET       = "GET";

    public static final int HTTP_RESPONSE_OK_LOW  = 200;
    public static final int HTTP_RESPONSE_OK_HIGH = 299;

    private static boolean isBetweenInclusive(int value, int low, int high) {
        return ((value >= low) && (value <= high));
    }

    public static boolean isResponseOk(int response) {
        return isBetweenInclusive(response, HTTP_RESPONSE_OK_LOW, HTTP_RESPONSE_OK_HIGH);
    }


}
