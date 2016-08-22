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

package com.sandklef.coachapp.misc;


import com.sandklef.coachapp.storage.Storage;

public class Log {

    /*  RELEASE APP SETTINGS*/
    static final boolean DISABLE_ALL_LOG = true;

    static final  boolean LOG = true;
    static public boolean LOG_CONSTRUCTOR_ONLY = false;

    static final boolean LOG_I = true;
    static final boolean LOG_E = true;
    static final boolean LOG_D = true;
    static final boolean LOG_V = true;
    static final boolean LOG_W = true;


    public static void c(String tag, String string) {
        if (DISABLE_ALL_LOG) { return ; }

//        android.util.Log.d("LOG", "c: " + LOG_CONSTRUCTOR_ONLY);
        if (LOG_CONSTRUCTOR_ONLY) { android.util.Log.d(tag, string) ; }
        else { Log.d(tag, string);}
    }

    public static void i(String tag, String string) {
        if (DISABLE_ALL_LOG) { return ; }

        if (((LOG || LOG_I) && (!LOG_CONSTRUCTOR_ONLY))) android.util.Log.i(tag, string);
    }

    public static void e(String tag, String string) {
        if (DISABLE_ALL_LOG) { return ; }

        if (((LOG || LOG_E) && (!LOG_CONSTRUCTOR_ONLY))) android.util.Log.e(tag, string);
    }

    public static void d(String tag, String string) {
        if (DISABLE_ALL_LOG) { return ; }

        // android.util.Log.d("LOG", "d: " + LOG + " " + LOG_D + " " + LOG_CONSTRUCTOR_ONLY);
        if (((LOG || LOG_D) && (!LOG_CONSTRUCTOR_ONLY))) android.util.Log.d(tag, string);
    }

    public static void v(String tag, String string) {
        if (DISABLE_ALL_LOG) { return ; }

        if ((LOG || LOG_V) && (!LOG_CONSTRUCTOR_ONLY) ) android.util.Log.v(tag, string);
    }

    public static void w(String tag, String string) {
        if (DISABLE_ALL_LOG) { return ; }

        if ((LOG || LOG_W) && (!LOG_CONSTRUCTOR_ONLY) ) android.util.Log.w(tag, string);
    }

}
