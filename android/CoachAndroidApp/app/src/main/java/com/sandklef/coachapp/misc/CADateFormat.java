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

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.TimeZone;

public class CADateFormat {

    public static SimpleDateFormat sdf;
    public static SimpleDateFormat sdfDay;
    public static SimpleDateFormat sdfTime;
    public static SimpleDateFormat sdfDayUTC;
    public static SimpleDateFormat sdfTimeUTC;

    static {
        sdf        = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
        sdfDay     = new SimpleDateFormat("yyyy-MM-dd");
        sdfTime    = new SimpleDateFormat("HH:mm:ss");
        sdfDayUTC  = new SimpleDateFormat("yyyy-MM-dd");
        sdfTimeUTC = new SimpleDateFormat("HH:mm:ss");

        sdfDayUTC.setTimeZone(TimeZone.getTimeZone("UTC"));
        sdfTimeUTC.setTimeZone(TimeZone.getTimeZone("UTC"));
    }

//    public static String DATE_FORMAT = "yyyyMMdd-HHmmss";

    public static Date getDate(long time) {
        return new Date(time);
    }

    public static String getDateStringForServerUTC(long time) {
//        Goal: 2016-06-11T23:29:12.935412Z
        Date d     = new Date(time);
        String ds   = sdfDayUTC.format(d);
        String ts   = sdfTimeUTC.format(d);
        return ds + "T" + ts + ".000000Z";
    }

    public static String getDateString(Date d) {
//        java.text.DateFormat df = new SimpleDateFormat(DATE_FORMAT);
        return sdf.format(d);
    }

    public static String getDayString(Date d) {
        return sdfDay.format(d);
    }

    public static String getTimeString(Date d) {
        return sdfTime.format(d);
    }

    public static String getDateString(long time) {
//        java.text.DateFormat df = new SimpleDateFormat(DATE_FORMAT);
        return getDateString(getDate(time));
    }


}
