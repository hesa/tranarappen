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

package com.sandklef.coachapp.model;

import com.sandklef.coachapp.misc.CADateFormat;
import com.sandklef.coachapp.misc.Log;
import com.sandklef.coachapp.storage.LocalStorage;

import java.security.acl.LastOwnerException;
import java.text.SimpleDateFormat;
import java.util.Date;

/**
 * Created by hesa on 2016-02-26.
 */
public class LogMessage {

    private String message;
    private String detail;
    private String clubUuid;
    private Date   date;
    private long   id;

    private final static String LOG_TAG = LogMessage.class.getSimpleName();


    //        String[] projectionArray = {LOG_ID, CLUB_COLUMN_NAME, LOG_MSG, LOG_DATE};

    public LogMessage(long id,
                      String clubUuid,
                      String msg,
                      String detail,
                      Date   date) {
        this.id       = id;
        this.clubUuid = clubUuid;
        this.message  = msg;
        this.detail   = detail;
        this.date     = date;
    }

    public String getMesssage() {
        return message;
    }
    public String getDetail() {
        return detail;
    }
    public Date getDate() {
        return date;
    }

    public String toString() {
        // Log.d(LOG_TAG, "LOG: converting date: " + date);
        return CADateFormat.getDateString(date) + "\n" + message;
    }

}
