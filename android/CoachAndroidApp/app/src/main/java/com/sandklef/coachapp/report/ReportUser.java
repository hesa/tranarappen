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

package com.sandklef.coachapp.report;

import android.widget.Toast;
import android.content.Context;

import com.sandklef.coachapp.Session.CoachAppSession;
import com.sandklef.coachapp.misc.Log;
import com.sandklef.coachapp.storage.Storage;

public class ReportUser {

    public static void warning(Context context, String text, String detail) {
        Toast toast = Toast.makeText(context, text, Toast.LENGTH_LONG);
        toast.show();
        log(text, detail);
    }

    public static void warning(Context context, int id, int detailedId) {
        String text   = CoachAppSession.getInstance().getContext().getString(id);
        String detail = CoachAppSession.getInstance().getContext().getString(detailedId);
        warning(context, text, detail);
    }

    public static void inform(Context context, String text) {
        Toast toast = Toast.makeText(context, text, Toast.LENGTH_LONG);
        toast.show();
    }

    public static void informError(Context context, String text) {
        Toast toast = Toast.makeText(context, "Error: " + text, Toast.LENGTH_LONG);
        toast.show();
    }

    public static void inform(Context context, int textId) {
        String text = CoachAppSession.getInstance().getContext().getString(textId);
        Toast toast = Toast.makeText(context, text, Toast.LENGTH_LONG);
        toast.show();
    }

    public static void log(String msg, String detail) {
        Storage.getInstance().log(msg, detail);
    }

    // TODO: remove me
    public static void Log(String msg, String detail) {
     //   Storage.getInstance().log(msg, detail);
        Storage.getInstance().log(msg, detail);
    }

    public static void Log(int msgId, int detailId) {
        Storage.getInstance().log(CoachAppSession.getInstance().getContext().getString(msgId),
                CoachAppSession.getInstance().getContext().getString(detailId));
    }

    public static void Log(int msgId, String text) {
        Storage.getInstance().log(CoachAppSession.getInstance().getContext().getString(msgId),
                text);
    }

}
