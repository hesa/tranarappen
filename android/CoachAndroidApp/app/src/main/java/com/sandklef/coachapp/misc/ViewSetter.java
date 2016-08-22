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

import android.app.Activity;
import android.widget.TextView;

import coachassistant.sandklef.com.coachapp.R;

public class ViewSetter {
    private final static String LOG_TAG = ViewSetter.class.getSimpleName();

    public static void setViewText(Activity activity, int id, String text) {
        Log.d(LOG_TAG, "Setting text \"" + text + "\" on " + id);
        TextView tv = (TextView) activity.findViewById(id);
        if (tv != null) {
            tv.setText(text);
        }
    }
}
