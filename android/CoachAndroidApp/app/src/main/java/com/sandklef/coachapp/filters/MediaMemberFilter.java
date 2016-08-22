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


package com.sandklef.coachapp.filters;


import com.sandklef.coachapp.misc.Log;
import com.sandklef.coachapp.model.Media;

public class MediaMemberFilter implements MediaFilter {

    private final static String LOG_TAG = MediaMemberFilter.class.getSimpleName();

    private String uuid;

    public MediaMemberFilter() {
        uuid=null;
    }

    static public MediaMemberFilter newMediaMemberFilter(String uuid) {
        MediaMemberFilter bf = new MediaMemberFilter();
        bf.uuid=uuid;
        return bf;
    }


    @Override
    public boolean check(Media m) {
        Log.d(LOG_TAG, "check media:   member: " + m.getMember());
        if (m.getMember()==null && uuid==null) {
            return true;
        } else if (uuid==null) {
            return false;
        }
        return (uuid.equals(m.getMember()));
    }

}
