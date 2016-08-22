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
import com.sandklef.coachapp.model.CoachAppBase;
import com.sandklef.coachapp.model.Media;

public class MediaStatusNameFilter implements MediaFilter {

    private final static String LOG_TAG = MediaStatusNameFilter.class.getSimpleName();

    private String name;
    private int    status;

    public MediaStatusNameFilter() {
        name=null;
        status=-1;
    }

    static public MediaStatusNameFilter newMediaFilterName(String needle) {
        MediaStatusNameFilter bf = new MediaStatusNameFilter();
        bf.name=needle;
        return bf;
    }

    static public MediaStatusNameFilter newMediaFilterStatus(int status) {
        Log.d(LOG_TAG, " newMediaFilterStatus (" + status + ")");
        MediaStatusNameFilter bf = new MediaStatusNameFilter();
        bf.status=status;
        return bf;
    }


    @Override
    public boolean check(Media m) {
        return ( (name==null || m.toString().contains(name)) &&
                ( (status==-1) || (m.getStatus()==status) ));
    }

    /*
    public List<Media> apply(List<Media> list) {
        List filteredList = new ArrayList<Media>();
        for (Media b: list){

            // Treat unset as include

            if ( (name==null || b.toString().contains(name)) &&
                    ( (status==-1) || (b.getStatus()==status) )){
                Log.d(LOG_TAG, " adding to filtered Media list: " + b);
                Log.d(LOG_TAG, "    name:     " + b.toString()+ " " + name);
                Log.d(LOG_TAG, "    status:   " + b.getStatus()+ " " + status);
                filteredList.add(b);
            } else {
                Log.d(LOG_TAG, " NOT adding to filtered Media list: " + b);
            }
        }
        return filteredList;
    }
    */
}
