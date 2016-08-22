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

package com.sandklef.coachapp.storage;


import com.sandklef.coachapp.model.Media;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.io.File;
import java.util.Date;

public class LocalMediaStorage  {
/*

    File dir ;

    public LocalMediaStorage() {
        dir = new File(LocalStorage.getInstance().getNewMediaDir());
        createMediaDir();
    }


    private Collection<Media> getMediaFiles(String[] extensions, boolean rec) {
        Collection<Media> list = new ArrayList<Media>();

        if (!dir.exists()) {
            return list;
        }
        File[] files = dir.listFiles();

        for (File f : files) {
            if (f.isDirectory()) {
                if (rec) {
                    list.addAll(getMediaFiles(extensions, rec));
                }
            } else {
                String name = f.getName().toLowerCase();
		
                for (String ext : extensions) {
                    if (name.endsWith(ext)) {
                        list.add(new Media(f));
                    }
                }
            }
        }
        return list;
    }


    public static String getMediaFileNamePrefix() {
        return LocalStorage.DEFAULT_COACHAPP_DATA_DIR;
    }

    public String getNextImageName() {
        return getMediaFileNamePrefix()+LocalStorage.COACHAPP_IMAGE_SUFFIX;
    }

    public String getNextMovieName() {
        return getMediaFileNamePrefix()+LocalStorage.COACHAPP_VIDEO_SUFFIX;
    }

    private boolean createMediaDir() {
        if (!dir.exists()) {
            return dir.mkdirs();
        }
        return true;
    }
*/
}
