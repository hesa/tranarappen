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
import com.sandklef.coachapp.storage.Storage;

import java.io.File;

/**
 * Created by hesa on 2016-02-09.
 */
public class Media extends CoachAppBase {

    /*    public enum MediaStatus {*/
    public final static int MEDIA_STATUS_UNDEFINED     = 0;
    public final static int MEDIA_STATUS_NEW           = 1;
    public final static int MEDIA_STATUS_CREATED       = 2;
    public final static int MEDIA_STATUS_UPLOADED      = 3;
    public final static int MEDIA_STATUS_AVAILABLE     = 4;
    public final static int MEDIA_STATUS_UPLOAD_FAILED = 5;
    public final static int MEDIA_STATUS_DOWNLOADED    = 6;
    public final static int MEDIA_STATUS_DELETABLE     = 7;
    public final static int MEDIA_STATUS_FROMSERVER    = 8;
  /*  } */

    private String file;
    private int status;
    private long date;
    private String teamUuid;
    private String trainingPhaseUuid;
    private String memberUuid;

    private final static String LOG_TAG = Media.class.getSimpleName();

    public Media(String uuid,
                 String name,
                 String clubUuid,
                 String file,
                 int status,
                 long date,
                 String teamUuid,
                 String trainingPhaseUuid,
                 String memberUuid) {
        super(uuid, name, clubUuid);

     //   Log.d(LOG_TAG, "--> Constructing new Media with uuid: " + getUuid());
        this.file = file;
        this.status = status;
        this.date = date;
        this.teamUuid = teamUuid;
        this.trainingPhaseUuid = trainingPhaseUuid;
        this.memberUuid = memberUuid;
        //  Log.d(LOG_TAG, "<-- Constructing new Media ");
    }


    public Media(File f) {
        super(null, null, LocalStorage.getInstance().getCurrentClub());
        file = f.getAbsolutePath();
        status = MEDIA_STATUS_UNDEFINED;
    }

    public static Media newInstructionVideo(String f, String  tpUuid) {
        return new Media(null, "", LocalStorage.getInstance().getCurrentClub(),
                f, MEDIA_STATUS_NEW, System.currentTimeMillis(), null, tpUuid, null);
    }

    public void setStatus(int s) {
        status = s;
    }

    public static String statusToString(int s) {
        switch (s) {
            case MEDIA_STATUS_UNDEFINED:
                return "undefined";
            case MEDIA_STATUS_NEW:
                return "new";
            case MEDIA_STATUS_CREATED:
                return "created";
            case MEDIA_STATUS_UPLOADED:
                return "uploaded";
            case MEDIA_STATUS_AVAILABLE:
                return "available";
            case MEDIA_STATUS_UPLOAD_FAILED:
                return "uploaded";
            case MEDIA_STATUS_DOWNLOADED:
                return "downloaded";
            case MEDIA_STATUS_DELETABLE:
                return "deletable";
            case MEDIA_STATUS_FROMSERVER:
                return "from server";
        }
        return "WARNING";
    }

    public String fileName() {
        return file;
    }

    public void setFileName(String f) {
        file=f;
    }

    public int getStatus() {
        return status;
    }

    public long getDate() {
        return date;
    }

    public String getTeam() {
        return teamUuid;
    }

    public String getTrainingPhase() {
        return trainingPhaseUuid;
    }

    public String getMember() {
        return memberUuid;
    }

    public String toString() {
/*        return super.toString() +
                " " + getDate()
                + " " + getTeam()
                + " " + getTrainingPhase()
                + " " + getDate()
                + " " + getStatus();
*/
        Member member = Storage.getInstance().getMemberUUid(getMember());
        String result = CADateFormat.getDateString(getDate());
        if (member!=null) {
            result = result + " (" + member.getName() + ") " + getUuid() + " [" + statusToString(getStatus()) + "]";}
        return result;//+ super.toString() + "-" + file  ;
    }

}
