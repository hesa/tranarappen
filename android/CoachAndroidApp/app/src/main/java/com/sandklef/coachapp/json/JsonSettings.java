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

package com.sandklef.coachapp.json;

public class JsonSettings {

    // SERVER TYPE TAGS
    public static final String TRAINING_PHASES_TAG = "trainingPhases";
//    public static final String VIDEOS_TAG = "videos";
    public static final String TEAMS_TAG = "teams";
    public static final String MEMBERS_TAG = "members";

    // SERVER JSON TAGS
    public static final String UUID_TAG  = "uuid";
    public static final String ID_TAG    = "id";
    public static final String NAME_TAG  = "name";
    public static final String CLUB_TAG  = "clubUuid";
    public static final String TEAM_TAG  = "teamUuid";
    public static final String ITEMS_TAG = "items";
    public static final String INSTANCES_TAG = "instances";
    public static final String TRAININGPHASE_TAG = "trainingPhaseUuid";
    public static final String MEMBER_TAG = "memberUuid";
    public static final String STATUS_TAG = "status";
    public static final String CREATED_TAG = "created";
    public static final String TOKEN_TAG   = "token";
    public static final String VIDEO_TAG   = "videoUuid";
    public static final String RECORDED_DATE_TAG = "recorded";

    // SERVER VIDEO TAGS
    public static final String SERVER_VIDEO_EMPTY_TAG = "empty";
    public static final String SERVER_VIDEO_PROCESSING_TAG = "processing";
    public static final String SERVER_VIDEO_COMPLETE_TAG = "complete";
    public static final String SERVER_VIDEO_FAILURE_TAG = "failure";

    // TODO: Move this somewhere else
    public static final String SERVER_VIDEO_SUFFIX = ".webm";

}
