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

import com.sandklef.coachapp.misc.Log;
import com.sandklef.coachapp.model.*;
import com.sandklef.coachapp.report.ReportUser;


import java.security.acl.LastOwnerException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;

import android.content.ContentValues;
import android.content.Context;
import android.database.sqlite.SQLiteConstraintException;
import android.database.sqlite.SQLiteDatabase;
import android.database.sqlite.SQLiteOpenHelper;
import android.database.Cursor;

public class BaseStorageHelper extends SQLiteOpenHelper {

    private final static String LOG_TAG = BaseStorageHelper.class.getSimpleName();

    // Tables
    private static final String TEAM_TABLE          = "teams";
    private static final String MEMBER_TABLE        = "members";
    private static final String TRAININGPHASE_TABLE = "trainingphases";
    private static final String MEDIA_TABLE         = "media";
    private static final String LOG_TABLE           = "eventlog";
    private static final String LOCAL_USER_TABLE     = "local_user";

    // Base
    public static final String UUID_COLUMN_NAME = "uuid";
    public static final String NAME_COLUMN_NAME = "name";
    public static final String CLUB_COLUMN_NAME = "club_uuid";

    // extending Base
    public static final String TEAM_COLUMN_NAME     = "team_uuid";
    public static final String MEMBER_COLUMN_NAME   = "member_uuid";
    public static final String TRAININGPHASE_COLUMN_NAME = "trainingphase_uuid";
    public static final String URI_COLUMN_NAME      = "uri";
    public static final String STATUS_COLUMN_NAME   = "status";
    public static final String DATE_COLUMN_NAME     = "date";
    public static final String VIDEO_COLUMN_NAME    = "video_uuid";
    // Event Log
    public static final String LOG_ID        = "id";
    public static final String LOG_MSG       = "message";
    public static final String LOG_DETAIL    = "detail";
    public static final String LOG_DATE      = "date";

    // Local User
    public static final String LOCAL_USER_ID           = "id";
    public static final String LOCAL_USER_NAME         = "name";
    public static final String LOCAL_USER_EMAIL        = "email";
    public static final String LOCAL_USER_PASSWORD     = "password";  // currently not used
    public static final String LOCAL_USER_CLUBS        = "clubs";
    public static final String LOCAL_USER_LATEST_CLUB  = "latest_club";
    public static final String LOCAL_USER_TOKEN        = "token";


    // General
    public static final int DATABASE_VERSION = 1;
    public static final String DATABASE_NAME = "coachassistant.db";
    public static final String SORT_ORDER = "";
//    public static final String SORT_ORDER = NAME_COLUMN_NAME + " ASC";


    private Context context;
//    private String  currentClubUuid;

    private boolean isCreating = false;
    private SQLiteDatabase currentDB = null;

//    private  db;

    private static final String BASE_COLUMNS =
            BaseStorageHelper.UUID_COLUMN_NAME + " text primary key," +
                    BaseStorageHelper.CLUB_COLUMN_NAME + " text ," +
                    BaseStorageHelper.NAME_COLUMN_NAME + " text ";

    private static final String LOG_COLUMNS =
            LOG_ID + " text primary key," +
                    CLUB_COLUMN_NAME     + " text ," +
                    LOG_MSG + " text , " +
                    LOG_DETAIL + " text , " +
                    LOG_DATE + " long ";

    private static final String LOCAL_USER_COLUMNS =
            LOCAL_USER_ID           + " int primary key," +
                    LOCAL_USER_NAME         + " text," +
                    LOCAL_USER_EMAIL        + " text," +
                    LOCAL_USER_PASSWORD     + " text," +
                    LOCAL_USER_CLUBS        + " text," +
                    LOCAL_USER_LATEST_CLUB  + " text , " +
                    LOCAL_USER_TOKEN        + " text  " ;

    private String buildCreateString(String tableName, String col) {
        if (col == null) {
            col = "";
        }
        return "CREATE TABLE " + tableName + "(" + BASE_COLUMNS + col + ");";
    }

    private String buildDeleteString(String tableName) {
        return "DROP TABLE IF EXISTS " + tableName;
    }

    public BaseStorageHelper(Context context) {
        super(context, BaseStorageHelper.DATABASE_NAME,
                null, BaseStorageHelper.DATABASE_VERSION);

        this.context = context;
    }

    public static ContentValues buildContentValues(CoachAppBase b) {
        ContentValues values = new ContentValues();
        values.put(BaseStorageHelper.UUID_COLUMN_NAME, b.getUuid());
        values.put(BaseStorageHelper.NAME_COLUMN_NAME, b.getName());
        values.put(BaseStorageHelper.CLUB_COLUMN_NAME, b.getClubUuid());
        return values;
    }

    private void logExecSQL(String sql) {
        SQLiteDatabase db = getWritableDatabase();
        Log.d(LOG_TAG, "SQL (" + db + ") stmt: " + sql);

        db.execSQL(sql);
    }

    public void onCreate(SQLiteDatabase db) {
        isCreating = true;
        currentDB = db;
        logExecSQL(buildCreateString(TEAM_TABLE, null));
        logExecSQL(buildCreateString(MEMBER_TABLE, ", " + TEAM_COLUMN_NAME + " text"));
        logExecSQL(buildCreateString(MEDIA_TABLE, ", "
                + TEAM_COLUMN_NAME + " text, "
                + URI_COLUMN_NAME + " text, "
                + STATUS_COLUMN_NAME + "  int,"
                + TRAININGPHASE_COLUMN_NAME + " text, "
                + MEMBER_COLUMN_NAME + " text, "
                + DATE_COLUMN_NAME + " int"));


        logExecSQL(buildCreateString(TRAININGPHASE_TABLE,  ", " + VIDEO_COLUMN_NAME + " text"));

        Log.d(LOG_TAG, "Creating log table: " + LOG_COLUMNS);
        logExecSQL("CREATE TABLE " + LOG_TABLE + " ( " + LOG_COLUMNS + " ); ");
        logExecSQL("CREATE TABLE " + LOCAL_USER_TABLE + " ( " + LOCAL_USER_COLUMNS + " ); ");

        isCreating = false;
        currentDB = null;
    }

    public void onUpgrade(SQLiteDatabase db, int oldVersion, int newVersion) {
        db.execSQL(buildDeleteString(MEMBER_TABLE));
        db.execSQL(buildDeleteString(TEAM_TABLE));
        db.execSQL(buildDeleteString(TRAININGPHASE_TABLE));
        db.execSQL(buildDeleteString(MEDIA_TABLE));
        db.execSQL(buildDeleteString(LOG_TABLE));
        onCreate(db);
    }

    public void onDowngrade(SQLiteDatabase db, int oldVersion, int newVersion) {
        onUpgrade(db, oldVersion, newVersion);
    }

    public Context getContext() {
        return context;
    }

    public ArrayList<String> buildProjection(String[] projections) {
        ArrayList<String> projs = new ArrayList<String>();
        projs.add(UUID_COLUMN_NAME);
        projs.add(NAME_COLUMN_NAME);
        projs.add(CLUB_COLUMN_NAME);

        if (projections != null) {
            for (int i = 0; i < projections.length; i++) {
                projs.add(projections[i]);
            }
        }
        return projs;
    }

    public String[] buildProjectionArray(String[] projections) {
        ArrayList<String> projs = buildProjection(projections);
        Object[] projsOArray = projs.toArray();
        String[] projsArray = Arrays.copyOf(projsOArray,
                projsOArray.length, String[].class);
        return projsArray;
    }


  /*  public void setClubUuid(String club) {
        currentClubUuid = club;
    }
*/

    private Cursor getCursorFirst(String tbl, String[] projections)  {
        //  SQLiteDatabase db = getWritableDatabase();

        /*
        Log.d(LOG_TAG, "  projection array: " + buildProjectionArray(projections));
        Log.d(LOG_TAG, "  Printing projection array: " + buildProjectionArray(projections).length);
        if (projections != null) {
            for (String a : buildProjectionArray(projections)) {
                Log.d(LOG_TAG, "  * " + a);
            }
        }
*/
        String whereClause = CLUB_COLUMN_NAME + " = ? ";
        SQLiteDatabase db = getWritableDatabase();

        Log.d(LOG_TAG, "  get cursor: club: " + LocalStorage.getInstance().getCurrentClub());

        Cursor cursor = db.query(tbl,
                buildProjectionArray(projections),
                whereClause, new String[]{LocalStorage.getInstance().getCurrentClub()}, null, null,
                BaseStorageHelper.SORT_ORDER);
        if (cursor == null) {
            return null;
        }
        cursor.moveToFirst();
        return cursor;
    }

    public void closeCursor(Cursor cursor) {
        cursor.close();
    }


    public void storeBaseToDB(SQLiteDatabase db, List<CoachAppBase> bases, String table) {
        Log.d(LOG_TAG, "Getting db " + db);
        if (db == null) {
            ReportUser.warning(getContext(), "Database problem", "Failed to get hold of db");
            throw new DBException();
        }
        try {
            db.delete(table, null, null);
        } catch (android.database.sqlite.SQLiteException e) {
            Log.d(LOG_TAG, "storeBaseToDB: " + e.getMessage());
        }

    }

    public void storeTeamsToDB(List<Team> teams) {
        Log.d(LOG_TAG, "Storing teams " + teams.size());
        SQLiteDatabase db = getWritableDatabase();
        storeBaseToDB(db, (List<CoachAppBase>) (List<? extends CoachAppBase>) teams, TEAM_TABLE);
        for (Team t : teams) {
            ContentValues values = buildContentValues(t);
            long rowId = db.insert(TEAM_TABLE, null, values);
            //    Log.d(LOG_TAG, " * " + rowId + " inserted " + t);
            if (rowId < 0) {
                Log.e(LOG_TAG, "ERROR inserting (" + rowId + "): " + t);
            }
        }
    }


    public List<Team> getTeamsFromDB() throws StorageNoClubException {
        String[] subProjectionArray = null;
        List<Team> teams = new ArrayList<Team>();
        if (LocalStorage.getInstance().getCurrentClub()==null) {
            throw new StorageNoClubException("Club not set or faulty");
        }
        Cursor cursor = getCursorFirst(TEAM_TABLE, subProjectionArray);
        while (!cursor.isAfterLast()) {
            teams.add(cursorToTeam(cursor));
            cursor.moveToNext();
        }
        closeCursor(cursor);
        return teams;
    }


    private Team cursorToTeam(Cursor cursor) {
        if (cursor == null) {
            Log.d(LOG_TAG, "Cursor is null in team database");
            return null;
        }
        Team t = new Team(cursor.getString(0),
                cursor.getString(1),
                cursor.getString(2));
        return t;
    }


    public void storeTrainingPhasesToDB(List<TrainingPhase> TrainingPhases) {
        Log.d(LOG_TAG, "Storing TrainingPhases " + TrainingPhases.size());
        SQLiteDatabase db = getWritableDatabase();
        storeBaseToDB(db, (List<CoachAppBase>) (List<? extends CoachAppBase>) TrainingPhases, TRAININGPHASE_TABLE);

        for (TrainingPhase t : TrainingPhases) {
            ContentValues values = buildContentValues(t);
            values.put(VIDEO_COLUMN_NAME, t.getVideoUuid());
            long rowId = db.insert(TRAININGPHASE_TABLE, null, values);
            if (rowId < 0) {
                Log.e(LOG_TAG, "ERROR inserting (" + rowId + "): " + t);
            }

            if (t.getVideoUuid().length()>5) {
                Log.d(LOG_TAG, "Storing media " + t.getVideoUuid() + " based on TP: " + t.getUuid() );
                Media m = new Media(t.getVideoUuid(),
                        "tmp-name"+t.getVideoUuid(),
                        t.getClubUuid(),
                        null,
                        Media.MEDIA_STATUS_AVAILABLE,
                        new Date().getDate(),
                        null,
                        t.getUuid(),
                        "");
                storeMedia(m);
            }

        }
    }


    public List<TrainingPhase> getTrainingPhasesFromDB() throws StorageNoClubException {
        String[] subProjectionArray = {VIDEO_COLUMN_NAME};

        List<TrainingPhase> trainingPhases = new ArrayList<TrainingPhase>();
        if (LocalStorage.getInstance().getCurrentClub()==null) {
            throw new StorageNoClubException("Club not set or faulty");
        }
        Cursor cursor = getCursorFirst(TRAININGPHASE_TABLE, subProjectionArray);

        while (!cursor.isAfterLast()) {
            trainingPhases.add(cursorToTrainingPhase(cursor));
            cursor.moveToNext();
        }
        closeCursor(cursor);
        return trainingPhases;
    }

    private TrainingPhase cursorToTrainingPhase(Cursor cursor) {
        if (cursor == null) {
            Log.d(LOG_TAG, "Cursor is null in TrainingPhase database");
            return null;
        }
        TrainingPhase t = new TrainingPhase(cursor.getString(0),
                cursor.getString(1),
                cursor.getString(2),
                cursor.getString(3) );
        return t;
    }

    public void storeMedia(Media m) {
        Log.d(LOG_TAG, "Storing media, file: " + m.fileName());
        Log.d(LOG_TAG, "Storing media, tp:   " + m.getTrainingPhase());
        SQLiteDatabase db = getWritableDatabase();

        ContentValues values = new ContentValues();
        if (m.getUuid()!=null) {
            Log.d(LOG_TAG, "Storing media, uuuid:  " + m.getUuid());
            values.put(BaseStorageHelper.UUID_COLUMN_NAME, m.getUuid());
        } else {
            Log.d(LOG_TAG, "Storing media, uuuid not set");
        }
        values.put(BaseStorageHelper.NAME_COLUMN_NAME, m.getName());
        values.put(BaseStorageHelper.CLUB_COLUMN_NAME, m.getClubUuid());
        values.put(TEAM_COLUMN_NAME, m.getTeam());
        values.put(URI_COLUMN_NAME, m.fileName());
        values.put(STATUS_COLUMN_NAME, m.getStatus());
        values.put(TRAININGPHASE_COLUMN_NAME, m.getTrainingPhase());
        values.put(MEMBER_COLUMN_NAME, m.getMember());
        values.put(DATE_COLUMN_NAME, m.getDate());

        Log.d(LOG_TAG, "Contentvalues: " + values);

//        long rowId = db.insertWithOnConflict(MEDIA_TABLE, null, values, SQLiteDatabase.CONFLICT_IGNORE);
        long rowId = db.insertWithOnConflict(MEDIA_TABLE, null, values, SQLiteDatabase.CONFLICT_REPLACE);
//        long rowId = db.insert(MEDIA_TABLE, null, values);
        //  Log.d(LOG_TAG, " * " + rowId + " inserted " + m + " (Storing media)");
        if (rowId < 0) {
            Log.d(LOG_TAG, "ERROR inserting, but don't worry (" + rowId + "): media uuid:" + m.getUuid());
        } else {
            Log.d(LOG_TAG, "Inserted (" + rowId + "): media uuid:" + m.getUuid());
        }


    }

    public void storeMediaToDB(List<Media> media) {
        Log.d(LOG_TAG, "Storing Media " + media.size());
        SQLiteDatabase db = getWritableDatabase();

        // TODO: better removal of media present on server (keep local)

        for (Media m : media) {
            ContentValues values = buildContentValues(m);
            values.put(TEAM_COLUMN_NAME, m.getTeam());
            values.put(URI_COLUMN_NAME, m.fileName());
            values.put(STATUS_COLUMN_NAME, m.getStatus());
            values.put(TRAININGPHASE_COLUMN_NAME, m.getTrainingPhase());
            values.put(MEMBER_COLUMN_NAME, m.getMember());
            values.put(DATE_COLUMN_NAME, m.getDate());

            long rowId = db.insert(MEDIA_TABLE, null, values);
            //          Log.d(LOG_TAG, " * " + rowId + " inserted " + m + " (Storing media)");
            if (rowId < 0) {
                Log.e(LOG_TAG, "ERROR inserting (" + rowId + "): " + m);
            }
        }
    }


    public boolean removeMediaFromDB(Media m) {
        SQLiteDatabase db = getWritableDatabase();
        Log.d(LOG_TAG, "delete media from db: " + m.getUuid());
        return db.delete(MEDIA_TABLE, UUID_COLUMN_NAME + "= '" + m.getUuid() + "'" , null) > 0;
    }

    public void removeDeletableMediaFromDB() {
        SQLiteDatabase db = getWritableDatabase();
        Log.d(LOG_TAG, "delete deletable media from db: ");
//        int rows = db.delete(MEDIA_TABLE, STATUS_COLUMN_NAME + "= '" + Media.MEDIA_STATUS_DELETABLE + "'" , null);
  //      Log.d(LOG_TAG, "delete deletable media from db, deleted " + rows + " rows");
    }


    public List<Member> getMembersTeamFromDB(String teamUuid) {
        try {
            List<Member> filteredMembers    = new ArrayList<Member>();
            List<Member> allMembers = getMembersFromDB();
            Log.d(LOG_TAG, " getMembersTeamFromDB()  : " + teamUuid + "  size: " + allMembers.size());
            for (Member m: allMembers) {
                if (m.getTeamUuid().equals(teamUuid)) {
                    Log.d(LOG_TAG, " Adding    : " + m + "  team: " + m.getTeamUuid());
                    filteredMembers.add(m);
                } else {
                    Log.d(LOG_TAG, " Not adding: " + m + "  team: " + m.getTeamUuid());
                }
            }
            return filteredMembers;

        } catch (StorageNoClubException e) {
            e.printStackTrace();
        }

//        Log.d(LOG_TAG, " getMembersTeamFromDB()  : " + filteredMembers.size());
        return null;
    }

    public List<Member> getMembersFromDB() throws StorageNoClubException {
        String[] subProjectionArray = {TEAM_COLUMN_NAME};
        List<Member> members = new ArrayList<Member>();

        if (LocalStorage.getInstance().getCurrentClub()==null) {
            throw new StorageNoClubException("Club not set or faulty");
        }

        Cursor cursor = getCursorFirst(MEMBER_TABLE, subProjectionArray);

        while (!cursor.isAfterLast()) {
            Member m = cursorToMember(cursor);
            Log.d(LOG_TAG," add member: " + m);
            members.add(m);
            cursor.moveToNext();
        }
        closeCursor(cursor);
        return members;
    }

    public List<Media> getMediaFromDB() throws StorageNoClubException {
        String[] subProjectionArray = {TEAM_COLUMN_NAME, URI_COLUMN_NAME, STATUS_COLUMN_NAME,
                TRAININGPHASE_COLUMN_NAME, MEMBER_COLUMN_NAME, DATE_COLUMN_NAME};


        List<Media> media = new ArrayList<Media>();
        if (LocalStorage.getInstance().getCurrentClub()==null) {
            throw new StorageNoClubException("Club not set or faulty");
        }
        Cursor cursor = getCursorFirst(MEDIA_TABLE, subProjectionArray);

        while (!cursor.isAfterLast()) {
            Media m = cursorToMedia(cursor);
            media.add(m);
          //  Log.d(LOG_TAG, " fetching media: " + m.getStatus() + " | " + m.getUuid() + " | " + m.fileName());
            cursor.moveToNext();
        }
        Log.d(LOG_TAG, " fetching media: ---- fini");

        closeCursor(cursor);
        return media;
    }

    public List<LogMessage> getLogMessagesFromDB(int limit) {
        Log.d(LOG_TAG, "---> Getting log messages");

        String[] projectionArray = {LOG_ID, CLUB_COLUMN_NAME, LOG_MSG, LOG_DETAIL, LOG_DATE};

        List<LogMessage> logmsgs = new ArrayList<>();

        String whereClause = CLUB_COLUMN_NAME + " = ? ";
        SQLiteDatabase db = getWritableDatabase();

        Log.d(LOG_TAG, "  get cursor, clubuuid: " + LocalStorage.getInstance().getCurrentClub());

        Cursor cursor = db.query(LOG_TABLE,
                projectionArray,
                whereClause,
                new String[]{LocalStorage.getInstance().getCurrentClub()},
                null,
                null,
                "date DESC",
                "" + limit);
        if (cursor == null) {
            return null;
        }
        cursor.moveToFirst();

        while (!cursor.isAfterLast()) {
            Log.d(LOG_TAG, "  keep readin' log");
            LogMessage lm = cursorToLogMessage(cursor);
            logmsgs.add(lm);
            cursor.moveToNext();
        }

        closeCursor(cursor);
        Log.d(LOG_TAG, "<-- Getting log messages");
        return logmsgs;
    }


    public void storeMembersToDB(List<Member> members) {
        Log.d(LOG_TAG, "Storing member " + members.size() );
        SQLiteDatabase db = getWritableDatabase();
        storeBaseToDB(db, (List<CoachAppBase>) (List<? extends CoachAppBase>) members, MEMBER_TABLE);

        for (Member m : members) {
            Log.d(LOG_TAG, "Storing member " + m );
            ContentValues values = buildContentValues(m);
            // Add team to member
            values.put(TEAM_COLUMN_NAME, m.getTeamUuid());

            long rowId = db.insert(MEMBER_TABLE, null, values);
            //  Log.d(LOG_TAG, " * " + rowId + " inserted " + m);
            if (rowId < 0) {
                Log.e(LOG_TAG, "ERROR inserting (" + rowId + "): " + m);
            }
        }
    }

    private Member cursorToMember(Cursor cursor) {
        if (cursor == null) {
            Log.d(LOG_TAG, "Cursor is null in member database");
            return null;
        }
//        Log.d(LOG_TAG, "Cursor in member database" + cursor + " count: " + cursor.getColumnCount() + "  name: " + cursor.getColumnName(0));
        Member m = new Member(cursor.getString(0),
                cursor.getString(1),
                cursor.getString(2),
                cursor.getString(3));
        return m;
    }


    private LogMessage cursorToLogMessage(Cursor cursor) {
        Log.d(LOG_TAG, "cursorToLogMessage()");
        if (cursor == null) {
            Log.d(LOG_TAG, "Cursor is null in Log database");
            return null;
        }

        for (String s: cursor.getColumnNames()) {
            Log.d(LOG_TAG, " column: " + s);
        }

        Log.d(LOG_TAG, " * " + cursor.getCount());
        Log.d(LOG_TAG, " * " + cursor.getInt(0));
        Log.d(LOG_TAG, " * " + cursor.getString(1));
        Log.d(LOG_TAG, " * " + cursor.getString(2));
        Log.d(LOG_TAG, " * " + cursor.getString(3));
        //        String[] projectionArray = {LOG_ID, CLUB_COLUMN_NAME, LOG_MSG, LOG_DATE};

        LogMessage lm = new LogMessage(
                cursor.getInt(0),
                cursor.getString(1),
                cursor.getString(2),
                cursor.getString(3),
                new Date(cursor.getLong(4)));
        return lm;
    }


    public boolean updateMediaState(Media m, int state) {
        int oldStatus = m.getStatus();
        ContentValues values = buildContentValues(m);
        values.put(STATUS_COLUMN_NAME, state);
        SQLiteDatabase db = getWritableDatabase();

        long rows = db.update(MEDIA_TABLE,
                values, "uuid = ? ",
                new String[]{m.getUuid()});

        Log.d(LOG_TAG, " * " + rows + " updated " + m + " from state " + oldStatus + " to new state: " + m.getStatus());

        return rows == 1;
    }


    private boolean checkIfMediaExists(String filename) {
        SQLiteDatabase db = getWritableDatabase();
        Log.d(LOG_TAG, "checkIfMediaExists(): " + filename);
        String Query = "SELECT * from " + MEDIA_TABLE + " where " + URI_COLUMN_NAME + " = '" + filename + "'";
        Cursor cursor = db.rawQuery(Query, null);
        Log.d(LOG_TAG, "SELECT => count: " + cursor.getCount());
        if(cursor.getCount() <= 0){
            cursor.close();
            return false;
        }
        cursor.close();
        return true;
    }

    public boolean updateMediaStateCreated(Media m, String uuid) {
        Log.d(LOG_TAG, "Updating media element: " + m + " " + uuid);
        ContentValues values = new ContentValues();
        values.put(STATUS_COLUMN_NAME, Media.MEDIA_STATUS_CREATED);
        values.put(UUID_COLUMN_NAME, uuid);
        SQLiteDatabase db = getWritableDatabase();
        long rows = -1;
        try {
            List<Media> mediaList = getMediaFromDB();
            for (Media med : mediaList) {
                Log.d(LOG_TAG, " * media (db, exists?): " + med.getUuid() + " " + med.fileName());
            }
        } catch ( StorageNoClubException e) {
            Log.d(LOG_TAG, "no club set, just printing media so don't give a f%%k....");
        }

        Log.d(LOG_TAG, " * " + m.fileName() + " uuid: " + m.getUuid() + " exists? " + checkIfMediaExists(uuid));

        try {
            rows = db.update(MEDIA_TABLE,
                    values, "  " + URI_COLUMN_NAME + " = ? ",
                    new String[]{m.fileName()});
            Log.d(LOG_TAG, " * Updating " + rows + " elements to: " + uuid + " (media)");
        } catch (SQLiteConstraintException e) {
            Log.d(LOG_TAG, " UH OH.... ");
            // TODO: FIX FIX FIX HESAHESA HESA HESA
            return false;
        }

/*
        Log.d(LOG_TAG, "Listing all Media after status update");
        mediaList  = getMediaFromDB();
        for (Media med: mediaList) {
            Log.d(LOG_TAG, " * media: " + m.getUuid() + " " + m.fileName());
        }
*/
        return rows == 1;
    }

    private Media cursorToMedia(Cursor cursor) {
        if (cursor == null) {
            Log.d(LOG_TAG, "Cursor is null in member database");
            return null;
        }

/*
UUID_COLUMN_NAME,
NAME_COLUMN_NAME,
CLUB_COLUMN_NAME,
TEAM_COLUMN_NAME
URI_COLUMN_NAME
STATUS_COLUMN_NAME
TRAININGPHASE_COLUMN_NAME
MEMBER_COLUMN_NAME
DATE_COLUMN_NAME
*/
        //  Log.d(LOG_TAG, "Media: ");
        String uuid = cursor.getString(0);
        String name = cursor.getString(1);
        String club = cursor.getString(2);

        String team = cursor.getString(3);
        String uri = cursor.getString(4);
        int status = cursor.getInt(5);
        String tp = cursor.getString(6);
        String member = cursor.getString(7);
        long date = cursor.getLong(8);
/*
        Log.d(LOG_TAG, " creating Media from db:  uri: " + uri);
        Log.d(LOG_TAG, "    * uuid:  " + uuid);
        Log.d(LOG_TAG, "    * name:  " + name);
        Log.d(LOG_TAG, "    * club:  " + club);
        Log.d(LOG_TAG, "    * team:  " + team);
        Log.d(LOG_TAG, "    * uri:   " + uri);
        Log.d(LOG_TAG, "    * status:" + status);
        Log.d(LOG_TAG, "    * tp:    " + tp);
        Log.d(LOG_TAG, "    * member:" + member);
        Log.d(LOG_TAG, "    * date:  " + date);
*/
        Media m = new Media(uuid, name, club,
                uri, status, date,
                team, tp, member);

        return m;
    }


    @Override
    public SQLiteDatabase getWritableDatabase() {
        if (isCreating && currentDB != null) {
            return currentDB;
        }
        return super.getWritableDatabase();
    }

    @Override
    public SQLiteDatabase getReadableDatabase() {
        if (isCreating && currentDB != null) {
            return currentDB;
        }
        return super.getReadableDatabase();
    }

    public boolean updateMediaReplaceDownloadedFile(Media m, String file) {
        Log.d(LOG_TAG, "Updating media element: " + m.getUuid() + "   new file: " + file);
        ContentValues values = new ContentValues();
        values.put(STATUS_COLUMN_NAME, Media.MEDIA_STATUS_DOWNLOADED);
        values.put(URI_COLUMN_NAME, file);
        SQLiteDatabase db = getWritableDatabase();

        long rows = db.update(MEDIA_TABLE,
                values, "  " + UUID_COLUMN_NAME + " = ? ",
                new String[]{m.getUuid()});

        Log.d(LOG_TAG, " * " + rows + " updated " + m + " to downloaded (" + file + ")");
/*
        try {
            for (Media media : Storage.getInstance().getMedia()) {
                Log.d(LOG_TAG, " media: " + media.fileName());
            }

        } catch (StorageNoClubException e) {
            Log.d(LOG_TAG, "Failed checking media");
        }
*/
        return rows == 1;
    }

    public boolean updateMediaFileName(Media m, String file) {
        Log.d(LOG_TAG, "Updating media element: " + m.getUuid() + "   new file: " + file);
        ContentValues values = new ContentValues();
        values.put(URI_COLUMN_NAME, file);
        SQLiteDatabase db = getWritableDatabase();

        long rows = db.update(MEDIA_TABLE,
                values, "  " + UUID_COLUMN_NAME + " = ? ",
                new String[]{m.getUuid()});

        Log.d(LOG_TAG, " * " + rows + " updated " + m + " to downloaded (" + file + ")");
        return rows == 1;
    }

    public void log(String msg, String detail) {
        Log.d(LOG_TAG, "LOG: " + msg);
        Log.d(LOG_TAG, "LOG: " + new Date().getTime());
        Log.d(LOG_TAG, "LOG: " + new Date(new Date().getTime()));
        SQLiteDatabase db = getWritableDatabase();
        ContentValues values = new ContentValues();
        values.put(CLUB_COLUMN_NAME, LocalStorage.getInstance().getCurrentClub());
        values.put(LOG_MSG,     msg);
        values.put(LOG_DETAIL,  detail);
        values.put(LOG_DATE,    (new Date().getTime()));

        long rowId = db.insert(LOG_TABLE, null, values);
        if (rowId < 1) {
            Log.e(LOG_TAG, "ERROR inserting (" + rowId + "): ");
        }
        Log.d(LOG_TAG, "LOG: " + msg + "  ==> " + rowId);
    }

    public void storeLocalUser(LocalUser lu) {
        SQLiteDatabase db = getWritableDatabase();
        ContentValues values = new ContentValues();

        int    id          = lu.getId();
        String name        = lu.getName();
        String email       = lu.getEmail();
        String password    = lu.getPassword();
        List<String> clubs = lu.getClubUuids();
        String latestClub  = lu.getLatestClubUuid();
        String token       = lu.getToken();


        Log.d(LOG_TAG, "Storing local user:");
        Log.d(LOG_TAG, " * " + id);
        Log.d(LOG_TAG, " * " + name);
        Log.d(LOG_TAG, " * " + email);
        Log.d(LOG_TAG, " * " + password);
        Log.d(LOG_TAG, " * " + clubs);
        Log.d(LOG_TAG, " * " + latestClub);
        Log.d(LOG_TAG, " * " + token);


//        values.put(LOCAL_USER_ID,          id);
        values.put(LOCAL_USER_NAME,        name);
        values.put(LOCAL_USER_EMAIL,       email);
        values.put(LOCAL_USER_PASSWORD,    password);
        values.put(LOCAL_USER_EMAIL,       Arrays.toString(clubs.toArray()));
        values.put(LOCAL_USER_LATEST_CLUB, latestClub);
        values.put(LOCAL_USER_TOKEN,       token);

        // TODO: Is this safe?
        db.replace(LOCAL_USER_TABLE, null, values);
    }

    private LocalUser cursorToLocalUser(Cursor cursor) {
        if (cursor == null) {
            Log.d(LOG_TAG, "Cursor is null in local user database");
            return null;
        }

        Log.d(LOG_TAG, "cursor: " + cursor);

        int    id          = cursor.getInt(0);
        String name        = cursor.getString(1);
        String email       = cursor.getString(2);
        String password    = cursor.getString(3);
        String clubsString = cursor.getString(4);
        String latestClub  = cursor.getString(5);
        String token       = cursor.getString(6);

        List<String> clubs = new ArrayList<String>();

        Log.d(LOG_TAG, "Cursor (LocalUser) id:    " + id);
        Log.d(LOG_TAG, "Cursor (LocalUser) name:  " + name);
        Log.d(LOG_TAG, "Cursor (LocalUser) email: " + email);
        Log.d(LOG_TAG, "Cursor (LocalUser) pwd:   " + password);
        Log.d(LOG_TAG, "Cursor (LocalUser) clubs: " + clubsString);
        Log.d(LOG_TAG, "Cursor (LocalUser) lc:    " + latestClub);
        Log.d(LOG_TAG, "Cursor (LocalUser) token: " + token);

        if (clubsString!=null) {
            for (String s : clubsString.split(",")) {
                Log.d(LOG_TAG, "  club: " + s);
                clubs.add(s);
            }
        }

        LocalUser lu = new LocalUser(id, name, email, password, clubs, latestClub, token);
        return lu;
    }



    public List<LocalUser> getLocalUserFromDB() {
        String[] projectionArray = {
                LOCAL_USER_ID,
                LOCAL_USER_NAME,
                LOCAL_USER_EMAIL,
                LOCAL_USER_PASSWORD,
                LOCAL_USER_CLUBS,
                LOCAL_USER_LATEST_CLUB,
                LOCAL_USER_TOKEN};
        List<LocalUser> users = new ArrayList<LocalUser>();
//        Cursor cursor = getCursorFirst(LOCAL_USER_TABLE, subProjectionArray);
        Log.d(LOG_TAG, "---> Getting local users");

        SQLiteDatabase db = getWritableDatabase();

        Cursor cursor = db.query(LOCAL_USER_TABLE,
                projectionArray,
                null, null, null, null,
                "id DESC");
        if (cursor == null) {
            return null;
        }
        cursor.moveToFirst();

        while (!cursor.isAfterLast()) {
            LocalUser lu = cursorToLocalUser(cursor);
            users.add(lu);
            cursor.moveToNext();
        }
        closeCursor(cursor);
        return users;
    }


}


