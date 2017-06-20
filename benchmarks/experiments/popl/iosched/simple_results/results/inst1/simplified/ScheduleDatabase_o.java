package com.google.samples.apps.iosched.provider;

import static com.google.samples.apps.iosched.util.LogUtils.*;

/**
 * Helper for managing {@link SQLiteDatabase} that stores data for
 * {@link ScheduleProvider}.
 */
public class ScheduleDatabase extends SQLiteOpenHelper {
    SQLiteDatabase db;

    @Override
    public int onCreate() {
        db.execSQL("CREATE TABLE");        
        db.execSQL("CREATE TABLE " + Tables.TAGS + " ("
                + BaseColumns._ID + " INTEGER PRIMARY KEY AUTOINCREMENT,"
                + TagsColumns.TAG_ID + " TEXT NOT NULL,"
                + TagsColumns.TAG_CATEGORY + " TEXT NOT NULL,"
                + TagsColumns.TAG_NAME + " TEXT NOT NULL,"
                + TagsColumns.TAG_ORDER_IN_CATEGORY + " INTEGER,"
                + TagsColumns.TAG_COLOR + " TEXT NOT NULL,"
                + TagsColumns.TAG_ABSTRACT + " TEXT NOT NULL,"
                + "UNIQUE (" + TagsColumns.TAG_ID + ") ON CONFLICT REPLACE)");

        db.execSQL("CREATE TABLE " + Tables.ROOMS + " ("
                + BaseColumns._ID + " INTEGER PRIMARY KEY AUTOINCREMENT,"
                + RoomsColumns.ROOM_ID + " TEXT NOT NULL,"
                + RoomsColumns.ROOM_NAME + " TEXT,"
                + RoomsColumns.ROOM_FLOOR + " TEXT,"
                + "UNIQUE (" + RoomsColumns.ROOM_ID + ") ON CONFLICT REPLACE)");

        db.execSQL("CREATE TABLE " + Tables.SESSIONS + " ("
                + BaseColumns._ID + " INTEGER PRIMARY KEY AUTOINCREMENT,"
                + SyncColumns.UPDATED + " INTEGER NOT NULL,"
                + SessionsColumns.SESSION_ID + " TEXT NOT NULL,"
                + Sessions.ROOM_ID + " TEXT " + References.ROOM_ID + ","
                + SessionsColumns.SESSION_START + " INTEGER NOT NULL,"
                + SessionsColumns.SESSION_END + " INTEGER NOT NULL,"
                + SessionsColumns.SESSION_LEVEL + " TEXT,"
                + SessionsColumns.SESSION_TITLE + " TEXT,"
                + SessionsColumns.SESSION_ABSTRACT + " TEXT,"
                + SessionsColumns.SESSION_REQUIREMENTS + " TEXT,"
                + SessionsColumns.SESSION_KEYWORDS + " TEXT,"
                + SessionsColumns.SESSION_HASHTAG + " TEXT,"
                + SessionsColumns.SESSION_URL + " TEXT,"
                + SessionsColumns.SESSION_YOUTUBE_URL + " TEXT,"
                + SessionsColumns.SESSION_MODERATOR_URL + " TEXT,"
                + SessionsColumns.SESSION_PDF_URL + " TEXT,"
                + SessionsColumns.SESSION_NOTES_URL + " TEXT,"
                + SessionsColumns.SESSION_CAL_EVENT_ID + " INTEGER,"
                + SessionsColumns.SESSION_LIVESTREAM_ID + " TEXT,"
                + SessionsColumns.SESSION_TAGS + " TEXT,"
                + SessionsColumns.SESSION_GROUPING_ORDER + " INTEGER,"
                + SessionsColumns.SESSION_SPEAKER_NAMES + " TEXT,"
                + SessionsColumns.SESSION_IMPORT_HASHCODE + " TEXT NOT NULL DEFAULT '',"
                + SessionsColumns.SESSION_MAIN_TAG + " TEXT,"
                + SessionsColumns.SESSION_COLOR + " INTEGER,"
                + SessionsColumns.SESSION_CAPTIONS_URL + " TEXT,"
                + SessionsColumns.SESSION_PHOTO_URL + " TEXT,"
                + SessionsColumns.SESSION_RELATED_CONTENT + " TEXT,"
                + "UNIQUE (" + SessionsColumns.SESSION_ID + ") ON CONFLICT REPLACE)");

        db.execSQL("CREATE TABLE " + Tables.SPEAKERS + " ("
                + BaseColumns._ID + " INTEGER PRIMARY KEY AUTOINCREMENT,"
                + SyncColumns.UPDATED + " INTEGER NOT NULL,"
                + SpeakersColumns.SPEAKER_ID + " TEXT NOT NULL,"
                + SpeakersColumns.SPEAKER_NAME + " TEXT,"
                + SpeakersColumns.SPEAKER_IMAGE_URL + " TEXT,"
                + SpeakersColumns.SPEAKER_COMPANY + " TEXT,"
                + SpeakersColumns.SPEAKER_ABSTRACT + " TEXT,"
                + SpeakersColumns.SPEAKER_URL + " TEXT,"
                + SpeakersColumns.SPEAKER_IMPORT_HASHCODE + " TEXT NOT NULL DEFAULT '',"
                + "UNIQUE (" + SpeakersColumns.SPEAKER_ID + ") ON CONFLICT REPLACE)");

        db.execSQL("CREATE TABLE " + Tables.MY_SCHEDULE + " ("
                + BaseColumns._ID + " INTEGER PRIMARY KEY AUTOINCREMENT,"
                + MySchedule.SESSION_ID + " TEXT NOT NULL " + References.SESSION_ID + ","
                + MySchedule.MY_SCHEDULE_ACCOUNT_NAME + " TEXT NOT NULL,"
                + MySchedule.MY_SCHEDULE_DIRTY_FLAG + " INTEGER NOT NULL DEFAULT 1,"
                + MySchedule.MY_SCHEDULE_IN_SCHEDULE + " INTEGER NOT NULL DEFAULT 1,"
                + "UNIQUE (" + MySchedule.SESSION_ID + ","
                + MySchedule.MY_SCHEDULE_ACCOUNT_NAME + ") ON CONFLICT REPLACE)");

        db.execSQL("CREATE TABLE " + Tables.SESSIONS_SPEAKERS + " ("
                + BaseColumns._ID + " INTEGER PRIMARY KEY AUTOINCREMENT,"
                + SessionsSpeakers.SESSION_ID + " TEXT NOT NULL " + References.SESSION_ID + ","
                + SessionsSpeakers.SPEAKER_ID + " TEXT NOT NULL " + References.SPEAKER_ID + ","
                + "UNIQUE (" + SessionsSpeakers.SESSION_ID + ","
                + SessionsSpeakers.SPEAKER_ID + ") ON CONFLICT REPLACE)");

        db.execSQL("CREATE TABLE " + Tables.SESSIONS_TAGS + " ("
                + BaseColumns._ID + " INTEGER PRIMARY KEY AUTOINCREMENT,"
                + SessionsTags.SESSION_ID + " TEXT NOT NULL " + References.SESSION_ID + ","
                + SessionsTags.TAG_ID + " TEXT NOT NULL " + References.TAG_ID + ","
                + "UNIQUE (" + SessionsTags.SESSION_ID + ","
                + SessionsTags.TAG_ID + ") ON CONFLICT REPLACE)");

        db.execSQL("CREATE TABLE " + Tables.ANNOUNCEMENTS + " ("
                + BaseColumns._ID + " INTEGER PRIMARY KEY AUTOINCREMENT,"
                + SyncColumns.UPDATED + " INTEGER NOT NULL,"
                + AnnouncementsColumns.ANNOUNCEMENT_ID + " TEXT,"
                + AnnouncementsColumns.ANNOUNCEMENT_TITLE + " TEXT NOT NULL,"
                + AnnouncementsColumns.ANNOUNCEMENT_ACTIVITY_JSON + " BLOB,"
                + AnnouncementsColumns.ANNOUNCEMENT_URL + " TEXT,"
                + AnnouncementsColumns.ANNOUNCEMENT_DATE + " INTEGER NOT NULL)");

        db.execSQL("CREATE TABLE " + Tables.MAPTILES + " ("
                + BaseColumns._ID + " INTEGER PRIMARY KEY AUTOINCREMENT,"
                + MapTileColumns.TILE_FLOOR + " INTEGER NOT NULL,"
                + MapTileColumns.TILE_FILE + " TEXT NOT NULL,"
                + MapTileColumns.TILE_URL + " TEXT NOT NULL,"
                + "UNIQUE (" + MapTileColumns.TILE_FLOOR + ") ON CONFLICT REPLACE)");

        db.execSQL("CREATE TABLE " + Tables.FEEDBACK + " ("
                + BaseColumns._ID + " INTEGER PRIMARY KEY AUTOINCREMENT,"
                + SyncColumns.UPDATED + " INTEGER NOT NULL,"
                + Sessions.SESSION_ID + " TEXT " + References.SESSION_ID + ","
                + FeedbackColumns.SESSION_RATING + " INTEGER NOT NULL,"
                + FeedbackColumns.ANSWER_RELEVANCE + " INTEGER NOT NULL,"
                + FeedbackColumns.ANSWER_CONTENT + " INTEGER NOT NULL,"
                + FeedbackColumns.ANSWER_SPEAKER + " INTEGER NOT NULL,"
                + FeedbackColumns.COMMENTS + " TEXT,"
                + FeedbackColumns.SYNCED + " INTEGER NOT NULL DEFAULT 0)");

        db.execSQL("CREATE TRIGGER " + Triggers.SESSIONS_FEEDBACK_DELETE + " AFTER DELETE ON "
                + Tables.SESSIONS + " BEGIN DELETE FROM " + Tables.FEEDBACK + " "
                + " WHERE " + Qualified.FEEDBACK_SESSION_ID + "=old." + Sessions.SESSION_ID
                + ";" + " END;");

        db.execSQL("CREATE TABLE " + Tables.MAPMARKERS + " ("
                + BaseColumns._ID + " INTEGER PRIMARY KEY AUTOINCREMENT,"
                + MapMarkerColumns.MARKER_ID + " TEXT NOT NULL,"
                + MapMarkerColumns.MARKER_TYPE + " TEXT NOT NULL,"
                + MapMarkerColumns.MARKER_LATITUDE + " DOUBLE NOT NULL,"
                + MapMarkerColumns.MARKER_LONGITUDE + " DOUBLE NOT NULL,"
                + MapMarkerColumns.MARKER_LABEL + " TEXT,"
                + MapMarkerColumns.MARKER_FLOOR + " INTEGER NOT NULL,"
                + "UNIQUE (" + MapMarkerColumns.MARKER_ID + ") ON CONFLICT REPLACE)");

        db.execSQL("CREATE TABLE " + Tables.HASHTAGS + " ("
                + BaseColumns._ID + " INTEGER PRIMARY KEY AUTOINCREMENT,"
                + HashtagColumns.HASHTAG_NAME + " TEXT NOT NULL,"
                + HashtagColumns.HASHTAG_DESCRIPTION + " TEXT NOT NULL,"
                + HashtagColumns.HASHTAG_COLOR + " INTEGER NOT NULL,"
                + HashtagColumns.HASHTAG_ORDER + " INTEGER NOT NULL,"
                + "UNIQUE (" + HashtagColumns.HASHTAG_NAME + ") ON CONFLICT REPLACE)");

        db.execSQL("CREATE TABLE " + Tables.VIDEOS + " ("
                + BaseColumns._ID + " INTEGER PRIMARY KEY AUTOINCREMENT,"
                + VideoColumns.VIDEO_ID + " TEXT NOT NULL,"
                + VideoColumns.VIDEO_YEAR + " INTEGER NOT NULL,"
                + VideoColumns.VIDEO_TITLE + " TEXT,"
                + VideoColumns.VIDEO_DESC + " TEXT,"
                + VideoColumns.VIDEO_VID + " TEXT,"
                + VideoColumns.VIDEO_TOPIC + " TEXT,"
                + VideoColumns.VIDEO_SPEAKERS + " TEXT,"
                + VideoColumns.VIDEO_THUMBNAIL_URL + " TEXT,"
                + VideoColumns.VIDEO_IMPORT_HASHCODE + " TEXT NOT NULL,"
                + "UNIQUE (" + VideoColumns.VIDEO_ID + ") ON CONFLICT REPLACE)");

        // Full-text search index. Update using updateSessionSearchIndex method.
        // Use the porter tokenizer for simple stemming, so that "frustration" matches "frustrated."
        db.execSQL("CREATE VIRTUAL TABLE " + Tables.SESSIONS_SEARCH + " USING fts3("
                + BaseColumns._ID + " INTEGER PRIMARY KEY AUTOINCREMENT,"
                + SessionsSearchColumns.BODY + " TEXT NOT NULL,"
                + SessionsSearchColumns.SESSION_ID
                + " TEXT NOT NULL " + References.SESSION_ID + ","
                + "UNIQUE (" + SessionsSearchColumns.SESSION_ID + ") ON CONFLICT REPLACE,"
                + "tokenize=porter)");

        // Search suggestions
        db.execSQL("CREATE TABLE " + Tables.SEARCH_SUGGEST + " ("
                + BaseColumns._ID + " INTEGER PRIMARY KEY AUTOINCREMENT,"
                + SearchManager.SUGGEST_COLUMN_TEXT_1 + " TEXT NOT NULL)");

        // Session deletion triggers
        db.execSQL("CREATE TRIGGER " + Triggers.SESSIONS_TAGS_DELETE + " AFTER DELETE ON "
                + Tables.SESSIONS + " BEGIN DELETE FROM " + Tables.SESSIONS_TAGS + " "
                + " WHERE " + Qualified.SESSIONS_TAGS_SESSION_ID + "=old." + Sessions.SESSION_ID
                + ";" + " END;");

        db.execSQL("CREATE TRIGGER " + Triggers.SESSIONS_SPEAKERS_DELETE + " AFTER DELETE ON "
                + Tables.SESSIONS + " BEGIN DELETE FROM " + Tables.SESSIONS_SPEAKERS + " "
                + " WHERE " + Qualified.SESSIONS_SPEAKERS_SESSION_ID + "=old." + Sessions.SESSION_ID
                + ";" + " END;");

        db.execSQL("CREATE TRIGGER " + Triggers.SESSIONS_MY_SCHEDULE_DELETE + " AFTER DELETE ON "
                + Tables.SESSIONS + " BEGIN DELETE FROM " + Tables.MY_SCHEDULE + " "
                + " WHERE " + Tables.MY_SCHEDULE + "." + MySchedule.SESSION_ID +
                "=old." + Sessions.SESSION_ID
                + ";" + " END;");

        db.upgradeFrom2014Cto2015A();
        db.upgradeFrom2015Ato2015B();
        return 0;
    }

    private void upgradeFrom2014Cto2015A(SQLiteDatabase db) {
        db.execSQL("CREATE TABLE " + Tables.MY_FEEDBACK_SUBMITTED + " ("
                + BaseColumns._ID + " INTEGER PRIMARY KEY AUTOINCREMENT,"
                + MyFeedbackSubmitted.SESSION_ID + " TEXT NOT NULL " + References.SESSION_ID + ","
                + MyFeedbackSubmitted.MY_FEEDBACK_SUBMITTED_ACCOUNT_NAME + " TEXT NOT NULL,"
                + MyFeedbackSubmitted.MY_FEEDBACK_SUBMITTED_DIRTY_FLAG + " INTEGER NOT NULL DEFAULT 1,"
                + "UNIQUE (" + MyFeedbackSubmitted.SESSION_ID + ","
                + MyFeedbackSubmitted.MY_FEEDBACK_SUBMITTED_ACCOUNT_NAME + ") ON CONFLICT REPLACE)");

        db.execSQL("CREATE TABLE " + Tables.MY_VIEWED_VIDEO + " ("
                + BaseColumns._ID + " INTEGER PRIMARY KEY AUTOINCREMENT,"
                + MyViewedVideos.VIDEO_ID + " TEXT NOT NULL " + References.VIDEO_ID + ","
                + MyViewedVideos.MY_VIEWED_VIDEOS_ACCOUNT_NAME + " TEXT NOT NULL,"
                + MyViewedVideos.MY_VIEWED_VIDEOS_DIRTY_FLAG + " INTEGER NOT NULL DEFAULT 1,"
                + "UNIQUE (" + MyViewedVideos.VIDEO_ID + ","
                + MyViewedVideos.MY_VIEWED_VIDEOS_ACCOUNT_NAME + ") ON CONFLICT REPLACE)");
    }

    private void upgradeFrom2015Ato2015B(SQLiteDatabase db) {
        // Note: SpeakersColumns.SPEAKER_URL is unused in 2015. The columns added here are used
        // instead.
        db.execSQL("ALTER TABLE " + Tables.SPEAKERS
                + " ADD COLUMN " + SpeakersColumns.SPEAKER_PLUSONE_URL + " TEXT");
        db.execSQL("ALTER TABLE " + Tables.SPEAKERS
                + " ADD COLUMN " + SpeakersColumns.SPEAKER_TWITTER_URL + " TEXT");
    }
}
