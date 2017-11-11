private static final int CUR_DATABASE_VERSION = VER_2015_RELEASE_B;
private static final String DATABASE_NAME = "schedule.db";
private static final String TAG = makeLogTag(ScheduleDatabase.class);
private static final int VER_2014_RELEASE_A = 122;
private static final int VER_2014_RELEASE_C = 207;
private static final int VER_2015_RELEASE_A = 208;
private static final int VER_2015_RELEASE_B = 210;
private final Context mContext;
private static final int CUR_DATABASE_VERSION = VER_2016_RELEASE_A;
private static final int VER_2016_RELEASE_A = 211;
@Override
 public void onUpgrade (SQLiteDatabase db, int oldVersion, int newVersion)
{
  LOGD(TAG, ("onUpgrade() from " + oldVersion + " to " + newVersion));
  Account account = AccountUtils.getActiveAccount(mContext);
  if (account != null)
  {
    LOGI(TAG, "Cancelling any pending syncs for account");
    ContentResolver.cancelSync(account, ScheduleContract.CONTENT_AUTHORITY);
  }
  else
    ;
  int version = oldVersion;
  boolean dataInvalidated = true;
  if (version == VER_2014_RELEASE_C)
  {
    LOGD(TAG, "Upgrading database from 2014 release C to 2015 release A.");
    upgradeFrom2014Cto2015A(db);
    version = VER_2015_RELEASE_A;
  }
  else
    ;
  if (version == VER_2015_RELEASE_A)
  {
    LOGD(TAG, "Upgrading database from 2015 release A to 2015 release B.");
    upgradeFrom2015Ato2015B(db);
    version = VER_2015_RELEASE_B;
  }
  else
    ;
  if (version == VER_2015_RELEASE_B)
  {
    LOGD(TAG, "Upgrading database from 2015 release B to 2016 release A.");
    upgradeFrom2015Bto2016A(db);
    version = VER_2016_RELEASE_A;
  }
  else
    ;
  LOGD(TAG, ("After upgrade logic, at version " + version));
  {
    int wiz_i = 0;
    Tables.DeprecatedTables deprecatedTable = Tables.DeprecatedTables.values().get(wiz_i);
    while (wiz_i < Tables.DeprecatedTables.values().length())
    {
      {
        db.execSQL(("DROP TABLE IF EXISTS " + deprecatedTable.tableName));
      }
      wiz_i + 1;
    }
  }
  if (version != CUR_DATABASE_VERSION)
  {
    LOGW(TAG, "Upgrade unsuccessful -- destroying old data during upgrade");
    db.execSQL(("DROP TRIGGER IF EXISTS " + Triggers.SESSIONS_TAGS_DELETE));
    db.execSQL(("DROP TRIGGER IF EXISTS " + Triggers.SESSIONS_SPEAKERS_DELETE));
    db.execSQL(("DROP TRIGGER IF EXISTS " + Triggers.SESSIONS_FEEDBACK_DELETE));
    db.execSQL(("DROP TRIGGER IF EXISTS " + Triggers.SESSIONS_MY_SCHEDULE_DELETE));
    db.execSQL(("DROP TRIGGER IF EXISTS " + Triggers.DeprecatedTriggers.SESSIONS_TRACKS_DELETE));
    db.execSQL(("DROP TABLE IF EXISTS " + Tables.BLOCKS));
    db.execSQL(("DROP TABLE IF EXISTS " + Tables.ROOMS));
    db.execSQL(("DROP TABLE IF EXISTS " + Tables.TAGS));
    db.execSQL(("DROP TABLE IF EXISTS " + Tables.SESSIONS));
    db.execSQL(("DROP TABLE IF EXISTS " + Tables.SPEAKERS));
    db.execSQL(("DROP TABLE IF EXISTS " + Tables.MY_SCHEDULE));
    db.execSQL(("DROP TABLE IF EXISTS " + Tables.MY_FEEDBACK_SUBMITTED));
    db.execSQL(("DROP TABLE IF EXISTS " + Tables.MY_VIEWED_VIDEO));
    db.execSQL(("DROP TABLE IF EXISTS " + Tables.SESSIONS_SPEAKERS));
    db.execSQL(("DROP TABLE IF EXISTS " + Tables.SESSIONS_TAGS));
    db.execSQL(("DROP TABLE IF EXISTS " + Tables.ANNOUNCEMENTS));
    db.execSQL(("DROP TABLE IF EXISTS " + Tables.FEEDBACK));
    db.execSQL(("DROP TABLE IF EXISTS " + Tables.SESSIONS_SEARCH));
    db.execSQL(("DROP TABLE IF EXISTS " + Tables.SEARCH_SUGGEST));
    db.execSQL(("DROP TABLE IF EXISTS " + Tables.MAPMARKERS));
    db.execSQL(("DROP TABLE IF EXISTS " + Tables.MAPTILES));
    db.execSQL(("DROP TABLE IF EXISTS " + Tables.HASHTAGS));
    db.execSQL(("DROP TABLE IF EXISTS " + Tables.VIDEOS));
    onCreate(db);
    version = CUR_DATABASE_VERSION;
  }
  else
    ;
  if (dataInvalidated)
  {
    LOGD(TAG, "Data invalidated; resetting our data timestamp.");
    ConferenceDataHandler.resetDataTimestamp(mContext);
    if (account != null)
    {
      LOGI(TAG, "DB upgrade complete. Requesting resync.");
      SyncHelper.requestManualSync(account);
    }
    else
      ;
  }
  else
    ;
  return;
}
private void upgradeFrom2015Bto2016A (SQLiteDatabase db)
{
  db.execSQL(("ALTER TABLE " + Tables.MY_SCHEDULE + " ADD COLUMN " + MyScheduleColumns.MY_SCHEDULE_TIMESTAMP + " DATETIME"));
}
private void upgradeFrom2015Bto2016A (SQLiteDatabase db)
{
  db.execSQL(("ALTER TABLE " + Tables.TAGS + " ADD COLUMN " + TagsColumns.TAG_PHOTO_URL + " TEXT"));
  db.execSQL(("ALTER TABLE " + Tables.MY_SCHEDULE + " ADD COLUMN " + MyScheduleColumns.MY_SCHEDULE_TIMESTAMP + " DATETIME"));
}
private void upgradeFrom2015Bto2016A (SQLiteDatabase db)
{
  db.execSQL(("ALTER TABLE " + Tables.TAGS + " ADD COLUMN " + TagsColumns.TAG_PHOTO_URL + " TEXT"));
}
@Override
 public void onCreate (SQLiteDatabase db)
{
  db.execSQL(("CREATE TABLE " + Tables.BLOCKS + " (" + BaseColumns._ID + " INTEGER PRIMARY KEY AUTOINCREMENT," + BlocksColumns.BLOCK_ID + " TEXT NOT NULL," + BlocksColumns.BLOCK_TITLE + " TEXT NOT NULL," + BlocksColumns.BLOCK_START + " INTEGER NOT NULL," + BlocksColumns.BLOCK_END + " INTEGER NOT NULL," + BlocksColumns.BLOCK_TYPE + " TEXT," + BlocksColumns.BLOCK_SUBTITLE + " TEXT," + "UNIQUE (" + BlocksColumns.BLOCK_ID + ") ON CONFLICT REPLACE)"));
  db.execSQL(("CREATE TABLE " + Tables.TAGS + " (" + BaseColumns._ID + " INTEGER PRIMARY KEY AUTOINCREMENT," + TagsColumns.TAG_ID + " TEXT NOT NULL," + TagsColumns.TAG_CATEGORY + " TEXT NOT NULL," + TagsColumns.TAG_NAME + " TEXT NOT NULL," + TagsColumns.TAG_ORDER_IN_CATEGORY + " INTEGER," + TagsColumns.TAG_COLOR + " TEXT NOT NULL," + TagsColumns.TAG_ABSTRACT + " TEXT NOT NULL," + "UNIQUE (" + TagsColumns.TAG_ID + ") ON CONFLICT REPLACE)"));
  db.execSQL(("CREATE TABLE " + Tables.ROOMS + " (" + BaseColumns._ID + " INTEGER PRIMARY KEY AUTOINCREMENT," + RoomsColumns.ROOM_ID + " TEXT NOT NULL," + RoomsColumns.ROOM_NAME + " TEXT," + RoomsColumns.ROOM_FLOOR + " TEXT," + "UNIQUE (" + RoomsColumns.ROOM_ID + ") ON CONFLICT REPLACE)"));
  db.execSQL(("CREATE TABLE " + Tables.SESSIONS + " (" + BaseColumns._ID + " INTEGER PRIMARY KEY AUTOINCREMENT," + SyncColumns.UPDATED + " INTEGER NOT NULL," + SessionsColumns.SESSION_ID + " TEXT NOT NULL," + Sessions.ROOM_ID + " TEXT " + References.ROOM_ID + "," + SessionsColumns.SESSION_START + " INTEGER NOT NULL," + SessionsColumns.SESSION_END + " INTEGER NOT NULL," + SessionsColumns.SESSION_LEVEL + " TEXT," + SessionsColumns.SESSION_TITLE + " TEXT," + SessionsColumns.SESSION_ABSTRACT + " TEXT," + SessionsColumns.SESSION_REQUIREMENTS + " TEXT," + SessionsColumns.SESSION_KEYWORDS + " TEXT," + SessionsColumns.SESSION_HASHTAG + " TEXT," + SessionsColumns.SESSION_URL + " TEXT," + SessionsColumns.SESSION_YOUTUBE_URL + " TEXT," + SessionsColumns.SESSION_MODERATOR_URL + " TEXT," + SessionsColumns.SESSION_PDF_URL + " TEXT," + SessionsColumns.SESSION_NOTES_URL + " TEXT," + SessionsColumns.SESSION_CAL_EVENT_ID + " INTEGER," + SessionsColumns.SESSION_LIVESTREAM_ID + " TEXT," + SessionsColumns.SESSION_TAGS + " TEXT," + SessionsColumns.SESSION_GROUPING_ORDER + " INTEGER," + SessionsColumns.SESSION_SPEAKER_NAMES + " TEXT," + SessionsColumns.SESSION_IMPORT_HASHCODE + " TEXT NOT NULL DEFAULT ''," + SessionsColumns.SESSION_MAIN_TAG + " TEXT," + SessionsColumns.SESSION_COLOR + " INTEGER," + SessionsColumns.SESSION_CAPTIONS_URL + " TEXT," + SessionsColumns.SESSION_PHOTO_URL + " TEXT," + SessionsColumns.SESSION_RELATED_CONTENT + " TEXT," + "UNIQUE (" + SessionsColumns.SESSION_ID + ") ON CONFLICT REPLACE)"));
  db.execSQL(("CREATE TABLE " + Tables.SPEAKERS + " (" + BaseColumns._ID + " INTEGER PRIMARY KEY AUTOINCREMENT," + SyncColumns.UPDATED + " INTEGER NOT NULL," + SpeakersColumns.SPEAKER_ID + " TEXT NOT NULL," + SpeakersColumns.SPEAKER_NAME + " TEXT," + SpeakersColumns.SPEAKER_IMAGE_URL + " TEXT," + SpeakersColumns.SPEAKER_COMPANY + " TEXT," + SpeakersColumns.SPEAKER_ABSTRACT + " TEXT," + SpeakersColumns.SPEAKER_URL + " TEXT," + SpeakersColumns.SPEAKER_IMPORT_HASHCODE + " TEXT NOT NULL DEFAULT ''," + "UNIQUE (" + SpeakersColumns.SPEAKER_ID + ") ON CONFLICT REPLACE)"));
  db.execSQL(("CREATE TABLE " + Tables.MY_SCHEDULE + " (" + BaseColumns._ID + " INTEGER PRIMARY KEY AUTOINCREMENT," + MySchedule.SESSION_ID + " TEXT NOT NULL " + References.SESSION_ID + "," + MySchedule.MY_SCHEDULE_ACCOUNT_NAME + " TEXT NOT NULL," + MySchedule.MY_SCHEDULE_DIRTY_FLAG + " INTEGER NOT NULL DEFAULT 1," + MySchedule.MY_SCHEDULE_IN_SCHEDULE + " INTEGER NOT NULL DEFAULT 1," + "UNIQUE (" + MySchedule.SESSION_ID + "," + MySchedule.MY_SCHEDULE_ACCOUNT_NAME + ") ON CONFLICT REPLACE)"));
  db.execSQL(("CREATE TABLE " + Tables.SESSIONS_SPEAKERS + " (" + BaseColumns._ID + " INTEGER PRIMARY KEY AUTOINCREMENT," + SessionsSpeakers.SESSION_ID + " TEXT NOT NULL " + References.SESSION_ID + "," + SessionsSpeakers.SPEAKER_ID + " TEXT NOT NULL " + References.SPEAKER_ID + "," + "UNIQUE (" + SessionsSpeakers.SESSION_ID + "," + SessionsSpeakers.SPEAKER_ID + ") ON CONFLICT REPLACE)"));
  db.execSQL(("CREATE TABLE " + Tables.SESSIONS_TAGS + " (" + BaseColumns._ID + " INTEGER PRIMARY KEY AUTOINCREMENT," + SessionsTags.SESSION_ID + " TEXT NOT NULL " + References.SESSION_ID + "," + SessionsTags.TAG_ID + " TEXT NOT NULL " + References.TAG_ID + "," + "UNIQUE (" + SessionsTags.SESSION_ID + "," + SessionsTags.TAG_ID + ") ON CONFLICT REPLACE)"));
  db.execSQL(("CREATE TABLE " + Tables.ANNOUNCEMENTS + " (" + BaseColumns._ID + " INTEGER PRIMARY KEY AUTOINCREMENT," + SyncColumns.UPDATED + " INTEGER NOT NULL," + AnnouncementsColumns.ANNOUNCEMENT_ID + " TEXT," + AnnouncementsColumns.ANNOUNCEMENT_TITLE + " TEXT NOT NULL," + AnnouncementsColumns.ANNOUNCEMENT_ACTIVITY_JSON + " BLOB," + AnnouncementsColumns.ANNOUNCEMENT_URL + " TEXT," + AnnouncementsColumns.ANNOUNCEMENT_DATE + " INTEGER NOT NULL)"));
  db.execSQL(("CREATE TABLE " + Tables.MAPTILES + " (" + BaseColumns._ID + " INTEGER PRIMARY KEY AUTOINCREMENT," + MapTileColumns.TILE_FLOOR + " INTEGER NOT NULL," + MapTileColumns.TILE_FILE + " TEXT NOT NULL," + MapTileColumns.TILE_URL + " TEXT NOT NULL," + "UNIQUE (" + MapTileColumns.TILE_FLOOR + ") ON CONFLICT REPLACE)"));
  db.execSQL(("CREATE TABLE " + Tables.FEEDBACK + " (" + BaseColumns._ID + " INTEGER PRIMARY KEY AUTOINCREMENT," + SyncColumns.UPDATED + " INTEGER NOT NULL," + Sessions.SESSION_ID + " TEXT " + References.SESSION_ID + "," + FeedbackColumns.SESSION_RATING + " INTEGER NOT NULL," + FeedbackColumns.ANSWER_RELEVANCE + " INTEGER NOT NULL," + FeedbackColumns.ANSWER_CONTENT + " INTEGER NOT NULL," + FeedbackColumns.ANSWER_SPEAKER + " INTEGER NOT NULL," + FeedbackColumns.COMMENTS + " TEXT," + FeedbackColumns.SYNCED + " INTEGER NOT NULL DEFAULT 0)"));
  db.execSQL(("CREATE TRIGGER " + Triggers.SESSIONS_FEEDBACK_DELETE + " AFTER DELETE ON " + Tables.SESSIONS + " BEGIN DELETE FROM " + Tables.FEEDBACK + " " + " WHERE " + Qualified.FEEDBACK_SESSION_ID + "=old." + Sessions.SESSION_ID + ";" + " END;"));
  db.execSQL(("CREATE TABLE " + Tables.MAPMARKERS + " (" + BaseColumns._ID + " INTEGER PRIMARY KEY AUTOINCREMENT," + MapMarkerColumns.MARKER_ID + " TEXT NOT NULL," + MapMarkerColumns.MARKER_TYPE + " TEXT NOT NULL," + MapMarkerColumns.MARKER_LATITUDE + " DOUBLE NOT NULL," + MapMarkerColumns.MARKER_LONGITUDE + " DOUBLE NOT NULL," + MapMarkerColumns.MARKER_LABEL + " TEXT," + MapMarkerColumns.MARKER_FLOOR + " INTEGER NOT NULL," + "UNIQUE (" + MapMarkerColumns.MARKER_ID + ") ON CONFLICT REPLACE)"));
  db.execSQL(("CREATE TABLE " + Tables.HASHTAGS + " (" + BaseColumns._ID + " INTEGER PRIMARY KEY AUTOINCREMENT," + HashtagColumns.HASHTAG_NAME + " TEXT NOT NULL," + HashtagColumns.HASHTAG_DESCRIPTION + " TEXT NOT NULL," + HashtagColumns.HASHTAG_COLOR + " INTEGER NOT NULL," + HashtagColumns.HASHTAG_ORDER + " INTEGER NOT NULL," + "UNIQUE (" + HashtagColumns.HASHTAG_NAME + ") ON CONFLICT REPLACE)"));
  db.execSQL(("CREATE TABLE " + Tables.VIDEOS + " (" + BaseColumns._ID + " INTEGER PRIMARY KEY AUTOINCREMENT," + VideoColumns.VIDEO_ID + " TEXT NOT NULL," + VideoColumns.VIDEO_YEAR + " INTEGER NOT NULL," + VideoColumns.VIDEO_TITLE + " TEXT," + VideoColumns.VIDEO_DESC + " TEXT," + VideoColumns.VIDEO_VID + " TEXT," + VideoColumns.VIDEO_TOPIC + " TEXT," + VideoColumns.VIDEO_SPEAKERS + " TEXT," + VideoColumns.VIDEO_THUMBNAIL_URL + " TEXT," + VideoColumns.VIDEO_IMPORT_HASHCODE + " TEXT NOT NULL," + "UNIQUE (" + VideoColumns.VIDEO_ID + ") ON CONFLICT REPLACE)"));
  db.execSQL(("CREATE VIRTUAL TABLE " + Tables.SESSIONS_SEARCH + " USING fts3(" + BaseColumns._ID + " INTEGER PRIMARY KEY AUTOINCREMENT," + SessionsSearchColumns.BODY + " TEXT NOT NULL," + SessionsSearchColumns.SESSION_ID + " TEXT NOT NULL " + References.SESSION_ID + "," + "UNIQUE (" + SessionsSearchColumns.SESSION_ID + ") ON CONFLICT REPLACE," + "tokenize=porter)"));
  db.execSQL(("CREATE TABLE " + Tables.SEARCH_SUGGEST + " (" + BaseColumns._ID + " INTEGER PRIMARY KEY AUTOINCREMENT," + SearchManager.SUGGEST_COLUMN_TEXT_1 + " TEXT NOT NULL)"));
  db.execSQL(("CREATE TRIGGER " + Triggers.SESSIONS_TAGS_DELETE + " AFTER DELETE ON " + Tables.SESSIONS + " BEGIN DELETE FROM " + Tables.SESSIONS_TAGS + " " + " WHERE " + Qualified.SESSIONS_TAGS_SESSION_ID + "=old." + Sessions.SESSION_ID + ";" + " END;"));
  db.execSQL(("CREATE TRIGGER " + Triggers.SESSIONS_SPEAKERS_DELETE + " AFTER DELETE ON " + Tables.SESSIONS + " BEGIN DELETE FROM " + Tables.SESSIONS_SPEAKERS + " " + " WHERE " + Qualified.SESSIONS_SPEAKERS_SESSION_ID + "=old." + Sessions.SESSION_ID + ";" + " END;"));
  db.execSQL(("CREATE TRIGGER " + Triggers.SESSIONS_MY_SCHEDULE_DELETE + " AFTER DELETE ON " + Tables.SESSIONS + " BEGIN DELETE FROM " + Tables.MY_SCHEDULE + " " + " WHERE " + Tables.MY_SCHEDULE + "." + MySchedule.SESSION_ID + "=old." + Sessions.SESSION_ID + ";" + " END;"));
  upgradeFrom2014Cto2015A(db);
  upgradeFrom2015Ato2015B(db);
}
@Override
 public void onCreate (SQLiteDatabase db)
{
  db.execSQL(("CREATE TABLE " + Tables.BLOCKS + " (" + BaseColumns._ID + " INTEGER PRIMARY KEY AUTOINCREMENT," + BlocksColumns.BLOCK_ID + " TEXT NOT NULL," + BlocksColumns.BLOCK_TITLE + " TEXT NOT NULL," + BlocksColumns.BLOCK_START + " INTEGER NOT NULL," + BlocksColumns.BLOCK_END + " INTEGER NOT NULL," + BlocksColumns.BLOCK_TYPE + " TEXT," + BlocksColumns.BLOCK_SUBTITLE + " TEXT," + "UNIQUE (" + BlocksColumns.BLOCK_ID + ") ON CONFLICT REPLACE)"));
  db.execSQL(("CREATE TABLE " + Tables.TAGS + " (" + BaseColumns._ID + " INTEGER PRIMARY KEY AUTOINCREMENT," + TagsColumns.TAG_ID + " TEXT NOT NULL," + TagsColumns.TAG_CATEGORY + " TEXT NOT NULL," + TagsColumns.TAG_NAME + " TEXT NOT NULL," + TagsColumns.TAG_ORDER_IN_CATEGORY + " INTEGER," + TagsColumns.TAG_COLOR + " TEXT NOT NULL," + TagsColumns.TAG_ABSTRACT + " TEXT NOT NULL," + TagsColumns.TAG_PHOTO_URL + " TEXT," + "UNIQUE (" + TagsColumns.TAG_ID + ") ON CONFLICT REPLACE)"));
  db.execSQL(("CREATE TABLE " + Tables.ROOMS + " (" + BaseColumns._ID + " INTEGER PRIMARY KEY AUTOINCREMENT," + RoomsColumns.ROOM_ID + " TEXT NOT NULL," + RoomsColumns.ROOM_NAME + " TEXT," + RoomsColumns.ROOM_FLOOR + " TEXT," + "UNIQUE (" + RoomsColumns.ROOM_ID + ") ON CONFLICT REPLACE)"));
  db.execSQL(("CREATE TABLE " + Tables.SESSIONS + " (" + BaseColumns._ID + " INTEGER PRIMARY KEY AUTOINCREMENT," + SyncColumns.UPDATED + " INTEGER NOT NULL," + SessionsColumns.SESSION_ID + " TEXT NOT NULL," + Sessions.ROOM_ID + " TEXT " + References.ROOM_ID + "," + SessionsColumns.SESSION_START + " INTEGER NOT NULL," + SessionsColumns.SESSION_END + " INTEGER NOT NULL," + SessionsColumns.SESSION_LEVEL + " TEXT," + SessionsColumns.SESSION_TITLE + " TEXT," + SessionsColumns.SESSION_ABSTRACT + " TEXT," + SessionsColumns.SESSION_REQUIREMENTS + " TEXT," + SessionsColumns.SESSION_KEYWORDS + " TEXT," + SessionsColumns.SESSION_HASHTAG + " TEXT," + SessionsColumns.SESSION_URL + " TEXT," + SessionsColumns.SESSION_YOUTUBE_URL + " TEXT," + SessionsColumns.SESSION_MODERATOR_URL + " TEXT," + SessionsColumns.SESSION_PDF_URL + " TEXT," + SessionsColumns.SESSION_NOTES_URL + " TEXT," + SessionsColumns.SESSION_CAL_EVENT_ID + " INTEGER," + SessionsColumns.SESSION_LIVESTREAM_ID + " TEXT," + SessionsColumns.SESSION_TAGS + " TEXT," + SessionsColumns.SESSION_GROUPING_ORDER + " INTEGER," + SessionsColumns.SESSION_SPEAKER_NAMES + " TEXT," + SessionsColumns.SESSION_IMPORT_HASHCODE + " TEXT NOT NULL DEFAULT ''," + SessionsColumns.SESSION_MAIN_TAG + " TEXT," + SessionsColumns.SESSION_COLOR + " INTEGER," + SessionsColumns.SESSION_CAPTIONS_URL + " TEXT," + SessionsColumns.SESSION_PHOTO_URL + " TEXT," + SessionsColumns.SESSION_RELATED_CONTENT + " TEXT," + "UNIQUE (" + SessionsColumns.SESSION_ID + ") ON CONFLICT REPLACE)"));
  db.execSQL(("CREATE TABLE " + Tables.SPEAKERS + " (" + BaseColumns._ID + " INTEGER PRIMARY KEY AUTOINCREMENT," + SyncColumns.UPDATED + " INTEGER NOT NULL," + SpeakersColumns.SPEAKER_ID + " TEXT NOT NULL," + SpeakersColumns.SPEAKER_NAME + " TEXT," + SpeakersColumns.SPEAKER_IMAGE_URL + " TEXT," + SpeakersColumns.SPEAKER_COMPANY + " TEXT," + SpeakersColumns.SPEAKER_ABSTRACT + " TEXT," + SpeakersColumns.SPEAKER_URL + " TEXT," + SpeakersColumns.SPEAKER_IMPORT_HASHCODE + " TEXT NOT NULL DEFAULT ''," + "UNIQUE (" + SpeakersColumns.SPEAKER_ID + ") ON CONFLICT REPLACE)"));
  db.execSQL(("CREATE TABLE " + Tables.MY_SCHEDULE + " (" + BaseColumns._ID + " INTEGER PRIMARY KEY AUTOINCREMENT," + MySchedule.SESSION_ID + " TEXT NOT NULL " + References.SESSION_ID + "," + MySchedule.MY_SCHEDULE_ACCOUNT_NAME + " TEXT NOT NULL," + MySchedule.MY_SCHEDULE_DIRTY_FLAG + " INTEGER NOT NULL DEFAULT 1," + MySchedule.MY_SCHEDULE_IN_SCHEDULE + " INTEGER NOT NULL DEFAULT 1," + "UNIQUE (" + MySchedule.SESSION_ID + "," + MySchedule.MY_SCHEDULE_ACCOUNT_NAME + ") ON CONFLICT REPLACE)"));
  db.execSQL(("CREATE TABLE " + Tables.SESSIONS_SPEAKERS + " (" + BaseColumns._ID + " INTEGER PRIMARY KEY AUTOINCREMENT," + SessionsSpeakers.SESSION_ID + " TEXT NOT NULL " + References.SESSION_ID + "," + SessionsSpeakers.SPEAKER_ID + " TEXT NOT NULL " + References.SPEAKER_ID + "," + "UNIQUE (" + SessionsSpeakers.SESSION_ID + "," + SessionsSpeakers.SPEAKER_ID + ") ON CONFLICT REPLACE)"));
  db.execSQL(("CREATE TABLE " + Tables.SESSIONS_TAGS + " (" + BaseColumns._ID + " INTEGER PRIMARY KEY AUTOINCREMENT," + SessionsTags.SESSION_ID + " TEXT NOT NULL " + References.SESSION_ID + "," + SessionsTags.TAG_ID + " TEXT NOT NULL " + References.TAG_ID + "," + "UNIQUE (" + SessionsTags.SESSION_ID + "," + SessionsTags.TAG_ID + ") ON CONFLICT REPLACE)"));
  db.execSQL(("CREATE TABLE " + Tables.ANNOUNCEMENTS + " (" + BaseColumns._ID + " INTEGER PRIMARY KEY AUTOINCREMENT," + SyncColumns.UPDATED + " INTEGER NOT NULL," + AnnouncementsColumns.ANNOUNCEMENT_ID + " TEXT," + AnnouncementsColumns.ANNOUNCEMENT_TITLE + " TEXT NOT NULL," + AnnouncementsColumns.ANNOUNCEMENT_ACTIVITY_JSON + " BLOB," + AnnouncementsColumns.ANNOUNCEMENT_URL + " TEXT," + AnnouncementsColumns.ANNOUNCEMENT_DATE + " INTEGER NOT NULL)"));
  db.execSQL(("CREATE TABLE " + Tables.MAPTILES + " (" + BaseColumns._ID + " INTEGER PRIMARY KEY AUTOINCREMENT," + MapTileColumns.TILE_FLOOR + " INTEGER NOT NULL," + MapTileColumns.TILE_FILE + " TEXT NOT NULL," + MapTileColumns.TILE_URL + " TEXT NOT NULL," + "UNIQUE (" + MapTileColumns.TILE_FLOOR + ") ON CONFLICT REPLACE)"));
  db.execSQL(("CREATE TABLE " + Tables.FEEDBACK + " (" + BaseColumns._ID + " INTEGER PRIMARY KEY AUTOINCREMENT," + SyncColumns.UPDATED + " INTEGER NOT NULL," + Sessions.SESSION_ID + " TEXT " + References.SESSION_ID + "," + FeedbackColumns.SESSION_RATING + " INTEGER NOT NULL," + FeedbackColumns.ANSWER_RELEVANCE + " INTEGER NOT NULL," + FeedbackColumns.ANSWER_CONTENT + " INTEGER NOT NULL," + FeedbackColumns.ANSWER_SPEAKER + " INTEGER NOT NULL," + FeedbackColumns.COMMENTS + " TEXT," + FeedbackColumns.SYNCED + " INTEGER NOT NULL DEFAULT 0)"));
  db.execSQL(("CREATE TRIGGER " + Triggers.SESSIONS_FEEDBACK_DELETE + " AFTER DELETE ON " + Tables.SESSIONS + " BEGIN DELETE FROM " + Tables.FEEDBACK + " " + " WHERE " + Qualified.FEEDBACK_SESSION_ID + "=old." + Sessions.SESSION_ID + ";" + " END;"));
  db.execSQL(("CREATE TABLE " + Tables.MAPMARKERS + " (" + BaseColumns._ID + " INTEGER PRIMARY KEY AUTOINCREMENT," + MapMarkerColumns.MARKER_ID + " TEXT NOT NULL," + MapMarkerColumns.MARKER_TYPE + " TEXT NOT NULL," + MapMarkerColumns.MARKER_LATITUDE + " DOUBLE NOT NULL," + MapMarkerColumns.MARKER_LONGITUDE + " DOUBLE NOT NULL," + MapMarkerColumns.MARKER_LABEL + " TEXT," + MapMarkerColumns.MARKER_FLOOR + " INTEGER NOT NULL," + "UNIQUE (" + MapMarkerColumns.MARKER_ID + ") ON CONFLICT REPLACE)"));
  db.execSQL(("CREATE TABLE " + Tables.HASHTAGS + " (" + BaseColumns._ID + " INTEGER PRIMARY KEY AUTOINCREMENT," + HashtagColumns.HASHTAG_NAME + " TEXT NOT NULL," + HashtagColumns.HASHTAG_DESCRIPTION + " TEXT NOT NULL," + HashtagColumns.HASHTAG_COLOR + " INTEGER NOT NULL," + HashtagColumns.HASHTAG_ORDER + " INTEGER NOT NULL," + "UNIQUE (" + HashtagColumns.HASHTAG_NAME + ") ON CONFLICT REPLACE)"));
  db.execSQL(("CREATE TABLE " + Tables.VIDEOS + " (" + BaseColumns._ID + " INTEGER PRIMARY KEY AUTOINCREMENT," + VideoColumns.VIDEO_ID + " TEXT NOT NULL," + VideoColumns.VIDEO_YEAR + " INTEGER NOT NULL," + VideoColumns.VIDEO_TITLE + " TEXT," + VideoColumns.VIDEO_DESC + " TEXT," + VideoColumns.VIDEO_VID + " TEXT," + VideoColumns.VIDEO_TOPIC + " TEXT," + VideoColumns.VIDEO_SPEAKERS + " TEXT," + VideoColumns.VIDEO_THUMBNAIL_URL + " TEXT," + VideoColumns.VIDEO_IMPORT_HASHCODE + " TEXT NOT NULL," + "UNIQUE (" + VideoColumns.VIDEO_ID + ") ON CONFLICT REPLACE)"));
  db.execSQL(("CREATE VIRTUAL TABLE " + Tables.SESSIONS_SEARCH + " USING fts3(" + BaseColumns._ID + " INTEGER PRIMARY KEY AUTOINCREMENT," + SessionsSearchColumns.BODY + " TEXT NOT NULL," + SessionsSearchColumns.SESSION_ID + " TEXT NOT NULL " + References.SESSION_ID + "," + "UNIQUE (" + SessionsSearchColumns.SESSION_ID + ") ON CONFLICT REPLACE," + "tokenize=porter)"));
  db.execSQL(("CREATE TABLE " + Tables.SEARCH_SUGGEST + " (" + BaseColumns._ID + " INTEGER PRIMARY KEY AUTOINCREMENT," + SearchManager.SUGGEST_COLUMN_TEXT_1 + " TEXT NOT NULL)"));
  db.execSQL(("CREATE TRIGGER " + Triggers.SESSIONS_TAGS_DELETE + " AFTER DELETE ON " + Tables.SESSIONS + " BEGIN DELETE FROM " + Tables.SESSIONS_TAGS + " " + " WHERE " + Qualified.SESSIONS_TAGS_SESSION_ID + "=old." + Sessions.SESSION_ID + ";" + " END;"));
  db.execSQL(("CREATE TRIGGER " + Triggers.SESSIONS_SPEAKERS_DELETE + " AFTER DELETE ON " + Tables.SESSIONS + " BEGIN DELETE FROM " + Tables.SESSIONS_SPEAKERS + " " + " WHERE " + Qualified.SESSIONS_SPEAKERS_SESSION_ID + "=old." + Sessions.SESSION_ID + ";" + " END;"));
  db.execSQL(("CREATE TRIGGER " + Triggers.SESSIONS_MY_SCHEDULE_DELETE + " AFTER DELETE ON " + Tables.SESSIONS + " BEGIN DELETE FROM " + Tables.MY_SCHEDULE + " " + " WHERE " + Tables.MY_SCHEDULE + "." + MySchedule.SESSION_ID + "=old." + Sessions.SESSION_ID + ";" + " END;"));
  upgradeFrom2014Cto2015A(db);
  upgradeFrom2015Ato2015B(db);
}
@Override
 public void onCreate (SQLiteDatabase db)
{
  db.execSQL(("CREATE TABLE " + Tables.BLOCKS + " (" + BaseColumns._ID + " INTEGER PRIMARY KEY AUTOINCREMENT," + BlocksColumns.BLOCK_ID + " TEXT NOT NULL," + BlocksColumns.BLOCK_TITLE + " TEXT NOT NULL," + BlocksColumns.BLOCK_START + " INTEGER NOT NULL," + BlocksColumns.BLOCK_END + " INTEGER NOT NULL," + BlocksColumns.BLOCK_TYPE + " TEXT," + BlocksColumns.BLOCK_SUBTITLE + " TEXT," + "UNIQUE (" + BlocksColumns.BLOCK_ID + ") ON CONFLICT REPLACE)"));
  db.execSQL(("CREATE TABLE " + Tables.TAGS + " (" + BaseColumns._ID + " INTEGER PRIMARY KEY AUTOINCREMENT," + TagsColumns.TAG_ID + " TEXT NOT NULL," + TagsColumns.TAG_CATEGORY + " TEXT NOT NULL," + TagsColumns.TAG_NAME + " TEXT NOT NULL," + TagsColumns.TAG_ORDER_IN_CATEGORY + " INTEGER," + TagsColumns.TAG_COLOR + " TEXT NOT NULL," + TagsColumns.TAG_ABSTRACT + " TEXT NOT NULL," + "UNIQUE (" + TagsColumns.TAG_ID + ") ON CONFLICT REPLACE)"));
  db.execSQL(("CREATE TABLE " + Tables.ROOMS + " (" + BaseColumns._ID + " INTEGER PRIMARY KEY AUTOINCREMENT," + RoomsColumns.ROOM_ID + " TEXT NOT NULL," + RoomsColumns.ROOM_NAME + " TEXT," + RoomsColumns.ROOM_FLOOR + " TEXT," + "UNIQUE (" + RoomsColumns.ROOM_ID + ") ON CONFLICT REPLACE)"));
  db.execSQL(("CREATE TABLE " + Tables.SESSIONS + " (" + BaseColumns._ID + " INTEGER PRIMARY KEY AUTOINCREMENT," + SyncColumns.UPDATED + " INTEGER NOT NULL," + SessionsColumns.SESSION_ID + " TEXT NOT NULL," + Sessions.ROOM_ID + " TEXT " + References.ROOM_ID + "," + SessionsColumns.SESSION_START + " INTEGER NOT NULL," + SessionsColumns.SESSION_END + " INTEGER NOT NULL," + SessionsColumns.SESSION_LEVEL + " TEXT," + SessionsColumns.SESSION_TITLE + " TEXT," + SessionsColumns.SESSION_ABSTRACT + " TEXT," + SessionsColumns.SESSION_REQUIREMENTS + " TEXT," + SessionsColumns.SESSION_KEYWORDS + " TEXT," + SessionsColumns.SESSION_HASHTAG + " TEXT," + SessionsColumns.SESSION_URL + " TEXT," + SessionsColumns.SESSION_YOUTUBE_URL + " TEXT," + SessionsColumns.SESSION_MODERATOR_URL + " TEXT," + SessionsColumns.SESSION_PDF_URL + " TEXT," + SessionsColumns.SESSION_NOTES_URL + " TEXT," + SessionsColumns.SESSION_CAL_EVENT_ID + " INTEGER," + SessionsColumns.SESSION_LIVESTREAM_ID + " TEXT," + SessionsColumns.SESSION_TAGS + " TEXT," + SessionsColumns.SESSION_GROUPING_ORDER + " INTEGER," + SessionsColumns.SESSION_SPEAKER_NAMES + " TEXT," + SessionsColumns.SESSION_IMPORT_HASHCODE + " TEXT NOT NULL DEFAULT ''," + SessionsColumns.SESSION_MAIN_TAG + " TEXT," + SessionsColumns.SESSION_COLOR + " INTEGER," + SessionsColumns.SESSION_CAPTIONS_URL + " TEXT," + SessionsColumns.SESSION_PHOTO_URL + " TEXT," + SessionsColumns.SESSION_RELATED_CONTENT + " TEXT," + "UNIQUE (" + SessionsColumns.SESSION_ID + ") ON CONFLICT REPLACE)"));
  db.execSQL(("CREATE TABLE " + Tables.SPEAKERS + " (" + BaseColumns._ID + " INTEGER PRIMARY KEY AUTOINCREMENT," + SyncColumns.UPDATED + " INTEGER NOT NULL," + SpeakersColumns.SPEAKER_ID + " TEXT NOT NULL," + SpeakersColumns.SPEAKER_NAME + " TEXT," + SpeakersColumns.SPEAKER_IMAGE_URL + " TEXT," + SpeakersColumns.SPEAKER_COMPANY + " TEXT," + SpeakersColumns.SPEAKER_ABSTRACT + " TEXT," + SpeakersColumns.SPEAKER_URL + " TEXT," + SpeakersColumns.SPEAKER_IMPORT_HASHCODE + " TEXT NOT NULL DEFAULT ''," + "UNIQUE (" + SpeakersColumns.SPEAKER_ID + ") ON CONFLICT REPLACE)"));
  db.execSQL(("CREATE TABLE " + Tables.MY_SCHEDULE + " (" + BaseColumns._ID + " INTEGER PRIMARY KEY AUTOINCREMENT," + MySchedule.SESSION_ID + " TEXT NOT NULL " + References.SESSION_ID + "," + MySchedule.MY_SCHEDULE_ACCOUNT_NAME + " TEXT NOT NULL," + MySchedule.MY_SCHEDULE_DIRTY_FLAG + " INTEGER NOT NULL DEFAULT 1," + MySchedule.MY_SCHEDULE_IN_SCHEDULE + " INTEGER NOT NULL DEFAULT 1," + "UNIQUE (" + MySchedule.SESSION_ID + "," + MySchedule.MY_SCHEDULE_ACCOUNT_NAME + ") ON CONFLICT REPLACE)"));
  db.execSQL(("CREATE TABLE " + Tables.SESSIONS_SPEAKERS + " (" + BaseColumns._ID + " INTEGER PRIMARY KEY AUTOINCREMENT," + SessionsSpeakers.SESSION_ID + " TEXT NOT NULL " + References.SESSION_ID + "," + SessionsSpeakers.SPEAKER_ID + " TEXT NOT NULL " + References.SPEAKER_ID + "," + "UNIQUE (" + SessionsSpeakers.SESSION_ID + "," + SessionsSpeakers.SPEAKER_ID + ") ON CONFLICT REPLACE)"));
  db.execSQL(("CREATE TABLE " + Tables.SESSIONS_TAGS + " (" + BaseColumns._ID + " INTEGER PRIMARY KEY AUTOINCREMENT," + SessionsTags.SESSION_ID + " TEXT NOT NULL " + References.SESSION_ID + "," + SessionsTags.TAG_ID + " TEXT NOT NULL " + References.TAG_ID + "," + "UNIQUE (" + SessionsTags.SESSION_ID + "," + SessionsTags.TAG_ID + ") ON CONFLICT REPLACE)"));
  db.execSQL(("CREATE TABLE " + Tables.ANNOUNCEMENTS + " (" + BaseColumns._ID + " INTEGER PRIMARY KEY AUTOINCREMENT," + SyncColumns.UPDATED + " INTEGER NOT NULL," + AnnouncementsColumns.ANNOUNCEMENT_ID + " TEXT," + AnnouncementsColumns.ANNOUNCEMENT_TITLE + " TEXT NOT NULL," + AnnouncementsColumns.ANNOUNCEMENT_ACTIVITY_JSON + " BLOB," + AnnouncementsColumns.ANNOUNCEMENT_URL + " TEXT," + AnnouncementsColumns.ANNOUNCEMENT_DATE + " INTEGER NOT NULL)"));
  db.execSQL(("CREATE TABLE " + Tables.MAPTILES + " (" + BaseColumns._ID + " INTEGER PRIMARY KEY AUTOINCREMENT," + MapTileColumns.TILE_FLOOR + " INTEGER NOT NULL," + MapTileColumns.TILE_FILE + " TEXT NOT NULL," + MapTileColumns.TILE_URL + " TEXT NOT NULL," + "UNIQUE (" + MapTileColumns.TILE_FLOOR + ") ON CONFLICT REPLACE)"));
  db.execSQL(("CREATE TABLE " + Tables.FEEDBACK + " (" + BaseColumns._ID + " INTEGER PRIMARY KEY AUTOINCREMENT," + SyncColumns.UPDATED + " INTEGER NOT NULL," + Sessions.SESSION_ID + " TEXT " + References.SESSION_ID + "," + FeedbackColumns.SESSION_RATING + " INTEGER NOT NULL," + FeedbackColumns.ANSWER_RELEVANCE + " INTEGER NOT NULL," + FeedbackColumns.ANSWER_CONTENT + " INTEGER NOT NULL," + FeedbackColumns.ANSWER_SPEAKER + " INTEGER NOT NULL," + FeedbackColumns.COMMENTS + " TEXT," + FeedbackColumns.SYNCED + " INTEGER NOT NULL DEFAULT 0)"));
  db.execSQL(("CREATE TRIGGER " + Triggers.SESSIONS_FEEDBACK_DELETE + " AFTER DELETE ON " + Tables.SESSIONS + " BEGIN DELETE FROM " + Tables.FEEDBACK + " " + " WHERE " + Qualified.FEEDBACK_SESSION_ID + "=old." + Sessions.SESSION_ID + ";" + " END;"));
  db.execSQL(("CREATE TABLE " + Tables.MAPMARKERS + " (" + BaseColumns._ID + " INTEGER PRIMARY KEY AUTOINCREMENT," + MapMarkerColumns.MARKER_ID + " TEXT NOT NULL," + MapMarkerColumns.MARKER_TYPE + " TEXT NOT NULL," + MapMarkerColumns.MARKER_LATITUDE + " DOUBLE NOT NULL," + MapMarkerColumns.MARKER_LONGITUDE + " DOUBLE NOT NULL," + MapMarkerColumns.MARKER_LABEL + " TEXT," + MapMarkerColumns.MARKER_FLOOR + " INTEGER NOT NULL," + "UNIQUE (" + MapMarkerColumns.MARKER_ID + ") ON CONFLICT REPLACE)"));
  db.execSQL(("CREATE TABLE " + Tables.HASHTAGS + " (" + BaseColumns._ID + " INTEGER PRIMARY KEY AUTOINCREMENT," + HashtagColumns.HASHTAG_NAME + " TEXT NOT NULL," + HashtagColumns.HASHTAG_DESCRIPTION + " TEXT NOT NULL," + HashtagColumns.HASHTAG_COLOR + " INTEGER NOT NULL," + HashtagColumns.HASHTAG_ORDER + " INTEGER NOT NULL," + "UNIQUE (" + HashtagColumns.HASHTAG_NAME + ") ON CONFLICT REPLACE)"));
  db.execSQL(("CREATE TABLE " + Tables.VIDEOS + " (" + BaseColumns._ID + " INTEGER PRIMARY KEY AUTOINCREMENT," + VideoColumns.VIDEO_ID + " TEXT NOT NULL," + VideoColumns.VIDEO_YEAR + " INTEGER NOT NULL," + VideoColumns.VIDEO_TITLE + " TEXT," + VideoColumns.VIDEO_DESC + " TEXT," + VideoColumns.VIDEO_VID + " TEXT," + VideoColumns.VIDEO_TOPIC + " TEXT," + VideoColumns.VIDEO_SPEAKERS + " TEXT," + VideoColumns.VIDEO_THUMBNAIL_URL + " TEXT," + VideoColumns.VIDEO_IMPORT_HASHCODE + " TEXT NOT NULL," + "UNIQUE (" + VideoColumns.VIDEO_ID + ") ON CONFLICT REPLACE)"));
  db.execSQL(("CREATE VIRTUAL TABLE " + Tables.SESSIONS_SEARCH + " USING fts3(" + BaseColumns._ID + " INTEGER PRIMARY KEY AUTOINCREMENT," + SessionsSearchColumns.BODY + " TEXT NOT NULL," + SessionsSearchColumns.SESSION_ID + " TEXT NOT NULL " + References.SESSION_ID + "," + "UNIQUE (" + SessionsSearchColumns.SESSION_ID + ") ON CONFLICT REPLACE," + "tokenize=porter)"));
  db.execSQL(("CREATE TABLE " + Tables.SEARCH_SUGGEST + " (" + BaseColumns._ID + " INTEGER PRIMARY KEY AUTOINCREMENT," + SearchManager.SUGGEST_COLUMN_TEXT_1 + " TEXT NOT NULL)"));
  db.execSQL(("CREATE TRIGGER " + Triggers.SESSIONS_TAGS_DELETE + " AFTER DELETE ON " + Tables.SESSIONS + " BEGIN DELETE FROM " + Tables.SESSIONS_TAGS + " " + " WHERE " + Qualified.SESSIONS_TAGS_SESSION_ID + "=old." + Sessions.SESSION_ID + ";" + " END;"));
  db.execSQL(("CREATE TRIGGER " + Triggers.SESSIONS_SPEAKERS_DELETE + " AFTER DELETE ON " + Tables.SESSIONS + " BEGIN DELETE FROM " + Tables.SESSIONS_SPEAKERS + " " + " WHERE " + Qualified.SESSIONS_SPEAKERS_SESSION_ID + "=old." + Sessions.SESSION_ID + ";" + " END;"));
  db.execSQL(("CREATE TRIGGER " + Triggers.SESSIONS_MY_SCHEDULE_DELETE + " AFTER DELETE ON " + Tables.SESSIONS + " BEGIN DELETE FROM " + Tables.MY_SCHEDULE + " " + " WHERE " + Tables.MY_SCHEDULE + "." + MySchedule.SESSION_ID + "=old." + Sessions.SESSION_ID + ";" + " END;"));
  upgradeFrom2014Cto2015A(db);
  upgradeFrom2015Ato2015B(db);
  upgradeFrom2015Bto2016A(db);
}
@Override
 public void onCreate (SQLiteDatabase db)
{
  db.execSQL(("CREATE TABLE " + Tables.BLOCKS + " (" + BaseColumns._ID + " INTEGER PRIMARY KEY AUTOINCREMENT," + BlocksColumns.BLOCK_ID + " TEXT NOT NULL," + BlocksColumns.BLOCK_TITLE + " TEXT NOT NULL," + BlocksColumns.BLOCK_START + " INTEGER NOT NULL," + BlocksColumns.BLOCK_END + " INTEGER NOT NULL," + BlocksColumns.BLOCK_TYPE + " TEXT," + BlocksColumns.BLOCK_SUBTITLE + " TEXT," + "UNIQUE (" + BlocksColumns.BLOCK_ID + ") ON CONFLICT REPLACE)"));
  db.execSQL(("CREATE TABLE " + Tables.TAGS + " (" + BaseColumns._ID + " INTEGER PRIMARY KEY AUTOINCREMENT," + TagsColumns.TAG_ID + " TEXT NOT NULL," + TagsColumns.TAG_CATEGORY + " TEXT NOT NULL," + TagsColumns.TAG_NAME + " TEXT NOT NULL," + TagsColumns.TAG_ORDER_IN_CATEGORY + " INTEGER," + TagsColumns.TAG_COLOR + " TEXT NOT NULL," + TagsColumns.TAG_ABSTRACT + " TEXT NOT NULL," + TagsColumns.TAG_PHOTO_URL + " TEXT," + "UNIQUE (" + TagsColumns.TAG_ID + ") ON CONFLICT REPLACE)"));
  db.execSQL(("CREATE TABLE " + Tables.ROOMS + " (" + BaseColumns._ID + " INTEGER PRIMARY KEY AUTOINCREMENT," + RoomsColumns.ROOM_ID + " TEXT NOT NULL," + RoomsColumns.ROOM_NAME + " TEXT," + RoomsColumns.ROOM_FLOOR + " TEXT," + "UNIQUE (" + RoomsColumns.ROOM_ID + ") ON CONFLICT REPLACE)"));
  db.execSQL(("CREATE TABLE " + Tables.SESSIONS + " (" + BaseColumns._ID + " INTEGER PRIMARY KEY AUTOINCREMENT," + SyncColumns.UPDATED + " INTEGER NOT NULL," + SessionsColumns.SESSION_ID + " TEXT NOT NULL," + Sessions.ROOM_ID + " TEXT " + References.ROOM_ID + "," + SessionsColumns.SESSION_START + " INTEGER NOT NULL," + SessionsColumns.SESSION_END + " INTEGER NOT NULL," + SessionsColumns.SESSION_LEVEL + " TEXT," + SessionsColumns.SESSION_TITLE + " TEXT," + SessionsColumns.SESSION_ABSTRACT + " TEXT," + SessionsColumns.SESSION_REQUIREMENTS + " TEXT," + SessionsColumns.SESSION_KEYWORDS + " TEXT," + SessionsColumns.SESSION_HASHTAG + " TEXT," + SessionsColumns.SESSION_URL + " TEXT," + SessionsColumns.SESSION_YOUTUBE_URL + " TEXT," + SessionsColumns.SESSION_MODERATOR_URL + " TEXT," + SessionsColumns.SESSION_PDF_URL + " TEXT," + SessionsColumns.SESSION_NOTES_URL + " TEXT," + SessionsColumns.SESSION_CAL_EVENT_ID + " INTEGER," + SessionsColumns.SESSION_LIVESTREAM_ID + " TEXT," + SessionsColumns.SESSION_TAGS + " TEXT," + SessionsColumns.SESSION_GROUPING_ORDER + " INTEGER," + SessionsColumns.SESSION_SPEAKER_NAMES + " TEXT," + SessionsColumns.SESSION_IMPORT_HASHCODE + " TEXT NOT NULL DEFAULT ''," + SessionsColumns.SESSION_MAIN_TAG + " TEXT," + SessionsColumns.SESSION_COLOR + " INTEGER," + SessionsColumns.SESSION_CAPTIONS_URL + " TEXT," + SessionsColumns.SESSION_PHOTO_URL + " TEXT," + SessionsColumns.SESSION_RELATED_CONTENT + " TEXT," + "UNIQUE (" + SessionsColumns.SESSION_ID + ") ON CONFLICT REPLACE)"));
  db.execSQL(("CREATE TABLE " + Tables.SPEAKERS + " (" + BaseColumns._ID + " INTEGER PRIMARY KEY AUTOINCREMENT," + SyncColumns.UPDATED + " INTEGER NOT NULL," + SpeakersColumns.SPEAKER_ID + " TEXT NOT NULL," + SpeakersColumns.SPEAKER_NAME + " TEXT," + SpeakersColumns.SPEAKER_IMAGE_URL + " TEXT," + SpeakersColumns.SPEAKER_COMPANY + " TEXT," + SpeakersColumns.SPEAKER_ABSTRACT + " TEXT," + SpeakersColumns.SPEAKER_URL + " TEXT," + SpeakersColumns.SPEAKER_IMPORT_HASHCODE + " TEXT NOT NULL DEFAULT ''," + "UNIQUE (" + SpeakersColumns.SPEAKER_ID + ") ON CONFLICT REPLACE)"));
  db.execSQL(("CREATE TABLE " + Tables.MY_SCHEDULE + " (" + BaseColumns._ID + " INTEGER PRIMARY KEY AUTOINCREMENT," + MySchedule.SESSION_ID + " TEXT NOT NULL " + References.SESSION_ID + "," + MySchedule.MY_SCHEDULE_ACCOUNT_NAME + " TEXT NOT NULL," + MySchedule.MY_SCHEDULE_DIRTY_FLAG + " INTEGER NOT NULL DEFAULT 1," + MySchedule.MY_SCHEDULE_IN_SCHEDULE + " INTEGER NOT NULL DEFAULT 1," + "UNIQUE (" + MySchedule.SESSION_ID + "," + MySchedule.MY_SCHEDULE_ACCOUNT_NAME + ") ON CONFLICT REPLACE)"));
  db.execSQL(("CREATE TABLE " + Tables.SESSIONS_SPEAKERS + " (" + BaseColumns._ID + " INTEGER PRIMARY KEY AUTOINCREMENT," + SessionsSpeakers.SESSION_ID + " TEXT NOT NULL " + References.SESSION_ID + "," + SessionsSpeakers.SPEAKER_ID + " TEXT NOT NULL " + References.SPEAKER_ID + "," + "UNIQUE (" + SessionsSpeakers.SESSION_ID + "," + SessionsSpeakers.SPEAKER_ID + ") ON CONFLICT REPLACE)"));
  db.execSQL(("CREATE TABLE " + Tables.SESSIONS_TAGS + " (" + BaseColumns._ID + " INTEGER PRIMARY KEY AUTOINCREMENT," + SessionsTags.SESSION_ID + " TEXT NOT NULL " + References.SESSION_ID + "," + SessionsTags.TAG_ID + " TEXT NOT NULL " + References.TAG_ID + "," + "UNIQUE (" + SessionsTags.SESSION_ID + "," + SessionsTags.TAG_ID + ") ON CONFLICT REPLACE)"));
  db.execSQL(("CREATE TABLE " + Tables.ANNOUNCEMENTS + " (" + BaseColumns._ID + " INTEGER PRIMARY KEY AUTOINCREMENT," + SyncColumns.UPDATED + " INTEGER NOT NULL," + AnnouncementsColumns.ANNOUNCEMENT_ID + " TEXT," + AnnouncementsColumns.ANNOUNCEMENT_TITLE + " TEXT NOT NULL," + AnnouncementsColumns.ANNOUNCEMENT_ACTIVITY_JSON + " BLOB," + AnnouncementsColumns.ANNOUNCEMENT_URL + " TEXT," + AnnouncementsColumns.ANNOUNCEMENT_DATE + " INTEGER NOT NULL)"));
  db.execSQL(("CREATE TABLE " + Tables.MAPTILES + " (" + BaseColumns._ID + " INTEGER PRIMARY KEY AUTOINCREMENT," + MapTileColumns.TILE_FLOOR + " INTEGER NOT NULL," + MapTileColumns.TILE_FILE + " TEXT NOT NULL," + MapTileColumns.TILE_URL + " TEXT NOT NULL," + "UNIQUE (" + MapTileColumns.TILE_FLOOR + ") ON CONFLICT REPLACE)"));
  db.execSQL(("CREATE TABLE " + Tables.FEEDBACK + " (" + BaseColumns._ID + " INTEGER PRIMARY KEY AUTOINCREMENT," + SyncColumns.UPDATED + " INTEGER NOT NULL," + Sessions.SESSION_ID + " TEXT " + References.SESSION_ID + "," + FeedbackColumns.SESSION_RATING + " INTEGER NOT NULL," + FeedbackColumns.ANSWER_RELEVANCE + " INTEGER NOT NULL," + FeedbackColumns.ANSWER_CONTENT + " INTEGER NOT NULL," + FeedbackColumns.ANSWER_SPEAKER + " INTEGER NOT NULL," + FeedbackColumns.COMMENTS + " TEXT," + FeedbackColumns.SYNCED + " INTEGER NOT NULL DEFAULT 0)"));
  db.execSQL(("CREATE TRIGGER " + Triggers.SESSIONS_FEEDBACK_DELETE + " AFTER DELETE ON " + Tables.SESSIONS + " BEGIN DELETE FROM " + Tables.FEEDBACK + " " + " WHERE " + Qualified.FEEDBACK_SESSION_ID + "=old." + Sessions.SESSION_ID + ";" + " END;"));
  db.execSQL(("CREATE TABLE " + Tables.MAPMARKERS + " (" + BaseColumns._ID + " INTEGER PRIMARY KEY AUTOINCREMENT," + MapMarkerColumns.MARKER_ID + " TEXT NOT NULL," + MapMarkerColumns.MARKER_TYPE + " TEXT NOT NULL," + MapMarkerColumns.MARKER_LATITUDE + " DOUBLE NOT NULL," + MapMarkerColumns.MARKER_LONGITUDE + " DOUBLE NOT NULL," + MapMarkerColumns.MARKER_LABEL + " TEXT," + MapMarkerColumns.MARKER_FLOOR + " INTEGER NOT NULL," + "UNIQUE (" + MapMarkerColumns.MARKER_ID + ") ON CONFLICT REPLACE)"));
  db.execSQL(("CREATE TABLE " + Tables.HASHTAGS + " (" + BaseColumns._ID + " INTEGER PRIMARY KEY AUTOINCREMENT," + HashtagColumns.HASHTAG_NAME + " TEXT NOT NULL," + HashtagColumns.HASHTAG_DESCRIPTION + " TEXT NOT NULL," + HashtagColumns.HASHTAG_COLOR + " INTEGER NOT NULL," + HashtagColumns.HASHTAG_ORDER + " INTEGER NOT NULL," + "UNIQUE (" + HashtagColumns.HASHTAG_NAME + ") ON CONFLICT REPLACE)"));
  db.execSQL(("CREATE TABLE " + Tables.VIDEOS + " (" + BaseColumns._ID + " INTEGER PRIMARY KEY AUTOINCREMENT," + VideoColumns.VIDEO_ID + " TEXT NOT NULL," + VideoColumns.VIDEO_YEAR + " INTEGER NOT NULL," + VideoColumns.VIDEO_TITLE + " TEXT," + VideoColumns.VIDEO_DESC + " TEXT," + VideoColumns.VIDEO_VID + " TEXT," + VideoColumns.VIDEO_TOPIC + " TEXT," + VideoColumns.VIDEO_SPEAKERS + " TEXT," + VideoColumns.VIDEO_THUMBNAIL_URL + " TEXT," + VideoColumns.VIDEO_IMPORT_HASHCODE + " TEXT NOT NULL," + "UNIQUE (" + VideoColumns.VIDEO_ID + ") ON CONFLICT REPLACE)"));
  db.execSQL(("CREATE VIRTUAL TABLE " + Tables.SESSIONS_SEARCH + " USING fts3(" + BaseColumns._ID + " INTEGER PRIMARY KEY AUTOINCREMENT," + SessionsSearchColumns.BODY + " TEXT NOT NULL," + SessionsSearchColumns.SESSION_ID + " TEXT NOT NULL " + References.SESSION_ID + "," + "UNIQUE (" + SessionsSearchColumns.SESSION_ID + ") ON CONFLICT REPLACE," + "tokenize=porter)"));
  db.execSQL(("CREATE TABLE " + Tables.SEARCH_SUGGEST + " (" + BaseColumns._ID + " INTEGER PRIMARY KEY AUTOINCREMENT," + SearchManager.SUGGEST_COLUMN_TEXT_1 + " TEXT NOT NULL)"));
  db.execSQL(("CREATE TRIGGER " + Triggers.SESSIONS_TAGS_DELETE + " AFTER DELETE ON " + Tables.SESSIONS + " BEGIN DELETE FROM " + Tables.SESSIONS_TAGS + " " + " WHERE " + Qualified.SESSIONS_TAGS_SESSION_ID + "=old." + Sessions.SESSION_ID + ";" + " END;"));
  db.execSQL(("CREATE TRIGGER " + Triggers.SESSIONS_SPEAKERS_DELETE + " AFTER DELETE ON " + Tables.SESSIONS + " BEGIN DELETE FROM " + Tables.SESSIONS_SPEAKERS + " " + " WHERE " + Qualified.SESSIONS_SPEAKERS_SESSION_ID + "=old." + Sessions.SESSION_ID + ";" + " END;"));
  db.execSQL(("CREATE TRIGGER " + Triggers.SESSIONS_MY_SCHEDULE_DELETE + " AFTER DELETE ON " + Tables.SESSIONS + " BEGIN DELETE FROM " + Tables.MY_SCHEDULE + " " + " WHERE " + Tables.MY_SCHEDULE + "." + MySchedule.SESSION_ID + "=old." + Sessions.SESSION_ID + ";" + " END;"));
  upgradeFrom2014Cto2015A(db);
  upgradeFrom2015Ato2015B(db);
  upgradeFrom2015Bto2016A(db);
}
