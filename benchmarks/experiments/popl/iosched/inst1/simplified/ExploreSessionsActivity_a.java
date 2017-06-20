package com.google.samples.apps.iosched.explore;

/**
 * This activity displays all sessions based on the selected filters.
 * <p/>
 * It can either be invoked with specific filters or the user can choose the filters to use from the
 * alt_nav_bar.
 */
public class ExploreSessionsActivity extends BaseActivity
        implements Toolbar.OnMenuItemClickListener, LoaderManager.LoaderCallbacks<Cursor> {

    public static final String EXTRA_FILTER_TAG =
            "com.google.samples.apps.iosched.explore.EXTRA_FILTER_TAG";
    public static final String EXTRA_SHOW_LIVE_STREAM_SESSIONS =
            "com.google.samples.apps.iosched.explore.EXTRA_SHOW_LIVE_STREAM_SESSIONS";

    // The saved instance state filters
    private static final String STATE_FILTER_TAGS =
            "com.google.samples.apps.iosched.explore.STATE_FILTER_TAGS";
    private static final String STATE_CURRENT_URI =
            "com.google.samples.apps.iosched.explore.STATE_CURRENT_URI";

    private static final String SCREEN_LABEL = "ExploreSessions";

    private static final String TAG = makeLogTag(ExploreSessionsActivity.class);

    private static final int TAG_METADATA_TOKEN = 0x8;

    private static final int MODE_TIME_FIT = 1;

    private static final int MODE_EXPLORE = 2;

    private TagMetadata mTagMetadata;

    private TagFilterHolder mTagFilterHolder;

    // Keep track of the current URI. This can diverge from Intent.getData() if the user
    // dismisses a particular timeslot. At that point, the Activity switches the mode
    // as well as the Uri used.
    private Uri mCurrentUri;

    private ExploreSessionsFragment mFragment;

    private int mMode;

    private RecyclerView mFiltersList;

    private DrawerLayout mDrawerLayout;

    private CollapsingToolbarLayout mCollapsingToolbar;

    private ImageView mHeaderImage;

    private TextView mTitle;

    private View mTimeSlotLayout;

    private View mTimeSlotDivider;

    private ImageLoader mImageLoader;

    private int setHeader() {
        if ((mMode == MODE_EXPLORE) && (mTagMetadata != null)) {
            String title = null;
            String headerImage = null;
            int trackColor = Color.TRANSPARENT;

            // If exactly one theme or one track is selected, show its title and image.
            int selectedTracks = mTagFilterHolder.getCountByCategory(Config.Tags.CATEGORY_TRACK);
            int selectedThemes = mTagFilterHolder.getCountByCategory(Config.Tags.CATEGORY_THEME);
            if ((selectedThemes + selectedTracks) == 1) {
                Container c = mTagFilterHolder.getSelectedFilters();
                String tagId;
                int brk = 0;
                int len = c.size();
                for (int i = 0; ((i < len) && (brk == 0)); i++) {
                    tagId = c.get(i);
                    if ((TagUtils.isTrackTag(tagId) == 1) || (TagUtils.isThemeTag(tagId) == 1)) {
                        TagMetadata.Tag selectedTag = mTagMetadata.getTag(tagId);
                        title = selectedTag.getName();
                        headerImage = selectedTag.getPhotoUrl();
                        trackColor = selectedTag.getColor();
                        brk = 1;
                    }
                }
            }

            if (title == null) {
                title = getString(R.string.title_explore);
            }
            mTitle.setText(title);

            if (headerImage != null) {
                mHeaderImage.setScaleType(ImageView.ScaleType.CENTER_CROP);
                mImageLoader.loadImage(headerImage, mHeaderImage);
            } else {
                mHeaderImage.setScaleType(ImageView.ScaleType.FIT_CENTER);
                mHeaderImage.setImageResource(R.drawable.ic_hash_io_16_monochrome);
            }

            int statusBarColor = 0;
            if (trackColor != Color.TRANSPARENT) 
            { 
              statusBarColor = UIUtils.adjustColorForStatusBar(trackColor);
            } else
            {
              statusBarColor = UIUtils.getThemeColor(this, R.attr.colorPrimaryDark,R.color.theme_primary_dark);
            }
            mCollapsingToolbar.setContentScrimColor(trackColor);
            mDrawerLayout.setStatusBarBackgroundColor(statusBarColor);
        }
    return 0;
    }
}
