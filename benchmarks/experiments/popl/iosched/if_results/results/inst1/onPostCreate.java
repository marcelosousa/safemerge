public static final String BUNDLE_STATE_MAPVIEW = "mapview";
public static final String EXTRA_DETACHED_MODE = "com.google.samples.apps.iosched.EXTRA_DETACHED_MODE";
public static final String EXTRA_ROOM = "com.google.android.iosched.extra.ROOM";
private static final String SCREEN_LABEL = "Map";
private static final String TAG = makeLogTag(MapActivity.class);
private boolean mDetachedMode;
private View mInfoContainer;
private MapInfoFragment mInfoFragment;
private MapFragment mMapFragment;
public static final String LOCATION_PERMISSION = Manifest.permission.ACCESS_FINE_LOCATION;
private static final int REQUEST_LOCATION_PERMISSION = 1;
@Override
 protected void onPostCreate (Bundle savedInstanceState)
{
  super.onPostCreate(savedInstanceState);
  if (mDetachedMode)
  {
    final Toolbar toolbar = getToolbar();
    toolbar.setNavigationIcon(R.drawable.ic_up);
    toolbar.setNavigationOnClickListener();
  }
  else
    ;
  if (mMapFragment == null)
  {
    if (savedInstanceState != null)
    {
      Bundle previousState = savedInstanceState.getBundle(BUNDLE_STATE_MAPVIEW);
      mMapFragment = MapFragment.newInstance(previousState);
    }
    else
    {
      final String highlightRoomId = getIntent().hasExtra(EXTRA_ROOM) ? getIntent().getExtras().getString(EXTRA_ROOM) : null;
      mMapFragment = MapFragment.newInstance(highlightRoomId);
    }
    getFragmentManager().beginTransaction().add(R.id.fragment_container_map, mMapFragment, "map").commit();
  }
  else
    ;
  if (mInfoFragment == null)
  {
    mInfoFragment = MapInfoFragment.newInstace(this);
    getFragmentManager().beginTransaction().add(R.id.fragment_container_map_info, mInfoFragment, "mapsheet").commit();
  }
  else
    ;
  mDetachedMode = getIntent().getBooleanExtra(EXTRA_DETACHED_MODE, false);
  attemptEnableMyLocation();
  return;
}
@Override
 protected void onPostCreate (Bundle savedInstanceState)
{
  super.onPostCreate(savedInstanceState);
  if (mDetachedMode)
  {
    final Toolbar toolbar = getActionBarToolbar();
    toolbar.setNavigationIcon(R.drawable.ic_up);
    toolbar.setNavigationOnClickListener();
  }
  if (mMapFragment == null)
  {
    if (savedInstanceState != null)
    {
      Bundle previousState = savedInstanceState.getBundle(BUNDLE_STATE_MAPVIEW);
      mMapFragment = MapFragment.newInstance(previousState);
    }
    else
    {
      final String highlightRoomId = getIntent().hasExtra(EXTRA_ROOM) ? getIntent().getExtras().getString(EXTRA_ROOM) : null;
      mMapFragment = MapFragment.newInstance(highlightRoomId);
    }
    getFragmentManager().beginTransaction().add(R.id.fragment_container_map, mMapFragment, "map").commit();
  }
  if (mInfoFragment == null)
  {
    mInfoFragment = MapInfoFragment.newInstace(this);
    getFragmentManager().beginTransaction().add(R.id.fragment_container_map_info, mInfoFragment, "mapsheet").commit();
  }
  mDetachedMode = getIntent().getBooleanExtra(EXTRA_DETACHED_MODE, false);
  return;
}
@Override
 protected void onPostCreate (Bundle savedInstanceState)
{
  super.onPostCreate(savedInstanceState);
  if (mDetachedMode)
  {
    final Toolbar toolbar = getToolbar();
    toolbar.setNavigationIcon(R.drawable.ic_up);
    toolbar.setNavigationOnClickListener();
  }
  if (mMapFragment == null)
  {
    if (savedInstanceState != null)
    {
      Bundle previousState = savedInstanceState.getBundle(BUNDLE_STATE_MAPVIEW);
      mMapFragment = MapFragment.newInstance(previousState);
    }
    else
    {
      final String highlightRoomId = getIntent().hasExtra(EXTRA_ROOM) ? getIntent().getExtras().getString(EXTRA_ROOM) : null;
      mMapFragment = MapFragment.newInstance(highlightRoomId);
    }
    getFragmentManager().beginTransaction().add(R.id.fragment_container_map, mMapFragment, "map").commit();
  }
  if (mInfoFragment == null)
  {
    mInfoFragment = MapInfoFragment.newInstace(this);
    getFragmentManager().beginTransaction().add(R.id.fragment_container_map_info, mInfoFragment, "mapsheet").commit();
  }
  mDetachedMode = getIntent().getBooleanExtra(EXTRA_DETACHED_MODE, false);
  return;
}
@Override
 protected void onPostCreate (Bundle savedInstanceState)
{
  super.onPostCreate(savedInstanceState);
  if (mDetachedMode)
  {
    final Toolbar toolbar = getActionBarToolbar();
    toolbar.setNavigationIcon(R.drawable.ic_up);
    toolbar.setNavigationOnClickListener();
  }
  if (mMapFragment == null)
  {
    if (savedInstanceState != null)
    {
      Bundle previousState = savedInstanceState.getBundle(BUNDLE_STATE_MAPVIEW);
      mMapFragment = MapFragment.newInstance(previousState);
    }
    else
    {
      final String highlightRoomId = getIntent().hasExtra(EXTRA_ROOM) ? getIntent().getExtras().getString(EXTRA_ROOM) : null;
      mMapFragment = MapFragment.newInstance(highlightRoomId);
    }
    getFragmentManager().beginTransaction().add(R.id.fragment_container_map, mMapFragment, "map").commit();
  }
  if (mInfoFragment == null)
  {
    mInfoFragment = MapInfoFragment.newInstace(this);
    getFragmentManager().beginTransaction().add(R.id.fragment_container_map_info, mInfoFragment, "mapsheet").commit();
  }
  mDetachedMode = getIntent().getBooleanExtra(EXTRA_DETACHED_MODE, false);
  attemptEnableMyLocation();
  return;
}
@Override
 protected void onPostCreate (Bundle savedInstanceState)
{
  super.onPostCreate(savedInstanceState);
  if (mDetachedMode)
  {
    final Toolbar toolbar = getToolbar();
    toolbar.setNavigationIcon(R.drawable.ic_up);
    toolbar.setNavigationOnClickListener();
  }
  if (mMapFragment == null)
  {
    if (savedInstanceState != null)
    {
      Bundle previousState = savedInstanceState.getBundle(BUNDLE_STATE_MAPVIEW);
      mMapFragment = MapFragment.newInstance(previousState);
    }
    else
    {
      final String highlightRoomId = getIntent().hasExtra(EXTRA_ROOM) ? getIntent().getExtras().getString(EXTRA_ROOM) : null;
      mMapFragment = MapFragment.newInstance(highlightRoomId);
    }
    getFragmentManager().beginTransaction().add(R.id.fragment_container_map, mMapFragment, "map").commit();
  }
  if (mInfoFragment == null)
  {
    mInfoFragment = MapInfoFragment.newInstace(this);
    getFragmentManager().beginTransaction().add(R.id.fragment_container_map_info, mInfoFragment, "mapsheet").commit();
  }
  mDetachedMode = getIntent().getBooleanExtra(EXTRA_DETACHED_MODE, false);
  attemptEnableMyLocation();
  return;
}
public void attemptEnableMyLocation ()
{
  if (hasPermission())
  {
    if (mMapFragment != null)
    {
      mMapFragment.setMyLocationEnabled(true);
      return;
    }
  }
  ActivityCompat.requestPermissions(this, new String[] {
                                                         LOCATION_PERMISSION,
                                                       }, REQUEST_LOCATION_PERMISSION);
}
