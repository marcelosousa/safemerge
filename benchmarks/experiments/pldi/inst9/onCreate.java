private static final String SCREEN_LABEL = "Feed";
private DatabaseReference mDatabaseReference;
private FeedContract.Presenter mPresenter;
@Override
 protected void onCreate (final Bundle savedInstanceState)
{
  super.onCreate(savedInstanceState);
  setContentView(R.layout.feed_act);
  setFullscreenLayout();
  disableActionBarTitle();
  FeedFragment feedFragment = FeedFragment.getSupportFragmentManager().findFragmentById(R.id.main_content);
  feedFragment.setRetainInstance(true);
  mPresenter = new FeedPresenter(feedFragment);
  feedFragment.setPresenter(mPresenter);
  return;
}
@Override
 protected void onCreate (final Bundle savedInstanceState)
{
  super.onCreate(savedInstanceState);
  FeedMessage.initCategoryColorMap(getResources());
  setContentView(R.layout.feed_act);
  FeedFragment feedFragment = FeedFragment.getSupportFragmentManager().findFragmentById(R.id.main_content);
  mPresenter = new FeedPresenter(feedFragment);
  feedFragment.setPresenter(mPresenter);
  mDatabaseReference = FirebaseDatabase.getInstance().getReference().child("feed");
  mPresenter.initializeDataListener(mDatabaseReference);
}
@Override
 protected void onCreate (final Bundle savedInstanceState)
{
  super.onCreate(savedInstanceState);
  FeedMessage.initCategoryColorMap(getResources());
  setContentView(R.layout.feed_act);
  setFullscreenLayout();
  disableActionBarTitle();
  FeedFragment feedFragment = FeedFragment.getSupportFragmentManager().findFragmentById(R.id.main_content);
  mPresenter = new FeedPresenter(feedFragment);
  feedFragment.setPresenter(mPresenter);
  mDatabaseReference = FirebaseDatabase.getInstance().getReference().child("feed");
  mPresenter.initializeDataListener(mDatabaseReference);
}
@Override
 protected void onCreate (final Bundle savedInstanceState)
{
  super.onCreate(savedInstanceState);
  setContentView(R.layout.feed_act);
  FeedFragment feedFragment = FeedFragment.getSupportFragmentManager().findFragmentById(R.id.main_content);
  feedFragment.setRetainInstance(true);
  mPresenter = new FeedPresenter(feedFragment);
  feedFragment.setPresenter(mPresenter);
}
@Override
 protected void onCreate (final Bundle savedInstanceState)
{
  super.onCreate(savedInstanceState);
  setContentView(R.layout.feed_act);
  setFullscreenLayout();
  disableActionBarTitle();
  FeedFragment feedFragment = FeedFragment.getSupportFragmentManager().findFragmentById(R.id.main_content);
  feedFragment.setRetainInstance(true);
  mPresenter = new FeedPresenter(feedFragment);
  feedFragment.setPresenter(mPresenter);
}
