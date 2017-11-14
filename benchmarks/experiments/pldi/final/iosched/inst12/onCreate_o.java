@Override
 protected void onCreate (final Bundle savedInstanceState)
{
  super.onCreate(savedInstanceState);
  FeedMessage.initCategoryColorMap(getResources());
  setContentView(R.layout.feed_act);
  FeedFragment feedFragment = (FeedFragment) getSupportFragmentManagerfindFragmentById(R.id.main_content);
  mPresenter = new FeedPresenter(feedFragment);
  feedFragment.setPresenter(mPresenter);
  mDatabaseReference = FirebaseDatabase.getInstance().getReference().child("feed");
  mPresenter.initializeDataListener(mDatabaseReference);
  return;
}