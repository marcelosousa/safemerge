@Override
 protected void onCreate (final Bundle savedInstanceState)
{
  super.onCreate(savedInstanceState);
  setContentView(R.layout.feed_act);
  setFullscreenLayout();
  disableActionBarTitle();
  FeedFragment feedFragment = (FeedFragment) getSupportFragmentManagerfindFragmentById(R.id.main_content);
  feedFragment.setRetainInstance(true);
  mPresenter = new FeedPresenter(feedFragment);
  feedFragment.setPresenter(mPresenter);
  return;
}