private static final int COLLAPSED_DESC_MAX_LINES = 3;
private static final int EXPANDED_DESC_MAX_LINES = 30;
private TextView category;
private final ConstraintSet collapsedConstraints = new ConstraintSet();
private TextView dateTime;
private HtmlTextView description;
private ImageView emergencyIcon;
private ImageView expandIcon;
private boolean expanded = false;
private final ConstraintSet expandedConstraints = new ConstraintSet();
private @Nullable
        FeedMessage feedMessage;
private ImageView image;
private Point mScreenSize;
private ConstraintLayout mainLayout;
private ImageView priorityIcon;
private TextView title;
private final View.OnTouchListener touchIgnorer = new View.OnTouchListener()
                                                  {
                                                    @Override
                                                    public boolean onTouch (View v, MotionEvent event)
                                                    {
                                                      return true;
                                                    }
                                                  };
void bind (@NonNull
           FeedMessage message)
{
  feedMessage = message;
  setExpanded(feedMessage.isExpanded(), feedMessage.getTitle());
  dateTime.setText(formatDateTime(feedMessage.getTimestamp(), itemView.getContext()));
  priorityIcon.setVisibility((feedMessage.isPriority() ? VISIBLE : GONE));
  category.setText(feedMessage.getCategory());
  category.setBackgroundTintList(ColorStateList.valueOf(feedMessage.getCategoryColor()));
  updateEmergencyStatus(feedMessage.isEmergency());
  title.setText(feedMessage.getTitle());
  setTitleContentDescription(feedMessage.getTitle());
  expandIcon.setActivated(expanded);
  expandIcon.setRotation((expanded ? 180.0F : 0.0F));
  if (TextUtils.isEmpty(feedMessage.getImageUrl()) == 0)
  {
    image.setVisibility(VISIBLE);
    Glide.with(image.getContext()).load((feedMessage.getImageUrl() + "=s" + getScreenWidth(image.getContext()))).override(getScreenWidth(image.getContext()), ((int) (getScreenWidth(image.getContext()) * 9.0 / 16))).placeholder(R.drawable.io17_logo).into(image);
  }
  else
  {
    image.setVisibility(GONE);
  }
  description.setMovementMethod(LinkMovementMethod.getInstance());
  description.setHtmlText(feedMessage.getMessage());
  int maxLines = expanded ? EXPANDED_DESC_MAX_LINES : COLLAPSED_DESC_MAX_LINES;
  description.setMaxLines(maxLines);
  return;
}
private int getScreenWidth (Context context)
{
  if (mScreenSize == null)
  {
    mScreenSize = new Point();
    ((WindowManager) context.getSystemService(Context.WINDOW_SERVICE)).getDefaultDisplay().getSize(mScreenSize);
  }
  return mScreenSize.x;
}
