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