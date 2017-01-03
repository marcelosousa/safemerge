{
  float screenAspectRatio = (float) screenWidth / (float) screenHeight;
  if (snap)
  {
    if (Math.abs((screenAspectRatio - minAspectRatio)) < Math.abs((screenAspectRatio - maxAspectRatio)))
    {
      setWorldSize(minWorldWidth, minWorldHeight);
    }
    else
    {
      setWorldSize(maxWorldWidth, maxWorldHeight);
    }
  }
  else
  {
    if (screenAspectRatio < minAspectRatio)
    {
      setWorldSize(minWorldWidth, minWorldHeight);
    }
    else
      if (screenAspectRatio > maxAspectRatio)
      {
        setWorldSize(maxWorldWidth, maxWorldHeight);
      }
      else
        if (Math.abs((screenAspectRatio - minAspectRatio)) < Math.abs((screenAspectRatio - maxAspectRatio)))
        {
          setWorldSize(minWorldWidth, (minWorldWidth / screenAspectRatio));
        }
        else
        {
          setWorldSize(maxWorldWidth, (maxWorldWidth / screenAspectRatio));
        }
  }
  super.update(screenWidth, screenHeight, centerCamera);
}