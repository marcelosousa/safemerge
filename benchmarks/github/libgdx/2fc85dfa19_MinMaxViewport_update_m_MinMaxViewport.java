{
  float screenAspectRatio = (float) screenWidth / (float) screenHeight;
  if (snap)
  {
    if (Math.abs((screenAspectRatio - minAspectRatio)) < Math.abs((screenAspectRatio - maxAspectRatio)))
    {
      setScaling(Scaling.fit);
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
      setScaling(Scaling.fit);
      setWorldSize(minWorldWidth, minWorldHeight);
    }
    else
      if (screenAspectRatio > maxAspectRatio)
      {
        setScaling(Scaling.fit);
        setWorldSize(maxWorldWidth, maxWorldHeight);
      }
      else
        if ((screenWidth > maxWorldWidth || screenHeight) > maxWorldHeight)
        {
          setScaling(Scaling.fit);
          setWorldSize(maxWorldWidth, (maxWorldWidth / screenAspectRatio));
        }
        else
          if ((screenWidth < minWorldWidth || screenHeight) < minWorldHeight)
          {
            setScaling(Scaling.fit);
            setWorldSize(minWorldWidth, (minWorldWidth / screenAspectRatio));
          }
          else
          {
            setScaling(Scaling.fill);
            setWorldSize(screenWidth, screenHeight);
          }
  }
  super.update(screenWidth, screenHeight, centerCamera);
}