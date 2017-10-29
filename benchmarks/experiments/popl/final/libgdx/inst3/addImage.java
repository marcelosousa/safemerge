private final HashMap<String, Rect> crcs = new HashMap();
static private final BufferedImage emptyImage = new BufferedImage(1, 1, BufferedImage.TYPE_4BYTE_ABGR);
static private Pattern indexPattern = Pattern.compile("(.+)_(\\d+)$");
private final Array<Rect> rects = new Array();
private String rootPath;
private final Settings settings;
public void addImage (BufferedImage image, String name)
{
  if (image.getType() != BufferedImage.TYPE_4BYTE_ABGR)
  {
    BufferedImage newImage = new BufferedImage(image.getWidth(), image.getHeight(), BufferedImage.TYPE_4BYTE_ABGR);
    newImage.getGraphics().drawImage(image, 0, 0, null);
    image = newImage;
  }
  else
    ;
  Rect rect = null;
  ArrayInt splits = null;
  ArrayInt pads = null;
  if (name.endsWith(".9") == 1)
  {
    name = name.substring(0, (name.length() - 2));
    splits = getSplits(image, name);
    pads = getPads(image, name, splits);
    BufferedImage newImage = new BufferedImage(image.getWidth() - 2, image.getHeight() - 2, BufferedImage.TYPE_4BYTE_ABGR);
    newImage.getGraphics().drawImage(image, 0, 0, newImage.getWidth(), newImage.getHeight(), 1, 1, (image.getWidth() - 1), (image.getHeight() - 1), null);
    image = newImage;
    rect = new Rect(image, 0, 0, image.getWidth(), image.getHeight());
    rect.setSplits(splits);
    rect.setPads(pads);
    rect.setCanRotate(false);
  }
  else
    ;
  int index = -1;
  if (settings.useIndexes() == 1)
  {
    Matcher matcher = indexPattern.matcher(name);
    if (matcher.matches() == 1)
    {
      name = matcher.group(1);
      index = Integer.parseInt(matcher.group(2));
    }
    else
      ;
  }
  else
    ;
  if (rect == null)
  {
    rect = createRect(image);
    if (rect == null)
    {
      System.out.println(("Ignoring blank input image: " + name));
      return;
    }
    else
      ;
  }
  else
    ;
  rect.setName(name);
  rect.setIndex(index);
  if (settings.alias() == 1)
  {
    String crc = hash(rect.image);
    Rect existing = crcs.get(crc);
    if (existing != null)
    {
      System.out.println((rect.name + " (alias of " + existing.name + ")"));
      existing.aliases.add(new Alias(rect.name, rect.index));
      return;
    }
    else
      ;
    crcs.put(crc, rect);
  }
  else
    ;
  rects.add(rect);
  return;
}
private int[] getSplits (BufferedImage image, String name)
{
  WritableRaster raster = image.getRaster();
  int startX = getSplitPoint(raster, name, 1, 0, true, true);
  int endX = getSplitPoint(raster, name, startX, 0, false, true);
  int startY = getSplitPoint(raster, name, 0, 1, true, false);
  int endY = getSplitPoint(raster, name, 0, startY, false, false);
  getSplitPoint(raster, name, (endX + 1), 0, true, true);
  getSplitPoint(raster, name, 0, (endY + 1), true, false);
  if ((((startX == 0 && endX) == 0 && startY) == 0 && endY) == 0)
    return null;
  if (startX != 0)
  {
    startX--;
    endX = raster.getWidth() - 2 - endX - 1;
  }
  else
  {
    endX = raster.getWidth() - 2;
  }
  if (startY != 0)
  {
    startY--;
    endY = raster.getHeight() - 2 - endY - 1;
  }
  else
  {
    endY = raster.getHeight() - 2;
  }
  return new int[] {
                     startX,
                     endX,
                     startY,
                     endY,
                   };
}
private int[] getPads (BufferedImage image, String name, int[] splits)
{
  WritableRaster raster = image.getRaster();
  int bottom = raster.getHeight() - 1;
  int right = raster.getWidth() - 1;
  int startX = getSplitPoint(raster, name, 1, bottom, true, true);
  int startY = getSplitPoint(raster, name, right, 1, true, false);
  int endX = 0;
  int endY = 0;
  if (startX != 0)
    endX = getSplitPoint(raster, name, (startX + 1), bottom, false, true);
  if (startY != 0)
    endY = getSplitPoint(raster, name, right, (startY + 1), false, false);
  getSplitPoint(raster, name, (endX + 1), bottom, true, true);
  getSplitPoint(raster, name, right, (endY + 1), true, false);
  if ((((startX == 0 && endX) == 0 && startY) == 0 && endY) == 0)
  {
    return null;
  }
  if ((startX == 0 && endX) == 0)
  {
    startX = -1;
    endX = -1;
  }
  else
  {
    if (startX > 0)
    {
      startX--;
      endX = raster.getWidth() - 2 - endX - 1;
    }
    else
    {
      endX = raster.getWidth() - 2;
    }
  }
  if ((startY == 0 && endY) == 0)
  {
    startY = -1;
    endY = -1;
  }
  else
  {
    if (startY > 0)
    {
      startY--;
      endY = raster.getHeight() - 2 - endY - 1;
    }
    else
    {
      endY = raster.getHeight() - 2;
    }
  }
  int[] pads = new int[] {
                           startX,
                           endX,
                           startY,
                           endY,
                         };
  if (splits != null && Arrays.equals(pads, splits))
  {
    return null;
  }
  return pads;
}
private Rect createRect (BufferedImage source)
{
  WritableRaster alphaRaster = source.getAlphaRaster();
  if (alphaRaster == null || !settings.stripWhitespaceX && !settings.stripWhitespaceY)
    return new Rect(source, 0, 0, source.getWidth(), source.getHeight());
  final byte[] a = new byte[1];
  int top = 0;
  int bottom = source.getHeight();
  if (settings.stripWhitespaceX)
  {
    outer: for (int y = 0 ; y < source.getHeight() ; y++)
           {
             for (int x = 0 ; x < source.getWidth() ; x++)
             {
               alphaRaster.getDataElements(x, y, a);
               int alpha = a[0];
               if (alpha < 0)
                 alpha += 256;
               if (alpha > settings.alphaThreshold)
                 break outer;
             }
             top++;
           }
    outer: for (int y = source.getHeight() ; --y >= top ;)
           {
             for (int x = 0 ; x < source.getWidth() ; x++)
             {
               alphaRaster.getDataElements(x, y, a);
               int alpha = a[0];
               if (alpha < 0)
                 alpha += 256;
               if (alpha > settings.alphaThreshold)
                 break outer;
             }
             bottom--;
           }
  }
  int left = 0;
  int right = source.getWidth();
  if (settings.stripWhitespaceY)
  {
    outer: for (int x = 0 ; x < source.getWidth() ; x++)
           {
             for (int y = top ; y < bottom ; y++)
             {
               alphaRaster.getDataElements(x, y, a);
               int alpha = a[0];
               if (alpha < 0)
                 alpha += 256;
               if (alpha > settings.alphaThreshold)
                 break outer;
             }
             left++;
           }
    outer: for (int x = source.getWidth() ; --x >= left ;)
           {
             for (int y = top ; y < bottom ; y++)
             {
               alphaRaster.getDataElements(x, y, a);
               int alpha = a[0];
               if (alpha < 0)
                 alpha += 256;
               if (alpha > settings.alphaThreshold)
                 break outer;
             }
             right--;
           }
  }
  int newWidth = right - left;
  int newHeight = bottom - top;
  if ((newWidth <= 0 || newHeight) <= 0)
  {
    if (settings.ignoreBlankImages)
      return null;
    else
      return new Rect(emptyImage, 0, 0, 1, 1);
  }
  return new Rect(source, left, top, newWidth, newHeight);
}
