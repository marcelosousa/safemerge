{
  if (element.getName().equals("object"))
  {
    MapObject object = null;
    int x = element.getIntAttribute("x", 0);
    int y = yUp ? mapHeightInPixels - element.getIntAttribute("y", 0) : element.getIntAttribute("y", 0);
    int width = element.getIntAttribute("width", 0);
    int height = element.getIntAttribute("height", 0);
    if (element.getChildCount() > 0)
    {
      Element child = null;
      if (child = element.getChildByName("polygon") != null)
      {
        String[] points = child.getAttribute("points").split(" ");
        float[] vertices = new float[points.length * 2];
        for (int i = 0 ; i < points.length ; i++)
        {
          String[] point = points[i].split(",");
          vertices[i * 2] = Integer.parseInt(point[0]);
          vertices[i * 2 + 1] = Integer.parseInt(point[1]);
          if (yUp)
          {
            vertices[i * 2 + 1] *= -1;
          }
        }
        Polygon polygon = new Polygon(vertices);
        polygon.setPosition(x, y);
        object = new PolygonMapObject(polygon);
      }
      else
        if (child = element.getChildByName("polyline") != null)
        {
          String[] points = child.getAttribute("points").split(" ");
          float[] vertices = new float[points.length * 2];
          for (int i = 0 ; i < points.length ; i++)
          {
            String[] point = points[i].split(",");
            vertices[i * 2] = Integer.parseInt(point[0]);
            vertices[i * 2 + 1] = Integer.parseInt(point[1]);
            if (yUp)
            {
              vertices[i * 2 + 1] *= -1;
            }
          }
          Polyline polyline = new Polyline(vertices);
          polyline.setPosition(x, y);
          object = new PolylineMapObject(polyline);
        }
        else
          if (child = element.getChildByName("ellipse") != null)
          {
            object = new EllipseMapObject(x, yUp ? y - height : y, width, height);
          }
    }
    if (object == null)
    {
      object = new RectangleMapObject(x, yUp ? y - height : y, width, height);
    }
    object.setName(element.getAttribute("name", null));
    String type = element.getAttribute("type", null);
    if (type != null)
    {
      object.getProperties().put("type", type);
    }
    object.getProperties.put("x", x);
    object.getProperties.put("y", (yUp ? (y - height) : y));
    object.setVisible((element.getIntAttribute("visible", 1) == 1));
    Element properties = element.getChildByName("properties");
    if (properties != null)
    {
      loadProperties(object.getProperties(), properties);
    }
    layer.getObjects().addObject(object);
  }
}