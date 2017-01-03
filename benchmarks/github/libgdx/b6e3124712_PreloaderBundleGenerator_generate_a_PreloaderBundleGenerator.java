{
  String assetPath = getAssetPath(context);
  AssetFilter assetFilter = getAssetFilter(context);
  FileWrapper source = new FileWrapper(assetPath);
  if (!source.exists())
  {
    source = new FileWrapper("../" + assetPath);
    if (!source.exists())
      throw new RuntimeException("assets path '" + assetPath + "' does not exist. Check your gdx.assetpath property in your GWT project's module gwt.xml file");
  }
  if (!source.isDirectory())
    throw new RuntimeException("assets path '" + assetPath + "' is not a directory. Check your gdx.assetpath property in your GWT project's module gwt.xml file");
  System.out.println(("Copying resources from " + assetPath + " to war/"));
  System.out.println(source.file.getAbsolutePath());
  FileWrapper target = new FileWrapper("assets/");
  System.out.println(target.file.getAbsolutePath());
  if (!target.file.getAbsolutePath().replace("\\", "/").endsWith("war/assets"))
  {
    target = new FileWrapper("war/assets/");
  }
  if (target.exists())
  {
    if (!target.deleteDirectory())
      throw new RuntimeException("Couldn't clean target path '" + target + "'");
  }
  ArrayList<Asset> assets = new ArrayList<Asset>();
  copyDirectory(source, target, assetFilter, assets);
  StringBuffer buffer = new StringBuffer();
  for (Asset asset : assets) {
                               String path = asset.file.path().replace('\\', '/').replace("war/assets/", "").replaceFirst("assets", "");
                               if (path.startsWith("/"))
                                 path = path.substring(1);
                               buffer.append(asset.type.code);
                               buffer.append(":");
                               buffer.append(path);
                               buffer.append("\n");
                             }
  target.child("assets.txt").writeString(buffer.toString(), false);
  System.out.println(buffer.toString());
  return createDummyClass(logger, context);
}