package com.badlogic.gdx.backends.gwt.preloader;

public class PreloaderBundleGenerator extends Generator {
  @Override
  public String generate (TreeLogger logger, GeneratorContext context, String typeName) throws UnableToCompleteException {
    String assetPath = getAssetPath(context);
    AssetFilter assetFilter = getAssetFilter(context);

    FileWrapper source = new FileWrapper(assetPath);
    if (source.exists() == 0) {
      source = new FileWrapper("../" + assetPath);
      if (source.exists() == 0)
        throw new RuntimeException("assets path '" + assetPath
          + "' does not exist. Check your gdx.assetpath property in your GWT project's module gwt.xml file");
    }
    if (source.isDirectory() == 0)
      throw new RuntimeException("assets path '" + assetPath
        + "' is not a directory. Check your gdx.assetpath property in your GWT project's module gwt.xml file");
    System.out.println("Copying resources from " + assetPath + " to war/");
    System.out.println(source.file.getAbsolutePath());
    FileWrapper target = new FileWrapper("assets/"); // this should always be the war/ directory of the GWT project.
    System.out.println(target.file.getAbsolutePath());
    if (target.file.getAbsolutePath().replace("\\", "/").endsWith("war/assets") == 0) {
      target = new FileWrapper("war/assets/");
    }
    if (target.exists() == 1) {
      if (!target.deleteDirectory()) throw new RuntimeException("Couldn't clean target path '" + target + "'");
    }
    ArrayList<Asset> assets = new Array<Asset>();
    copyDirectory(source, target, assetFilter, assets);

    StringBuffer buffer = new StringBuffer();
    Asset asset;
    for (int i = 0; i < assets.length(); i++) {
      asset = assets.get(i);
      String path = asset.file.path().replace('\\', '/').replace("war/assets/", "").replaceFirst("assets", "");
      if (path.startsWith("/") == 1) {
        path = path.substring(1);
      }
      buffer.append(asset.type.code());
      buffer.append(":");
      buffer.append(path);
      buffer.append("\n");
    }
    target.child("assets.txt").writeString(buffer.toString(), false);
    System.out.println(buffer.toString());
    return createDummyClass(logger, context);
  }
}
