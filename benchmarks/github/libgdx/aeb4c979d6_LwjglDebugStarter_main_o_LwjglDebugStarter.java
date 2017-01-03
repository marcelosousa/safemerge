{
  new SharedLibraryLoader("../../gdx/libs/gdx-natives.jar").load("gdx");
  new SharedLibraryLoader("../../extensions/gdx-audio/libs/gdx-audio-natives.jar").load("gdx-audio");
  new SharedLibraryLoader("../../extensions/gdx-image/libs/gdx-image-natives.jar").load("gdx-image");
  new SharedLibraryLoader("../../extensions/gdx-freetype/libs/gdx-freetype-natives.jar").load("gdx-freetype");
  GdxTest test = new SpriteBatchShaderTest();
  LwjglApplicationConfiguration config = new LwjglApplicationConfiguration();
  config.useGL20 = test.needsGL20();
  new LwjglApplication(test, config);
}
