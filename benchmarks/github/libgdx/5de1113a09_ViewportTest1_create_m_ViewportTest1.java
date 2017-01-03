{
  stage = new Stage();
  Skin skin = new Skin(Gdx.files.internal("data/uiskin.json"));
  label = new Label("", skin);
  Table root = new Table(skin);
  root.setFillParent(true);
  root.setBackground(skin.getDrawable("default-pane"));
  root.debug().defaults().space(6);
  root.add(new TextButton("Button 1", skin));
  root.add(new TextButton("Button 2", skin)).row();
  root.add("Press spacebar to change the viewport:").colspan(2).row();
  root.add(label).colspan(2);
  stage.addActor(root);
  int minWorldWidth = 300;
  int minWorldHeight = 225;
  int maxWorldWidth = 300;
  int maxWorldHeight = 168;
  Camera camera = stage.getCamera();
  viewports.add(new StretchViewport(minWorldWidth, minWorldHeight, camera));
  viewports.add(new FillViewport(minWorldWidth, minWorldHeight, camera));
  viewports.add(new FitViewport(minWorldWidth, minWorldHeight, camera));
  viewports.add(new ExtendViewport(minWorldWidth, minWorldHeight, camera));
  viewports.add(new ScreenViewport(camera));
  viewports.add(new ScalingViewport(Scaling.none, minWorldWidth, minWorldHeight, camera));
  viewports.add(new MinMaxViewport(minWorldWidth, minWorldHeight, maxWorldWidth, maxWorldHeight, true, camera));
  viewports.add(new MinMaxViewport(minWorldWidth, minWorldHeight, maxWorldWidth, maxWorldHeight, false, camera));
  stage.setViewport(viewports.first());
  Gdx.input.setInputProcessor(new InputMultiplexer(new InputAdapter()
                                                   {
                                                     public boolean keyDown (int keycode)
                                                     {
                                                       if (keycode == Input.Keys.SPACE)
                                                       {
                                                         Viewport viewport = viewports.get(((viewports.indexOf(stage.getViewport(), true) + 1) % viewports.size));
                                                         stage.setViewport(viewport);
                                                         resize(Gdx.graphics.getWidth(), Gdx.graphics.getHeight());
                                                       }
                                                       return false;
                                                     }
                                                   }, stage));
}