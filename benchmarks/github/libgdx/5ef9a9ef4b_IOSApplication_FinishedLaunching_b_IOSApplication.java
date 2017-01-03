{
  this.uiApp = uiApp;
  this.uiWindow = new UIWindow(UIScreen.get_MainScreen().get_Bounds());
  UIViewController uiViewController = new UIViewController()
                                      {
                                        @Override
                                        public void DidRotate (UIInterfaceOrientation orientation)
                                        {
                                          RectangleF bounds = getBounds(this);
                                          graphics.width = (int) bounds.get_Width();
                                          graphics.height = (int) bounds.get_Height();
                                          graphics.MakeCurrent();
                                          listener.resize(graphics.width, graphics.height);
                                        }
                                        @Override
                                        public boolean ShouldAutorotateToInterfaceOrientation (UIInterfaceOrientation orientation)
                                        {
                                          switch (orientation.Value)
                                          {
                                            case UIInterfaceOrientation.LandscapeLeft:
                                            case UIInterfaceOrientation.LandscapeRight:
                                              return config.orientationLandscape;
                                            default:
                                              return config.orientationPortrait;
                                          }
                                        }
                                      };
  this.uiWindow.set_RootViewController(uiViewController);
  this.input = new IOSInput(config);
  this.graphics = new IOSGraphics(getBounds(uiViewController), this, input);
  this.files = new IOSFiles();
  this.audio = new IOSAudio();
  this.net = new IOSNet(this);
  Gdx.files = this.files;
  Gdx.graphics = this.graphics;
  Gdx.audio = this.audio;
  Gdx.input = this.input;
  Gdx.net = this.net;
  this.input.setupPeripherals();
  uiViewController.set_View(graphics);
  this.graphics.Run();
  this.uiWindow.MakeKeyAndVisible();
  Gdx.app.log("IOSApplication", "created");
  return true;
}