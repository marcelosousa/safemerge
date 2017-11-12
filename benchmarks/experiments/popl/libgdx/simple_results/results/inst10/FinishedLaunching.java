IOSAudio audio;
IOSApplicationConfiguration config;
IOSFiles files;
boolean firstResume;
IOSGraphics graphics;
IOSInput input;
ApplicationListener listener;
int logLevel = Application.LOG_DEBUG;
IOSNet net;
UIApplication uiApp;
UIWindow uiWindow;
@Override
 public boolean FinishedLaunching (UIApplication uiApp, NSDictionary options)
{
  this.uiApp = uiApp;
  this.uiWindow = new UIWindow(UIScreen.get_MainScreen().get_Bounds());
  UIViewController uiViewController = new UIViewController();
  this.uiWindow.set_RootViewController(uiViewController);
  this.input = new IOSInput(config);
  this.graphics = new IOSGraphics(getBounds(uiViewController), this, input);
  this.files = new IOSFiles();
  this.audio = new IOSAudio();
  this.net = new IOSNet(this);
  Gdx.files(this.files);
  Gdx.graphics(this.graphics);
  Gdx.audio(this.audio);
  Gdx.input(this.input);
  Gdx.net(this.net);
  this.input.setupPeripherals();
  uiViewController.set_View(graphics);
  this.graphics.Run();
  this.uiWindow.MakeKeyAndVisible();
  Gdx.app.debug("IOSApplication", "created");
  return true;
}
