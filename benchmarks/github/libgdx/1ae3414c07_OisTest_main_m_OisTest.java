{
  DesktopControllersBuild.main(null);
  new SharedLibraryLoader("libs/gdx-controllers-desktop-natives.jar").load("gdx-controllers-desktop");
  ApplicationAdapter app = new ApplicationAdapter()
                           {
                             Ois ois;
                             @Override
                             public void create ()
                             {
                               this.ois = new Ois();
                               if (ois.getJoysticks().size() > 0)
                               {
                                 ois.getJoysticks().get(0).setListener(new OisListener()
                                                                       {
                                                                         @Override
                                                                         public void sliderMoved (OisJoystick joystick, int slider, boolean x, boolean y)
                                                                         {
                                                                         }
                                                                         @Override
                                                                         public void povMoved (OisJoystick joystick, int pov, OisPov direction)
                                                                         {
                                                                         }
                                                                         @Override
                                                                         public void buttonReleased (OisJoystick joystick, int button)
                                                                         {
                                                                         }
                                                                         @Override
                                                                         public void buttonPressed (OisJoystick joystick, int button)
                                                                         {
                                                                         }
                                                                         @Override
                                                                         public void axisMoved (OisJoystick joystick, int axis, float value)
                                                                         {
                                                                         }
                                                                       });
                               }
                             }
                             public void render ()
                             {
                               ois.update();
                             }
                           };
  new LwjglApplication(app);
}