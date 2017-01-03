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
                               ois.getJoysticks().get(0).setCallback(new OisCallback()
                                                                     {
                                                                       @Override
                                                                       public void sliderMoved (OisJoystick joystick, int slider, boolean x, boolean y)
                                                                       {
                                                                         System.out.println(("slider " + slider + " moved, " + x + ", " + y));
                                                                       }
                                                                       @Override
                                                                       public void povMoved (OisJoystick joystick, int pov, OisPov direction)
                                                                       {
                                                                         System.out.println(("pov " + pov + " moved, " + direction));
                                                                       }
                                                                       @Override
                                                                       public void buttonReleased (OisJoystick joystick, int button)
                                                                       {
                                                                         System.out.println(("button " + button + " released"));
                                                                       }
                                                                       @Override
                                                                       public void buttonPressed (OisJoystick joystick, int button)
                                                                       {
                                                                         System.out.println(("button " + button + " pressed"));
                                                                       }
                                                                       @Override
                                                                       public void axisMoved (OisJoystick joystick, int axis, float value)
                                                                       {
                                                                         System.out.println(("axis " + axis + " moved, " + value));
                                                                       }
                                                                     });
                             }
                             public void render ()
                             {
                               ois.update();
                             }
                           };
  new LwjglApplication(app);
}