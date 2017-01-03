{
  LwjglApplicationConfiguration config = new LwjglApplicationConfiguration();
  config.title = "Hybrid Light";
  config.width = 800;
  config.height = 480;
  config.samples = 8;
  config.vSyncEnabled = false;
  config.useGL20 = true;
  new LwjglApplication(new SkeletonModelGpuSkinningTest(), config);
}