{
  JoglApplicationConfiguration config = new JoglApplicationConfiguration();
  config.title = "Hybrid Light";
  config.width = 800;
  config.height = 480;
  config.samples = 8;
  config.vSyncEnabled = true;
  config.useGL20 = true;
  new JoglApplication(new SkeletonModelGpuSkinningTest(), config);
}