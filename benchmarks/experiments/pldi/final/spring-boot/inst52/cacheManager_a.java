@Bean
 public RedisCacheManager cacheManager (RedisTemplate<Object, Object> redisTemplate)
{
  RedisCacheManager cacheManager = new RedisCacheManager(redisTemplate);
  cacheManager.setUsePrefix(true);
  List<String> cacheNames = this.cacheProperties.getCacheNames();
  if (!cacheNames.isEmpty())
  {
    cacheManager.setCacheNames(cacheNames);
  }
  else
    ;
  this.customizerInvoker.customize(cacheManager);
  return cacheManager;
  return;
}