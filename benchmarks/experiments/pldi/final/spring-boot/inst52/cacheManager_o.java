@Bean
 public RedisCacheManager cacheManager (RedisTemplate<Object, Object> redisTemplate)
{
  RedisCacheManager cacheManager = new RedisCacheManager(redisTemplate);
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