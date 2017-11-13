@Autowired
 private CacheProperties cacheProperties;
@Autowired
 CacheManagerCustomizerInvoker customizerInvoker;
@Autowired
 private CacheManagerCustomizers customizerInvoker;
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
  return this.customizerInvoker.customize(cacheManager);
}
