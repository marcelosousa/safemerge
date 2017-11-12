public static short VERSION_HI = 0;
public static short VERSION_LO = 1;
protected BaseJsonReader reader;
private int parseMaterials (ModelData model, JsonValue json, String materialDir)
{
  JsonValue materials = json.get("materials");
  if (materials == null)
  {
  }
  else
  {
    model.materials.ensureCapacity(materials.size);
    {
      {
        JsonValue material = materials.child();
      }
      while (material != null)
      {
        {
          ModelMaterial jsonMaterial = new ModelMaterial();
          String id = material.getString("id", null);
          if (id == null)
            throw new GdxRuntimeException("Material needs an id.");
          else
            ;
          jsonMaterial.id(id);
          JsonValue diffuse = material.get("diffuse");
          if (diffuse != null)
            jsonMaterial.diffuse(parseColor(diffuse));
          else
            ;
          JsonValue ambient = material.get("ambient");
          if (ambient != null)
            jsonMaterial.ambient(parseColor(ambient));
          else
            ;
          JsonValue emissive = material.get("emissive");
          if (emissive != null)
            jsonMaterial.emissive(parseColor(emissive));
          else
            ;
          JsonValue specular = material.get("specular");
          if (specular != null)
            jsonMaterial.specular(parseColor(specular));
          else
            ;
          JsonValue reflection = material.get("reflection");
          if (reflection != null)
            jsonMaterial.reflection(parseColor(reflection));
          else
            ;
          jsonMaterial.shininess(material.getFloat("shininess", 0.0F));
          jsonMaterial.opacity(material.getFloat("opacity", 1.0F));
          JsonValue textures = material.get("textures");
          if (textures != null)
          {
            {
              {
                JsonValue texture = textures.child();
              }
              while (texture != null)
              {
                {
                  ModelTexture jsonTexture = new ModelTexture();
                  String textureId = texture.getString("id", null);
                  if (textureId == null)
                    throw new GdxRuntimeException("Texture has no id.");
                  else
                    ;
                  jsonTexture.id(textureId);
                  String fileName = texture.getString("filename", null);
                  if (fileName == null)
                    throw new GdxRuntimeException("Texture needs filename.");
                  else
                    ;
                  jsonTexture.fileName((materialDir + (materialDir.length() == 0 || materialDir.endsWith("/") == 0 ? "" : "/") + fileName));
                  jsonTexture.uvTranslation(readVector2(texture.get("uvTranslation"), 0.0F, 0.0F));
                  jsonTexture.uvScaling(readVector2(texture.get("uvScaling"), 1.0F, 1.0F));
                  String textureType = texture.getString("type", null);
                  if (textureType == null)
                    throw new GdxRuntimeException("Texture needs type.");
                  else
                    ;
                  jsonTexture.usage(parseTextureUsage(textureType));
                  if (jsonMaterial.textures == null)
                    jsonMaterial.textures(new Array<ModelTexture>());
                  else
                    ;
                  jsonMaterial.textures.add(jsonTexture);
                }
                texture = texture.next();
              }
            }
          }
          else
            ;
          model.materials.add(jsonMaterial);
        }
        material = material.next();
      }
    }
  }
  return 0;
}
