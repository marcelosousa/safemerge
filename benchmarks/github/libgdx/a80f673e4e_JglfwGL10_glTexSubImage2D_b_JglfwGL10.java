{
  GL.glTexSubImage2D(target, level, xoffset, yoffset, width, height, format, type, pixels, Memory.getPosition(pixels));
}