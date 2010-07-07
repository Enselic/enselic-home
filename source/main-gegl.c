/*
export PKG_CONFIG_PATH=/home/martin/dev/lib/pkgconfig
export LD_LIBRARY_PATH=/home/martin/dev/lib
gcc `pkg-config --cflags --libs gegl cairo` -g -o main-gegl main-gegl.c
*/

#include <gegl.h>
#include <cairo/cairo.h>

int main (int argc, char **argv)
{
  GeglNode *load;
  GeglRectangle size;
  cairo_surface_t *image;

  gegl_init (&argc, &argv);

  load = gegl_node_new ();
  gegl_node_set (load,
                 "operation", "gegl:load",
                 "path", "/tmp/test.png",
                 NULL);

  size = gegl_node_get_bounding_box (load);
  image = cairo_image_surface_create (CAIRO_FORMAT_RGB24,
                                      size.width,
                                      size.height);

  gegl_node_blit (load,
                  1.0 /*scale*/,
                  &size,
                  babl_format ("R'G'B'A u8"),
                  cairo_image_surface_get_data (image),
                  GEGL_AUTO_ROWSTRIDE,
                  GEGL_BLIT_DEFAULT);

  cairo_surface_write_to_png (image, "/tmp/output.png");

  return 0;
}
