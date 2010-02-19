/*
gcc `pkg-config --cflags --libs gtk+-2.0` -g -o main-gtk main-gtk.c
*/

#include <gtk/gtk.h>

static void main_gtk_delete_event (GtkWidget *widget,
                                   GdkEvent  *event,
                                   gpointer   user_data)
{
  gtk_main_quit ();
}

int main(int argc, char **argv)
{
  GtkWidget *window = NULL;
  GtkListStore *store = gtk_list_store_new (1,
                                            G_TYPE_STRING
  GtkWidget *widget = NULL;

  gtk_init (&argc, &argv);

  window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
  gtk_widget_show (window);
  g_signal_connect (window, "delete-event",
                    G_CALLBACK (main_gtk_delete_event),
                    NULL);

  widget = gtk_combo_box_new ();
  gtk_container_add (GTK_CONTAINER (window), widget);
  gtk_widget_show (widget);

  gtk_main ();

  return 0;
}
