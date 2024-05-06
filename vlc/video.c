#include <stdio.h>
#include <stdlib.h>
#include <vlc/vlc.h>

int main (int argc, char ** argv) {
  if (argc != 2) {
    printf("usage: %s <input_file>\n", argv[0]);
    return EXIT_FAILURE;
  }

  char * file_name = argv[1];

  libvlc_instance_t * inst = libvlc_new(0, NULL);
  libvlc_media_t * m = libvlc_media_new_path(inst, file_name);
  libvlc_media_player_t * mp = libvlc_media_player_new_from_media(m);

  libvlc_media_release(m);

  libvlc_media_player_play(mp);
  sleep(10);
  libvlc_media_player_stop(mp);

  libvlc_media_player_release(mp);
  libvlc_release(inst);

  return EXIT_SUCCESS;
}
