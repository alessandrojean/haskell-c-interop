#include <stdio.h>
#include <stdlib.h>

#include <libavformat/avformat.h>
#include <libavutil/dict.h>

int main (int argc, char** argv) {
  if (argc != 2) {
    printf("usage: %s <input_file>\n", argv[0]);
    return EXIT_FAILURE;
  }

  char * file_name = argv[1];
  AVFormatContext * format_context = NULL;
  AVDictionary * format_opts = NULL;

  av_dict_set(&format_opts, "scan_all_pmts", "1", AV_DICT_DONT_OVERWRITE);

  if (avformat_open_input(&format_context, file_name, NULL, &format_opts)) {
    return EXIT_FAILURE;
  }

  if (avformat_find_stream_info(format_context, NULL) < 0) {
    av_log(NULL, AV_LOG_ERROR, "Cannot find stream information\n");
    return EXIT_FAILURE;
  }

  const AVDictionaryEntry * tag = NULL;

  while ((tag = av_dict_get(format_context->metadata, "", tag, AV_DICT_IGNORE_SUFFIX))) {
    printf("%s=%s\n", tag->key, tag->value);
  }

  printf("format_name=%s\n", format_context->iformat->name);
  printf("format_long_name=%s\n", format_context->iformat->long_name);
  printf("format_extensions=%s\n", format_context->iformat->extensions);
  printf("format_mime_type=%s\n", format_context->iformat->mime_type);

  avformat_free_context(format_context);

  return EXIT_SUCCESS;
}
