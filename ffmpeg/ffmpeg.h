#ifndef FFMPEG_H
#define FFMPEG_H

#include <libavformat/avformat.h>
#include <libavutil/dict.h>

struct FfmpegInput {
  AVDictionary * metadata;
  AVDictionary * format_metadata;
  AVDictionary ** streams_metadata;
  int nb_streams;
  AVFormatContext * context;
};

typedef struct FfmpegInput FfmpegInput;

extern FfmpegInput * load_input(const char *);
extern void free_input(FfmpegInput *);

#endif // ffmpeg_h
